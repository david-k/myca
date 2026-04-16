#include <filesystem>
#include <generator>
#include <iostream>
#include <string>

#include "syntax/parser.hpp"
#include "semantics/module.hpp"
#include "codegen/c_backend.hpp"
#include "utils.hpp"

using std::string;
using std::string_view;
using std::optional;
using std::nullopt;

namespace fs = std::filesystem;
namespace ranges = std::ranges;

static int run_cmd(string const &cmd)
{
	int status = std::system(cmd.c_str());
	if(status == -1)
		throw std::runtime_error("Creating child process failed");

	if(not WIFEXITED(status))
		throw std::runtime_error("Child proceses did not exit normally");

	return WEXITSTATUS(status);
}

constexpr size_t ARENA_SIZE = 50 * 1024*1024;
static std::pair<Arena, std::unique_ptr<char[]>> mk_arena()
{
	std::unique_ptr<char[]> memory(new char[ARENA_SIZE]);
	Arena arena(memory.get(), ARENA_SIZE);

	return {arena, std::move(memory)};
}

//--------------------------------------------------------------------
// Discovering tests
//--------------------------------------------------------------------
struct SeparateSpecTest
{
	string name;
	fs::path src_dir;
};

struct InlineSpecTest
{
	string name;
	string source;
};

struct Test : variant<SeparateSpecTest, InlineSpecTest>
{
	using variant::variant;
	string const& name() const
	{
		return std::visit([](auto const &t) -> string const& { return t.name; }, *this);
	}
};

static size_t find_test_case_separator(string_view text)
{
	for(string_view line: split_lines(text))
	{
		size_t separator_offset = line.find("//!---");
		if(separator_offset == string_view::npos) continue;
		if(not is_whitespace(line.substr(0, separator_offset))) continue;

		return (line.begin() - text.begin()) + separator_offset;
	}
	return string_view::npos;
}

static string_view next_test_case(string_view &source)
{
	size_t sep_begin_pos = find_test_case_separator(source);
	size_t sep_end_pos = source.find('\n', sep_begin_pos);
	string_view test_case = source.substr(0, sep_begin_pos);
	source.remove_prefix(std::min(source.length(), sep_end_pos));
	return test_case;
}

static std::generator<string_view> split_test_cases(string_view source)
{
	while(not source.empty())
	{
		string_view test_case = next_test_case(source);
		if(not is_whitespace(test_case))
			co_yield test_case;
	}
}

static optional<string_view> extract_test_case_name(string_view source)
{
	Parser parser(source);
	PrecedingText initial_text = get_preceding_text(TokenIdx(0), parser);
	vector<Comment> initial_comments = parse_comments(initial_text);
	OptionSet options;
	for(Comment const &c: initial_comments)
	{
		if(c.kind == CommentKind::SPECIAL and c.target == CommentTarget::NONE)
			parse_options_from_comment(c, options);
	}

	return options.try_get("test");
}

static vector<Test> discover_tests(fs::path const &src_dir)
{
	vector<Test> tests;
	for(fs::directory_entry const &entry: fs::directory_iterator(src_dir))
	{
		fs::path test_src_path = entry.path();
		if(fs::is_directory(test_src_path))
		{
			tests.push_back(SeparateSpecTest{
				.name = test_src_path.filename(),
				.src_dir = test_src_path,
			});
		}
		else if(test_src_path.extension() == ".myca")
		{
			string source = read_file(test_src_path);
			for(string_view case_source: split_test_cases(source))
			{
				optional<string_view> case_name = extract_test_case_name(case_source);
				if(not case_name)
					throw std::runtime_error("Test case in "s + test_src_path.filename() + " is missing the \"test\" option");

				tests.push_back(InlineSpecTest{
					.name = test_src_path.filename() + "." + *case_name,
					.source = string(case_source),
				});
			}
		}
	}

	ranges::sort(tests, {}, &Test::name);
	return tests;
}

//--------------------------------------------------------------------
// Compiling and running Myca code
//--------------------------------------------------------------------
enum class Outcome
{
	SUCCESS,
	FAILURE,
};

// The expected result of compiling the test
struct CompilationSpec
{
	Outcome expected_outcome;
	optional<string> expected_output;
};

// The actual result of compilation
struct CompilationResult
{
	std::unique_ptr<char[]> mem;
	Arena arena;
	Module mod;
	string source;
	ModuleOptions opts;

	Outcome outcome;
	string output;
};

// The expected result of executing the test
struct ExecutionSpec
{
	int expected_return_code;
};

// The actual result of executing the test
struct ExecutionResult
{
	bool gcc_failed;
	int test_return_code;
};

static CompilationResult compile_myca(string_view source, fs::path const &test_build_dir)
{
	auto [arena, mem] = mk_arena();
	auto [temp_arena, temp_mem] = mk_arena();
	Module mod = parse_module(source, Memory{&arena, temp_arena});
	ModuleOptions opts = gather_options(mod);

	try
	{
		sema(mod, arena, nullopt);

		std::stringstream c_code;
		CBackend backend(c_code);
		generate_c(mod, backend);
		std::ofstream c_file(test_build_dir / "main.c");
		c_file << c_code.str();

		std::stringstream ss;
		print(mod, ss);
		return CompilationResult(
			std::move(mem),
			std::move(arena),
			std::move(mod),
			string(source),
			std::move(opts),
			Outcome::SUCCESS, std::move(ss).str()
		);
	}
	catch(ParseError const &exc)
	{
		return CompilationResult(
			std::move(mem),
			std::move(arena),
			std::move(mod),
			string(source),
			std::move(opts),
			Outcome::FAILURE,
			exc.what()
		);
	}
}

static bool check_compilation(CompilationSpec spec, CompilationResult const &result)
{
	if(result.outcome == Outcome::SUCCESS)
	{
		if(spec.expected_outcome == Outcome::SUCCESS)
		{
			if(spec.expected_output and trimmed(*spec.expected_output) != trimmed(result.output))
			{
				std::cout << "ERROR" << std::endl;
				std::cout << "> Expected output:" << std::endl;
				std::cout << *spec.expected_output << std::endl;
				std::cout << "> Actual output:" << std::endl;
				std::cout << result.output << std::endl;
				return false;
			}
		}
		else
		{
			std::cout << "ERROR" << std::endl;
			std::cout << "> Compilation should have failed but succeeded" << std::endl;
			return false;
		}
	}
	else // result.outcome == Outcome::FAILURE
	{
		if(spec.expected_outcome == Outcome::FAILURE)
		{
			if(spec.expected_output and trimmed(*spec.expected_output) != trimmed(result.output))
			{
				std::cout << "ERROR" << std::endl;
				std::cout << "> Expected error: " << *spec.expected_output << std::endl;
				std::cout << "> Actual error:   " << result.output << std::endl;
				return false;
			}
		}
		else
		{
			std::cout << "ERROR" << std::endl;
			std::cout << "> Compilation failed with error:" << std::endl;
			std::cout << "> " << result.output << std::endl;
			return false;
		}
	}

	return true;
}

static ExecutionResult execute_myca(fs::path const &test_build_dir)
{
	std::stringstream gcc_cmd;
	gcc_cmd << "gcc -fsanitize=undefined,address ";
	gcc_cmd << std::quoted((test_build_dir / "main.c").string()) << " ";
	gcc_cmd << "-o " << std::quoted((test_build_dir / "main").string()) << " ";
	int gcc_result = run_cmd(gcc_cmd.str());
	if(gcc_result != 0)
	{
		return ExecutionResult{
			.gcc_failed = true,
			.test_return_code = 0,
		};
	}

	string main_cmd = test_build_dir / "main";
	int test_result = run_cmd(main_cmd);
	return ExecutionResult{
		.gcc_failed = false,
		.test_return_code = test_result,
	};
}

static bool check_execution(ExecutionSpec spec, ExecutionResult result)
{
	if(result.gcc_failed)
	{
		std::cout << "ERROR" << std::endl;
		std::cout << "> C compilation failed" << std::endl;
		return false;
	}

	if(result.test_return_code != spec.expected_return_code)
	{
		std::cout << "ERROR" << std::endl;
		std::cout << "> Expected return code: " << spec.expected_return_code << std::endl;
		std::cout << "> Actual return code: " << result.test_return_code << std::endl;
		return false;
	}

	return true;
}

//--------------------------------------------------------------------
// Running tests with separate spec files
//--------------------------------------------------------------------
constexpr string_view EXPECTED_PARSER_OUTPUT_FILENAME = "expected_parser_output.txt";
constexpr string_view EXPECTED_COMPILATION_ERROR_FILENAME = "expected_compilation_error.txt";
constexpr string_view EXPECTED_RETURN_CODE_FILENAME = "expected_return_code.txt";

static optional<CompilationSpec> read_compilation_spec(fs::path const &test_src_dir)
{
	optional<string> successful_output;
	if(fs::exists(test_src_dir / EXPECTED_PARSER_OUTPUT_FILENAME))
		successful_output = read_file(test_src_dir / EXPECTED_PARSER_OUTPUT_FILENAME);

	optional<string> error_output;
	if(fs::exists(test_src_dir / EXPECTED_COMPILATION_ERROR_FILENAME))
		error_output = read_file(test_src_dir / EXPECTED_COMPILATION_ERROR_FILENAME);

	if(successful_output and error_output)
		throw std::runtime_error("Both success and error output specified in test " + test_src_dir.string());

	if(successful_output)
		return CompilationSpec(Outcome::SUCCESS, std::move(*successful_output));

	if(error_output)
		return CompilationSpec(Outcome::FAILURE, std::move(*error_output));

	return nullopt;
}

static optional<ExecutionSpec> read_execution_spec(fs::path const &test_src_dir)
{
	if(fs::exists(test_src_dir / EXPECTED_RETURN_CODE_FILENAME))
	{
		string return_code_string = read_file(test_src_dir / EXPECTED_RETURN_CODE_FILENAME);
		int return_code = parse_int<int>(trimmed(return_code_string));
		return ExecutionSpec(return_code);
	}

	return nullopt;
}

static bool validate_files_in_test_dir(fs::path const &test_src_dir)
{
	string_view spec_files[] = {
		EXPECTED_PARSER_OUTPUT_FILENAME,
		EXPECTED_COMPILATION_ERROR_FILENAME,
		EXPECTED_RETURN_CODE_FILENAME,
		"main.myca",
	};

	bool has_main = false;
	bool has_spec_files = false;
	for(fs::directory_entry const &entry: fs::directory_iterator(test_src_dir))
	{
		string filename = entry.path().filename();
		if(filename == "main.myca")
			has_main = true;
		else if(std::ranges::contains(spec_files, filename))
			has_spec_files = true;
		else
		{
			throw std::runtime_error(
				"Test " + test_src_dir.string() + " contains invalid file " + filename
			);
		}
	}

	if(not has_main)
		throw std::runtime_error("Test " + test_src_dir.string() + " is missing a main.myca file");

	return has_spec_files;
}

static bool run_test_with_spec_files(
	CompilationResult const &compilation_result,
	fs::path const &test_src_dir,
	fs::path const &test_build_dir
)
{
	bool has_spec_files = validate_files_in_test_dir(test_src_dir);
	if(not has_spec_files)
		throw std::runtime_error("Test case "s + test_src_dir.filename() + " is missing spec files");

	optional<CompilationSpec> compilation_spec = read_compilation_spec(test_src_dir);
	optional<ExecutionSpec> execution_spec = read_execution_spec(test_src_dir);

	if(not compilation_spec and not execution_spec)
		throw std::runtime_error("No test spec for " + test_src_dir.filename());

	if(execution_spec and not compilation_spec)
		compilation_spec = CompilationSpec{.expected_outcome = Outcome::SUCCESS, .expected_output = nullopt};

	bool ok = check_compilation(*compilation_spec, compilation_result);
	if(not ok) return false;

	if(execution_spec)
	{
		ExecutionResult execution_result = execute_myca(test_build_dir);
		bool ok = check_execution(*execution_spec, execution_result);
		if(not ok) return false;
	}

	return true;
}

//--------------------------------------------------------------------
// Running tests with inline specs
//--------------------------------------------------------------------
static string set_line_number_in_msg(string_view error_msg, int line_number)
{
	string result(trimmed(error_msg));
	// Replace the underscore in "|_:COLUMN| error: ..." with `line_number`
	if(error_msg.starts_with("|_"))
		result.replace(1, 1, std::to_string(line_number));

	return result;
}

static void ensure_valid_keys(
	OptionSet const &opts,
	std::span<string_view> valid_keys,
	string_view error_context
)
{
	for(string_view key: ranges::views::keys(opts.opts))
	{
		if(not ranges::contains(valid_keys, key))
			throw std::runtime_error("Invalid "s + error_context + " option: "s + key);
	}
}

// Returns the expected error message, if specified
static optional<string> validate_inline_spec(ModuleOptions const &opts, Module const &mod)
{
	bool has_expect = false;
	bool has_return_code = false;
	optional<string> expected_error;
	if(OptionSet const *options = opts.try_get(TopLevelTarget()))
	{
		string_view valid_options[] = {"test", "return_code", "error"};
		ensure_valid_keys(*options, valid_options, "top-level");

		if(not options->try_get("test"))
			throw std::runtime_error("Top-level option \"test\" is missing");

		if(optional<string_view> error = options->try_get("error"))
			expected_error = string(*error);

		if(options->try_get("return_code"))
			has_return_code = true;
	}

	auto process_local_error_spec = [&](OptionSet const &options, TokenIdx item_token)
	{
		if(optional<string_view> error = options.try_get("error"))
		{
			if(expected_error)
				throw std::runtime_error("Only one \"error\" clause can be specified per test case");

			int line = mod.parser.token_at(item_token).span.begin.line;
			expected_error = set_line_number_in_msg(*error, line);
		}
	};

	for(auto const &[target, options]: opts.opts)
	{
		target | match
		{
			[&](TopLevelTarget) {},
			[&](StructItem const *item)
			{
				string_view valid_options[] = {"error"};
				ensure_valid_keys(options, valid_options, "struct");
				process_local_error_spec(options, item->range.first);
			},
			[&](pair<StructItem const*, VarMember const*> m)
			{
				string_view valid_options[] = {"error"};
				ensure_valid_keys(options, valid_options, "struct member");
				process_local_error_spec(options, m.second->var.range.first);
			},
			[&](ProcItem const *item)
			{
				string_view valid_options[] = {"error"};
				ensure_valid_keys(options, valid_options, "proc");
				process_local_error_spec(options, item->range.first);
			},
			[&](AliasItem const *item)
			{
				string_view valid_options[] = {"error"};
				ensure_valid_keys(options, valid_options, "alias");
				process_local_error_spec(options, item->range.first);
			},
			[&](Stmt const *stmt)
			{
				string_view valid_options[] = {"expect", "error"};
				ensure_valid_keys(options, valid_options, "statement");
				if(options.try_get("expect"))
					has_expect = true;

				process_local_error_spec(options, stmt->token_range().first);
			},
		};
	}

	if(not expected_error and not has_return_code and not has_expect)
		throw std::runtime_error("Test case does not specify any test condition");

	if(has_expect and expected_error)
		throw std::runtime_error("Statement-level \"expect\" cannot be used together with \"error\"");

	if(expected_error and has_return_code)
		throw std::runtime_error("Cannot use options \"error\" and \"return_code\" together");

	return expected_error;
}

static bool run_test_with_inline_spec(
	CompilationResult const &compilation_result,
	fs::path const &test_build_dir
)
{
	ModuleOptions const &opts = compilation_result.opts;
	optional<string> expected_error = validate_inline_spec(opts, compilation_result.mod);
	if(compilation_result.outcome == Outcome::SUCCESS)
	{
		if(expected_error)
		{
			std::cout << "ERROR" << std::endl;
			std::cout << "> Compilation should have failed but succeeded" << std::endl;
			return false;
		}

		ExecutionSpec execution_spec{.expected_return_code = 0};
		if(optional<string_view> return_code_str = opts.try_get(TopLevelTarget(), "return_code"))
			execution_spec.expected_return_code = parse_int<int>(*return_code_str);

		for(auto const &[target, options]: opts.opts)
		{
			bool ok = target | match
			{
				[&](Stmt const *stmt)
				{
					optional<string_view> expected = options.try_get("expect");
					if(not expected) return true;

					expected = trimmed(*expected);
					string actual = str(*stmt, compilation_result.mod);
					if(actual != expected)
					{
						std::cout << "ERROR" << std::endl;
						std::cout << "> Expected: " << *expected << std::endl;
						std::cout << "> Actual: " << actual << std::endl;
						return false;
					}

					return true;
				},
				[&](StructItem const*) { return true; },
				[&](pair<StructItem const*, VarMember const*>) { return true; },
				[&](ProcItem const*) { return true; },
				[&](AliasItem const*) { return true; },
				[&](TopLevelTarget) { return true; },
			};

			if(not ok) return false;
		}

		ExecutionResult execution_result = execute_myca(test_build_dir);
		bool ok = check_execution(execution_spec, execution_result);
		if(not ok) return false;
	}
	else // Outcome::FAILURE
	{
		if(not expected_error)
		{
			std::cout << "ERROR" << std::endl;
			std::cout << "> Compilation failed with error:" << std::endl;
			std::cout << "> " << compilation_result.output << std::endl;
			return false;
		}
		if(compilation_result.output != *expected_error)
		{
			std::cout << "ERROR" << std::endl;
			std::cout << "> Expected error: " << *expected_error << std::endl;
			std::cout << "> Actual error:   " << compilation_result.output << std::endl;
			return false;
		}
	}

	return true;
}

//--------------------------------------------------------------------
// Putting it all together
//--------------------------------------------------------------------
static bool run_test(Test const &test, fs::path const &root_build_dir)
{
	std::cout << "Testing " << std::quoted(test.name()) << "... " << std::flush;
	fs::path test_build_dir = root_build_dir / test.name();
	fs::create_directories(test_build_dir);

	bool ok = test | match
	{
		[&](SeparateSpecTest const &t)
		{
			string source = read_file(t.src_dir / "main.myca");
			CompilationResult compilation_result = compile_myca(source, test_build_dir);
			if(not compilation_result.opts.empty())
				throw std::runtime_error("Test " + t.name + " uses both spec files and comment options");

			return run_test_with_spec_files(compilation_result, t.src_dir, test_build_dir);
		},
		[&](InlineSpecTest const &t)
		{
			CompilationResult compilation_result = compile_myca(t.source, test_build_dir);
			if(compilation_result.opts.empty())
				throw std::runtime_error("Test " + t.name + " does not specify any test conditions");

			return run_test_with_inline_spec(compilation_result, test_build_dir);
		},
	};

	if(ok) std::cout << "ok" << std::endl;
	return ok;
}

static string_view next_arg(char** &argv)
{
	if(not *argv)
		throw std::runtime_error("Expected more command-line args");

	return string_view(*argv++);
}

static std::unordered_set<string_view> const TESTS_TO_IGNORE = {
    "struct_recursive_def",
};

int main(int argc, char *argv[])
{
	if(argc != 3)
	{
		std::cerr << "USAGE: " << *argv << " <PROJECT_DIR> <BUILD_DIR>" << std::endl;
		return 1;
	}

	next_arg(argv);
	fs::path project_dir = next_arg(argv);
	fs::path build_dir = next_arg(argv);
	fs::path root_test_dir = project_dir / "tests/integration";
	fs::path root_test_build_dir = build_dir / "tests/integration";
	vector<Test> tests = discover_tests(root_test_dir);

	int num_tests_passed = 0;
	int num_tests_failed = 0;
	int num_tests_ignored = 0;
	for(Test const &test: tests)
	{
		if(TESTS_TO_IGNORE.contains(test.name()))
		{
			num_tests_ignored += 1;
			continue;
		}

		if(run_test(test, root_test_build_dir))
			num_tests_passed += 1;
		else
			num_tests_failed += 1;
	}

	std::cout << "------------------------------" << std::endl;
	std::cout << num_tests_passed << " / " << (num_tests_passed + num_tests_failed) << " tests passed";
	if(num_tests_ignored)
		std::cout << " (" << num_tests_ignored << " ignored)";
	std::cout << "\n" << std::endl;
}
