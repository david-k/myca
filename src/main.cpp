#include <cassert>
#include <iostream>
#include <fstream>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <sstream>

#include "codegen/c_backend.hpp"
#include "semantics/ast_traversal.hpp"
#include "semantics/module.hpp"
#include "semantics/passes.hpp"

using std::string;
using std::string_view;
using std::optional;

using namespace std::string_literals;
using namespace std::string_view_literals;

constexpr size_t MAX_MEMORY      = 50 * 1024*1024;
constexpr size_t MAX_TEMP_MEMORY = 50 * 1024*1024;

static optional<string_view> next_arg(char** &argv)
{
	return *argv ? optional(*argv++) : nullopt;
}

int main(int, char *argv[])
{
	optional<string> arg_input_filename;
	optional<string> arg_output_filename;
	optional<string> arg_event_log_filename;
	bool arg_print_types = false;
	bool arg_print_stats = false;

	next_arg(argv);
	while(optional<string_view> arg = next_arg(argv))
	{
		if(arg == "-o")
		{
			if(optional<string_view> filename = next_arg(argv))
				arg_output_filename = *filename;
			else
			{
				std::cerr << "Error: expected output filename" << std::endl;
				return 1;
			}
		}
		else if(arg == "--print-types")
		{
			arg_print_types = true;
		}
		else if(arg == "--print-stats")
		{
			arg_print_stats = true;
		}
		else if(arg == "--log-file")
		{
			if(optional<string_view> filename = next_arg(argv))
				arg_event_log_filename = *filename;
			else
			{
				std::cerr << "Error: expected event log filename" << std::endl;
				return 1;
			}
		}
		else
		{
			if(arg_input_filename) {
				std::cerr << "Error: input file already provided" << std::endl;
				return 1;
			}
			arg_input_filename = *arg;
		}
	}

	if(!arg_input_filename)
	{
		std::cerr << "Error: no input file provided" << std::endl;
		return 1;
	}

	if(!arg_output_filename)
	{
		std::cerr << "Error: no output file provided" << std::endl;
		return 1;
	}

	std::ifstream file(*arg_input_filename);
	if(!file) {
		std::cerr << "Error: cannot read file: " << *arg_input_filename << std::endl;
		return 1;
	}

	std::ostringstream ss;
	ss << file.rdbuf();
	std::string source = std::move(ss).str();

	std::unique_ptr<char[]> memory(new char[MAX_MEMORY]);
	std::unique_ptr<char[]> temp_memory(new char[MAX_TEMP_MEMORY]);
	Arena main_arena(memory.get(), MAX_MEMORY);
	Arena temp_arena(temp_memory.get(), MAX_TEMP_MEMORY);

	try
	{
		std::ofstream event_sink;
		optional<EventLogger> logger;
		Module mod = parse_module(source, Memory{&main_arena, temp_arena});

		if(arg_event_log_filename)
		{
			event_sink.open(*arg_event_log_filename);
			logger.emplace(&mod, event_sink);
		}

		sema(mod, main_arena, std::move(logger));
		if(arg_print_types)
			print(mod, std::cout);

		std::stringstream c_code;
		CBackend backend(c_code);
		generate_c(mod, backend);

		std::ofstream c_file(*arg_output_filename);
		c_file << c_code.str();

		if(arg_print_stats)
		{
			std::cout << "==================================================" << std::endl;
			std::cout << "Memory usage (arena): " << ((char*)main_arena.current_ptr() - memory.get()) << std::endl;
		}
	}
	catch(LexingError const &e)
	{
		std::cerr << e.what() << std::endl;
		return 1;
	}
	catch(ParseError const &e)
	{
		std::cerr << e.what() << std::endl;
		return 1;
	}
}
