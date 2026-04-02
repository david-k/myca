#include <algorithm>
#include <initializer_list>
#include <iostream>
#include <memory>
#include <random>
#include <stdexcept>

#include "semantics/module.hpp"
#include "semantics/context.hpp"
#include "semantics/constraint_solver.hpp"
#include "semantics/type_env.hpp"

//==============================================================================
// Utils
//==============================================================================
class TestError : public std::runtime_error
{
public:
	TestError() :
		std::runtime_error("") {}

	using std::runtime_error::runtime_error;
};


constexpr size_t ARENA_SIZE = 50 * 1024*1024;

static std::pair<Arena, std::unique_ptr<char[]>> mk_arena()
{
	std::unique_ptr<char[]> memory(new char[ARENA_SIZE]);
	Arena arena(memory.get(), ARENA_SIZE);

	return {arena, std::move(memory)};
}

static std::unique_ptr<Module> mk_empty_module(Arena &arena)
{
	auto mod = std::make_unique<Module>(Module{
		.parser = {},
		.items = ListBuilder<TopLevelItem>(arena),
	});

	std::unique_ptr<Scope> global_scope = std::make_unique<Scope>(mod.get(), true);
	mod->sema = std::make_unique<SemaModule>(std::move(global_scope), arena);

	return mod;
}

// Replace Paths beginning with "_" with TypeDeductionVars
static void insert_type_deduction_vars(
	GenericArg &arg,
	UnorderedStringMap<GenericDeductionVar> &vars,
	SemaContext &ctx
);

static void insert_type_deduction_vars(
	Type &type,
	UnorderedStringMap<GenericDeductionVar> &vars,
	SemaContext &ctx
)
{
	type | match
	{
		[&](BuiltinType&) {},
		[&](KnownIntType&) {},
		[&](PointerType &t)
		{
			insert_type_deduction_vars(*t.pointee, vars, ctx);
		},
		[&](ArrayType&)
		{
			assert(!"[TODO] insert_type_deduction_vars: ArrayType");
		},
		[&](ProcType&) { UNREACHABLE; },
		[&](ProcTypeUnresolved&)
		{
			assert(!"[TODO] insert_type_deduction_vars: ProcTypeUnresolved");
		},
		[&](StructType&) { UNREACHABLE; },
		[&](UnionTypeUnresolved&)
		{
			assert(!"[TODO] insert_type_deduction_vars: UnionTypeUnresolved");
		},
		[&](UnionType&) { UNREACHABLE; },
		[&](Path &path)
		{
			string_view name = ctx.mod->name_of(path);
			if(name.starts_with("_"))
			{
				auto it = vars.find(name);
				if (it == vars.end())
				{
					GenericDeductionVar var = ctx.new_deduction_var(TypeDeductionVar());
					vars[string(name)] = var;
					type = VarType(UNKNOWN_TOKEN_RANGE, var);
				}
				else
					type = VarType(UNKNOWN_TOKEN_RANGE, it->second);
			}
			else
			{
				for(GenericArg &arg: *path.type_args)
					insert_type_deduction_vars(arg, vars, ctx);
			}
		},
		[&](VarType&) { UNREACHABLE; },
		[&](InlineStructType&)
		{
			assert(!"[TODO] insert_type_deduction_vars: InlineStructType");
		},
	};
}

static void insert_type_deduction_vars(
	GenericArg &arg,
	UnorderedStringMap<GenericDeductionVar> &vars,
	SemaContext &ctx
)
{
	arg | match
	{
		[&](Type &type) { insert_type_deduction_vars(type, vars, ctx); },
		[&](Expr &expr)
		{
			if(Path const *path = std::get_if<Path>(&expr))
			{
				string_view name = ctx.mod->name_of(*path);
				if(name.starts_with("_"))
				{
					auto it = vars.find(name);
					if(it == vars.end())
					{
						GenericDeductionVar var = ctx.new_deduction_var(ValueDeductionVar());
						vars[string(name)] = var;
						expr = GenericVarExpr(UNKNOWN_TOKEN_RANGE, var);
					}
					else
						expr = GenericVarExpr(UNKNOWN_TOKEN_RANGE, it->second);
				}
			}
		},
	};
}

static Type parse_incremental_type(
	string_view source,
	UnorderedStringMap<GenericDeductionVar> &vars,
	SemaContext &ctx,
	Memory M,
	size_t *bytes_consumed = nullptr
)
{
	Parser &parser = ctx.mod->parser;
	parser.append_source(source);
	size_t initial_byte_pos = parser.bytes_consumed();
	Type type = parse_type(ctx.mod->parser, M);

	if (bytes_consumed)
		*bytes_consumed = parser.bytes_consumed() - initial_byte_pos;

	insert_type_deduction_vars(type, vars, ctx);
	resolve_type(type, ctx.mod->sema->globals.get(), ResolutionContext::GENERAL, ctx);

	return type;
}

static void parse_incremental_top_level_item(
	string_view source,
	SemaContext &ctx,
	Memory M
)
{
	ctx.mod->parser.append_source(source);
	TopLevelItem *item = M.main->alloc<TopLevelItem>(parse_top_level_item(ctx.mod->parser, M));
	register_item(*item, ctx);
	resolve_item(*item, ctx);
}

struct ConstraintSpec
{
	Type *left;
	TypeConversion left_conv;
	Type *right;
	TypeConversion right_conv;
	ConstraintModifier right_modifier;
};

static ConstraintSpec parse_constraint_spec(
	string_view line,
	UnorderedStringMap<GenericDeductionVar> &vars,
	SemaContext &ctx,
	Memory M
)
{
	Lexer lexer(line);
	size_t bytes_consumed = 0;

	// Parse left-hand side
	Type type_left = parse_incremental_type(line, vars, ctx, M, &bytes_consumed);
	lexer.advance(bytes_consumed);

	// Parse edge
	skip_whitespace(lexer);
	TypeConversion left_conv;
	TypeConversion right_conv;
	if (try_consume(lexer, "--")) {
		left_conv = TypeConversion::NONE;
		right_conv = TypeConversion::NONE;
	}
	else if (try_consume(lexer, "->>")) {
		left_conv = TypeConversion::IMPLICIT_CTOR;
		right_conv = TypeConversion::NONE;
	}
	else if (try_consume(lexer, "->")) {
		left_conv = TypeConversion::TRIVIAL;
		right_conv = TypeConversion::NONE;
	}
	else if (try_consume(lexer, "<-")) {
		left_conv = TypeConversion::NONE;
		right_conv = TypeConversion::TRIVIAL;
	}
	else if (try_consume(lexer, "<<-")) {
		left_conv = TypeConversion::NONE;
		right_conv = TypeConversion::IMPLICIT_CTOR;
	}
	else if (try_consume(lexer, ">-<")) {
		left_conv = TypeConversion::TRIVIAL;
		right_conv = TypeConversion::TRIVIAL;
	}
	else if (try_consume(lexer, ">>-<<")) {
		left_conv = TypeConversion::IMPLICIT_CTOR;
		right_conv = TypeConversion::IMPLICIT_CTOR;
	}
	else
		throw std::runtime_error("Invalid constraint relation: "s + lexer.remaining_string());

	// Parse right-hand side
	Type type_right;
	skip_whitespace(lexer);
	if(try_consume(lexer, "$KnownInt(")) {
		optional<string_view> number_string = try_read_number(lexer);
		if(not number_string)
			throw std::runtime_error("Parsing ConstraintSpec failed: expected number");
		consume(lexer, ")");
		uint64_t n = parse_integer(*number_string);
		type_right = KnownIntType(n, n);
	}
	else
	{
		type_right = parse_incremental_type(lexer.remaining_string(), vars, ctx, M, &bytes_consumed);
		lexer.advance(bytes_consumed);
	}

	// Parse modifier
	skip_whitespace(lexer);
	ConstraintModifier modifier_right = NoModifier();
	if(try_consume(lexer, "/")) {
		skip_whitespace(lexer);
		optional<string_view> modifier_name = try_read_identifier(lexer);
		if(modifier_name == "member") {
			skip_whitespace(lexer);
			consume(lexer, ":");
			skip_whitespace(lexer);
			optional<string_view> member_name = try_read_identifier(lexer);
			if(not member_name)
				throw std::runtime_error("Parsing ConstraintSpec failed: expected field name");

			modifier_right = MemberTypeModifier(*member_name);
		}
		else
			throw std::runtime_error("Parsing ConstraintSpec failed: invalid modifier name");
	}

	return ConstraintSpec(
		M.main->alloc<Type>(type_left), left_conv,
		M.main->alloc<Type>(type_right), right_conv, modifier_right
	);
}

static void add_constraints(
	ConstraintSolver &constraints,
	UnorderedStringMap<GenericDeductionVar> &vars,
	SemaContext &ctx,
	Memory M,
	vector<string_view> specs
)
{
	for(string_view spec_str: specs)
	{
		ConstraintSpec spec = parse_constraint_spec(spec_str, vars, ctx, M);
		constraints.add_relational_constraint(
			spec.left->try_get_deduction_var().value(),
			ConstraintEdge{
				.var_conv = spec.left_conv,
				.var_expr = nullptr,
				.arg = *spec.right,
				.arg_conv = spec.right_conv,
				.arg_expr = nullptr,
				.arg_modifier = spec.right_modifier,
				.error_msg = nullopt,
			}
		);
	}
}

struct VarSubst
{
	string_view var_string;
	string_view type_string;
};

TypeEnv parse_type_env(
	std::initializer_list<VarSubst> expected_subst_strings,
	UnorderedStringMap<GenericDeductionVar> vars,
	SemaContext &ctx,
	Memory M
)
{
	TypeEnv env;
	for(VarSubst const &s: expected_subst_strings)
	{
		auto it = vars.find(s.var_string);
		assert(it != vars.end() && "The expected substitution has an invalid TypeDeductionVar");
		GenericDeductionVar var = it->second;
		Type type = parse_incremental_type(s.type_string, vars, ctx, M);

		env.add(var, type);
	}

	return env;
}

struct ExpectedSubst
{
	ExpectedSubst(std::initializer_list<VarSubst> subst_strings) :
		subst_strings(subst_strings) {}

	std::initializer_list<VarSubst> subst_strings;
};

struct ExpectedError
{
	string_view error;
};

using ExpectedOutcome = variant<ExpectedSubst, ExpectedError>;

void test_constraints_impl(
	vector<string_view> const &constraint_strings,
	ExpectedOutcome expected_outcome
)
{
	auto [allocator, mem] = mk_arena();
	auto [temp_allocator, temp_mem] = mk_arena();
	Memory M(&allocator, temp_allocator);
	std::unique_ptr<Module> mod = mk_empty_module(*M.main);
	SemaContext ctx(*mod, *M.main);

	parse_incremental_top_level_item("struct Option'T { case implicit Some{value: T}, case None }", ctx, M);
	parse_incremental_top_level_item("struct Pair'(S,T) { first: S, second: T }", ctx, M);
	parse_incremental_top_level_item("typealias AorB = struct A | struct B;", ctx, M);

	// Parse constraints and the expected TypeEnv
	UnorderedStringMap<GenericDeductionVar> vars;
	ConstraintSolver constraints(ctx);
	add_constraints(constraints, vars, ctx, M, constraint_strings);
	ConstraintSolver original_constraints = constraints;

	expected_outcome | match
	{
		[&](ExpectedSubst expected)
		{
			TypeEnv expected_env = parse_type_env(expected.subst_strings, vars, ctx, M);

			TypeEnv actual_env;
			actual_env = constraints.solve();

			string error;
			if(actual_env.mapping().size() != expected_env.mapping().size())
				error = "Actual TypeEnv does not match expected TypeEnv";

			for(auto const &[var, expected_type]: expected_env.mapping())
			{
				GenericArg const *actual_type = actual_env.try_lookup(var);
				if(not actual_type or not equiv(expected_type, *actual_type))
				{
					error = "Actual TypeEnv does not match expected TypeEnv";
					break;
				}
			}

			if(not error.empty())
			{
				std::cout << "\nInitial constraints:" << std::endl;
				original_constraints.print(std::cout);
				std::cout << "Final constraints:" << std::endl;
				constraints.print(std::cout);
				std::cout << "Expected TypeEnv:" << std::endl;
				expected_env.print(std::cout, *mod);
				std::cout << "Actual TypeEnv:" << std::endl;
				actual_env.print(std::cout, *mod);

				throw TestError(error);
			}
		},
		[&](ExpectedError expected)
		{
			try
			{
				TypeEnv actual_env;
				actual_env = constraints.solve();

				std::cout << "\nInitial constraints:" << std::endl;
				original_constraints.print(std::cout);
				std::cout << "Final constraints:" << std::endl;
				constraints.print(std::cout);
				std::cout << "Expected error: " << expected.error << std::endl;
				std::cout << "Actual TypeEnv:" << std::endl;
				actual_env.print(std::cout, *mod);

				throw TestError("Expected error, got success");
			}
			catch(ParseError const &actual_error)
			{
				if(not string_view(actual_error.what()).ends_with(expected.error))
				{
					std::cout << "\nInitial constraints:" << std::endl;
					original_constraints.print(std::cout);
					std::cout << "Final constraints:" << std::endl;
					constraints.print(std::cout);
					std::cout << "Expected error: " << expected.error << std::endl;
					std::cout << "Actual error: " << actual_error.what() << std::endl;

					throw TestError("Expected different error");
				}
			}
		},
	};
}

size_t RANDOM_SEED = 0;

void test_constraints(
	vector<string_view> constraint_strings,
	ExpectedOutcome expected_outcome
)
{
	std::mt19937 mt(RANDOM_SEED);
	for(int i = 0; i < 10; ++i)
	{
		test_constraints_impl(constraint_strings, expected_outcome);
		std::ranges::shuffle(constraint_strings, mt);
	}
}


//==============================================================================
// Test cases
//==============================================================================
std::pair<char const*, void(*)()> TESTS[] = {

	{"constraints.basic", []()
	{
		test_constraints(
			{
				"_a <<- i8"
			},
			ExpectedSubst{
				{"_a", "i8"}
			}
		);

		test_constraints(
			{
				"_a <<- i8",
				"_a <<- i32",
			},
			ExpectedSubst{
				{"_a", "i32"}
			}
		);

		test_constraints(
			{
				"_a <- i8",
				"_a <<- i32",
			},
			ExpectedError{"Incompatible types i8 and i32"}
		);

		// The final substitution should be [_a ↦ i8] because assignments (PUSH) weighs higher
		// than PULL.
		test_constraints(
			{
				"_a ->> i32",
				"_a <<- i8",
			},
			ExpectedSubst{
				{"_a", "i8"},
			}
		);

		test_constraints(
			{
				"_a ->> i32",
				"_a <<- i8",
				"_a <<- _b",
			},
			ExpectedSubst{
				{"_a", "i8"},
				{"_b", "i8"},
			}
		);

		// If _a is processed before _b, then _a receives no type from its push edges. Thus, its
		// pull edges are merged together, and since we don't support intersection types, for
		// simplicity, we require them to be equal. The result is then used as the type for _a.
		// However, in this case this fails as i8 and i32 are not equal. However, once _b has been
		// processed, constraint resolution should succeed.
		test_constraints(
			{
				"_a ->> i32",
				"_a ->> i8",
				"_a <<- _b",
				"_b <<- i8"
			},
			ExpectedSubst{
				{"_a", "i8"},
				{"_b", "i8"},
			}
		);

		// Reducing _a will unify _b and i32
		// 1. If _b <<- i8 has already been processed, then the type of _b will remain i8 and a
		//    conversion to i32 is inserted
		// 2. If _b <<- i8 has *not* already been processed, then the type of _b will be i32
		// The first option is what should happen
		test_constraints(
			{
				"_b <<- i8",
				"_a <<- _b",
				"_a <<- i32",
			},
			ExpectedSubst{
				{"_a", "i32"},
				{"_b", "i8"},
			}
		);

		test_constraints(
			{
				"_c -- isize",
				"_c <<- _a",
				"_a ->> i32",
				"_a <<- _b",
				"_a <<- i32",
				"_b <<- i8",
				"_b ->> i32",
			},
			ExpectedSubst{
				{"_a", "i32"},
				{"_b", "i8"},
				{"_c", "isize"},
			}
		);

		// Check that unification can handle the case where the right side contains the variable
		// from the left (classical first-order unification does not allow this)
		test_constraints(
			{
				"_a ->> Option'(_a)",
				"_a <<- i32",
			},
			ExpectedSubst{
				{"_a", "i32"},
			}
		);

		// The constraint `_a <<- _b` is satisfiable by using the implicit Option.Some constructor
		test_constraints(
			{
				"_a <<- _b",
				"_a <<- Option'(_b)",
				"_b <<- i8",
			},
			ExpectedSubst{
				{"_a", "Option'i8"},
				{"_b", "i8"},
			}
		);

		// The constraint `_b -- i32` is not stated explicitly but will be automatically generated
		// when unifying `Option'(i32)` and `Option'(_b)`
		test_constraints(
			{
				"_a <<- Option'(i32)",
				"_a <<- Option'(_b)",
				"_b <<- _c",
				"_c <<- i8",
			},
			ExpectedSubst{
				{"_a", "Option'(i32)"},
				{"_b", "i32"},
				{"_c", "i8"},
			}
		);

		test_constraints(
			{
				"_a <<- Option'(_c)",
				"_a <<- _b",
				"_b <<- Option'(i32)",
			},
			ExpectedSubst{
				{"_a", "Option'(i32)"},
				{"_b", "Option'(i32)"},
				{"_c", "i32"},
			}
		);

		test_constraints(
			{
				"_a ->> Option'(_a)",
			},
			// Would be nice if we could directly refer to _a instead of ?_0
			ExpectedError{"Insufficiently constrained type Option'(?_0)"}
		);

		test_constraints(
			{
				"_x -- Pair'(_a,i32)",
				"_y -- Pair'(_b,_c)",
				"_z -- Pair'(bool,_d)",

				"_x <<- _y",
				"_y <<- _z",
			},
			ExpectedSubst{
				{"_a", "bool"},
				{"_b", "bool"},
				{"_c", "i32"},
				{"_d", "i32"},
				{"_x", "Pair'(bool,i32)"},
				{"_y", "Pair'(bool,i32)"},
				{"_z", "Pair'(bool,i32)"},
			}
		);

		// The type for a type deduction variable is usually solely determined by its push
		// constraints. However, if there are no push constraints, then we look at its pull
		// constraints.
		test_constraints(
			{
				"_a ->> i8",
			},
			ExpectedSubst{
				{"_a", "i8"},
			}
		);

		// We *could* infer a common integer type in many cases. However, I think it's too confusing
		// to come up with an integer type that is not even mentioned in the code.
		test_constraints(
			{
				"_a <<- u8",
				"_a <<- i8",
			},
			ExpectedError{"Incompatible types i8 and u8"}
		);

		test_constraints(
			{
				"_a ->> i8",
				"_b ->> _a",
			},
			ExpectedSubst{
				{"_a", "i8"},
				{"_b", "i8"},
			}
		);

		// Only pull constraints
		test_constraints(
			{
				"_a ->> _b",
				"_a ->> _c",
				"_b ->> _c",
				"_c ->> i32",
			},
			ExpectedSubst{
				{"_a", "i32"},
				{"_b", "i32"},
				{"_c", "i32"},
			}
		);

		// If a type deduction variable is assigned to different types without being assigned a type
		// itself we get an error. I believe coming up with a fitting type could be confusing in
		// many cases (though I haven't even come across a scenario like this in practice)
		test_constraints(
			{
				"_a ->> i8",
				"_a ->> i32",
			},
		ExpectedError{"Incompatible types i8 and i32"}
		);
	}},
	{"constraints.modifiers", []()
	{
		test_constraints(
			{
				"_a <<- Pair'(bool,i32) /member: first",
			},
			ExpectedSubst{
				{"_a", "bool"},
			}
		);

		test_constraints(
			{
				"_a <<- Pair'(_x,_y) /member: first",
				"_b <<- Pair'(_x,i32)",
				"_b <<- _c",
				"_c <<- Pair'(bool,_y)",
			},
			ExpectedSubst{
				{"_a", "bool"},
				{"_b", "Pair'(bool,i32)"},
				{"_c", "Pair'(bool,i32)"},
				{"_x", "bool"},
				{"_y", "i32"},
			}
		);

		test_constraints(
			{
				"_a <<- _b /member: first",
				"_b <<- Pair'(_x,i32)",
				"_b <<- Pair'(bool,_y)",
			},
			ExpectedSubst{
				{"_a", "bool"},
				{"_b", "Pair'(bool,i32)"},
				{"_x", "bool"},
				{"_y", "i32"},
			}
		);

		// If _a is processed before _x, then `_a <<- _x /member: first` won't have any effect and
		// _a is assigned `Option'(bool)`. If `_z <<- _a /member: value` is processed next,
		// accessing the member `value` on _a fails because `Option'(bool)` has no such member.
		// However, this must not be a hard error, because eventually _a will be assigned
		// `Option'(bool).Some` at which point the member access succeeds.
		test_constraints(
			{
				"_z <<- _a /member: value",
				"_a ->> Option'(bool)",
				"_a <<- _x /member: first",
				"_x <<- Pair'(Option'(_b).Some,i32)",
			},
			ExpectedSubst{
				{"_z", "bool"},
				{"_a", "Option'(bool).Some"},
				{"_b", "bool"},
				{"_x", "Pair'(Option'(bool).Some,i32)"},
			}
		);

		// Making the previous test pass should not mean that we don't produce a proper error
		// message in case we do access an invalid member.
		test_constraints(
			{
				"_z <<- _a /member: valueeeeee",
				"_a ->> Option'(bool)",
				"_a <<- _x /member: first",
				"_x <<- Pair'(Option'(_b).Some,i32)",
			},
			ExpectedError("`Some` has no field named `valueeeeee`")
		);
	}},
	{"constraints.known_ints", []()
	{
		test_constraints(
			{
				"_a <<- $KnownInt(23)",
			},
			ExpectedSubst{
				{"_a", "i32"},
			}
		);

		test_constraints(
			{
				"_a <<- $KnownInt(2147483648)",
			},
			ExpectedSubst{
				{"_a", "u32"},
			}
		);

		test_constraints(
			{
				"_a <<- $KnownInt(23)",
				"_a <<- i8",
			},
			ExpectedSubst{
				{"_a", "i8"},
			}
		);

		test_constraints(
			{
				"_a <<- $KnownInt(23)",
				"_a ->> i8",
			},
			ExpectedSubst{
				{"_a", "i8"},
			}
		);

		test_constraints(
			{
				"_a <<- $KnownInt(23)",
				"_b <<- _a",
				"_b ->> i8",
			},
			ExpectedSubst{
				{"_a", "i8"},
				{"_b", "i8"},
			}
		);

		test_constraints(
			{
				"_a <<- $KnownInt(23)",
				"_c <<- i8",
				"_b <<- Option'(_a)",
				"_b <<- Option'(_c)",
			},
			ExpectedSubst{
				{"_a", "i8"},
				{"_b", "Option'(i8)"},
				{"_c", "i8"},
			}
		);

		test_constraints(
			{
				"_a <<- $KnownInt(23)",
				"_b <<- Option'(_a)",
				"_b <<- Option'(_c)",
			},
			ExpectedSubst{
				{"_a", "i32"},
				{"_b", "Option'(i32)"},
				{"_c", "i32"},
			}
		);

		// It's unclear what type should be assigned to $KnownInt(23), hence the error
		test_constraints(
			{
				"_a ->> i8",
				"_a ->> i32",
				"_a <<- $KnownInt(23)",
			},
			ExpectedError{"Incompatible types i8 and i32"}
		);
	}},
	{"constraints.conversions", []()
	{
		test_constraints(
			{
				"_a <<- A",
				"_b <<- Option'AorB",
				"_b <<- _a",
			},
			ExpectedSubst{
				{"_a", "A"},
				{"_b", "Option'AorB"},
			}
		);
	}},
	{"constraints.cycles", []()
	{
		// Cyclic constraints seem like they could be useful (though I haven't seen them generated
		// in practice).

		// Here, _a and _b must be assignable to each other, and i32 should be assignable to _a.
		// This can be achieved with [_a ↦ i32, _b ↦ i32].
		test_constraints(
			{
				"_a <<- _b",
				"_a ->> _b",
				"_a <<- i32",
			},
			ExpectedSubst{
				{"_a", "i32"},
				{"_b", "i32"},
			}
		);

		// Longer cycle
		test_constraints(
			{
				"_a <<- _b",
				"_c <<- _a",
				"_b <<- _c",
				"_a <<- i32",
			},
			ExpectedSubst{
				{"_a", "i32"},
				{"_b", "i32"},
				{"_c", "i32"},
			}
		);
	}},
};

int main()
{
	std::random_device rd;
	RANDOM_SEED = rd();
	std::cout << "RANDOM_SEED = " << RANDOM_SEED << std::endl;

	for(auto [name, fn]: TESTS)
	{
		std::cout << "Testing \"" << name << "\"... " << std::flush;
		fn();
		std::cout << "ok" << std::endl;
	}

	return 0;
}
