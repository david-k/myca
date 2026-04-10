#include "semantics/error.hpp"
#include "semantics/instance_registry.hpp"
#include "semantics/type_properties.hpp"
#include "semantics/module.hpp"
#include "semantics/substitution.hpp"
#include "semantics/type_env.hpp"

#include <unordered_set>

using std::unordered_set;

static void validate_unsubstituted_var(
	GenericVar var,
	SubstitutionOptions opts,
	Module const &mod
)
{
	if(opts.mode == SubstitutionMode::FULL)
	{
		switch(opts.phase)
		{
			case SubstitutionPhase::DEDUCTION:
			{
				if(is<GenericDeductionVar>(var))
					throw_sem_error("Type parameter could not be deduced", opts.region_being_substituted.first, &mod);
			} break;

			case SubstitutionPhase::INSTANTIATION:
				assert("VarType has not been substituted");
				break;
		}
	}
}

// Make sure that the kinds of variables that must be substituted actually have been substituted
static void validate_unsubstituted_vars(
	unordered_set<GenericVar> const &vars,
	TypeEnv const &env,
	SubstitutionOptions opts,
	Module const &mod
)
{
	for(GenericVar var: vars)
	{
		if(not env.try_lookup(var))
			validate_unsubstituted_var(var, opts, mod);
	}
}

void substitute_in_type_args(TypeArgList &args, TypeEnv const &env, InstanceRegistry &registry, SubstitutionOptions opts)
{
	for(size_t i = 0; i < args.args->count; ++i)
		substitute_in_generic_arg(args.args->items[i], env, registry, opts);

	// Update args.occurring_vars and args.has_type_deduction_vars
	args.occurring_vars.clear();
	args.has_type_deduction_vars = false;
	for(GenericArg const &arg: *args.args)
	{
		TermInfo info = gather_type_vars(arg, args.occurring_vars, false);
		args.has_type_deduction_vars |= info.has_deduction_vars;
		args.has_known_ints |= info.has_known_ints;
	}
}

// If `inst` is deduction complete, apply `env` to it directly and return `inst`.
// Otherwise, leave `inst` unchanged and return the StructInstance that corresponds to `inst` where
// `env` has been applied to the type args of `inst`
StructInstance* substitute_in_struct(
	StructInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions opts,
	bool *modified
)
{
	optional<EventLogger> &logger = registry.mod().sema->logger;
	LOGGER(logger, on_struct_substitution_start, inst);
	LOGGER(logger, on_data, env);

	optional<DeclContainerInst> new_decl_parent = nullopt;
	bool parent_modified = false;
	if(inst->decl_parent())
	{
		*inst->decl_parent() | match
		{
			[&](StructInstance *parent_inst)
			{
				new_decl_parent = substitute_in_struct(parent_inst, env, registry, opts, &parent_modified);
			},
			[&](ProcInstance *parent_inst)
			{
				new_decl_parent = substitute_in_proc(parent_inst, env, registry, opts, &parent_modified);
			},
		};
	}

	if(parent_modified or inst->type_args().needs_substitution(env))
	{
		inst = registry.get_struct_instance(inst->struct_(), inst->type_args(), env, new_decl_parent, opts);

		LOGGER(logger, on_struct_substitution_replaced, inst);
		if(modified) *modified = true;
	}
	else
	{
		validate_unsubstituted_vars(
			inst->type_args().occurring_vars,
			env, opts, registry.mod()
		);

		LOGGER(logger, on_struct_substitution_noop);
		if(modified) *modified = false;
	}

	LOGGER(logger, on_struct_substitution_end);

	return inst;
}

ProcInstance* substitute_in_proc(
	ProcInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions opts,
	bool *modified
)
{
	if(inst->type_args().needs_substitution(env))
	{
		inst = registry.get_proc_instance(inst->proc(), inst->type_args(), env, opts);
		if(modified) *modified = true;
	}
	else
	{
		validate_unsubstituted_vars(
			inst->type_args().occurring_vars,
			env, opts, registry.mod()
		);
		if(modified) *modified = false;
	}

	return inst;
}

static ProcTypeInstance* substitute_in_proc_type(
	ProcTypeInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions opts
)
{
	if(inst->needs_substitution(env))
		inst = registry.get_proc_type_instance(inst->params, inst->ret, env, opts);
	else
	{
		validate_unsubstituted_vars(
			inst->occurring_vars,
			env, opts, registry.mod()
		);
	}

	return inst;
}

static UnionInstance* substitute_in_union(
	UnionInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions opts
)
{
	if(inst->needs_substitution(env))
	{
		vector<Type*> new_alts;
		new_alts.reserve(inst->alternatives().size());
		for(Type const *alt: inst->alternatives())
		{
			Type *new_alt = clone_ptr(alt, registry.arena());
			substitute_in_type(*new_alt, env, registry, opts);
			new_alts.push_back(new_alt);
		}

		canonicalize_union_alternatives(new_alts, registry.arena());
		inst = registry.get_union_instance(std::move(new_alts));
	}
	else
	{
		validate_unsubstituted_vars(
			inst->occurring_vars(),
			env, opts, registry.mod()
		);
	}

	return inst;
}

static optional<GenericArg> get_substituted(
	GenericVar var,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions opts
)
{
	if(GenericArg const *mapped_arg = env.try_lookup(var))
	{
		GenericArg result = clone(*mapped_arg, registry.arena());

		// Apply substitution recursively.
		// This is not how substitution is traditionally defined for e.g. first-order logic,
		// but is easier to implement. See note for the Unifier::go() function.
		substitute_in_generic_arg(result, env, registry, opts);
		return result;
	}
	else
	{
		validate_unsubstituted_var(var, opts, registry.mod());
		return nullopt;
	}
}

static void validate_const_eval(Expr &expr, Module const &mod, SubstitutionOptions opts)
{
	try {
		const_eval(expr, mod);
	}
	catch(ParseError const &exc)
	{
		if(opts.mode == SubstitutionMode::FULL)
		{
			// FULL substitution is only performed during code generation. If const_eval fails at
			// that point we messed up during semantic analysis. (We are doing definition-checked
			// generics, so all potential errors should be caught before instantiation.)
			assert(!"substitute_in_type: ArrayType: const_eval failed");
		}
		throw;
	}
}

void substitute_in_type(
	Type &type,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions opts
)
{
	type | match
	{
		[&](BuiltinType&) {},
		[&](KnownIntType &t)
		{
			if(opts.phase == SubstitutionPhase::DEDUCTION and opts.mode == SubstitutionMode::FULL)
				type = materialize_known_int(t);
		},
		[&](PointerType &t)
		{
			substitute_in_type(*t.pointee, env, registry, opts);
		},
		[&](ArrayType &t)
		{
			substitute_in_type(*t.element, env, registry, opts);
			substitute_in_expr(*t.count_arg, env, registry, opts);
			validate_const_eval(*t.count_arg, registry.mod(), opts);
		},
		[&](StructType &t)
		{
			t.inst = substitute_in_struct(t.inst, env, registry, opts);
		},
		[&](ProcType &t)
		{
			t.inst = substitute_in_proc_type(t.inst, env, registry, opts);
			t.callable = t.callable->get_substituted(env, registry, opts);
		},
		[&](UnionType &t)
		{
			t.inst = substitute_in_union(t.inst, env, registry, opts);
		},
		[&](VarType &t)
		{
			if(optional<GenericArg> result = get_substituted(t.var, env, registry, opts))
				type = std::get<Type>(*result);
		},
		[&](ProcTypeUnresolved const&) { UNREACHABLE; },
		[&](UnionTypeUnresolved const&) { UNREACHABLE; },
		[&](Path const&) { UNREACHABLE; },
		[&](InlineStructType const&) { UNREACHABLE; },
	};
}

void substitute_in_expr(Expr &expr, TypeEnv const &env, InstanceRegistry &registry, SubstitutionOptions opts)
{
	if(Type *expr_type = expr.try_get_type())
		substitute_in_type(*expr_type, env, registry, opts);

	expr | match
	{
		[&](IntLiteralExpr&) {},
		[&](BoolLiteralExpr&) {},
		[&](StringLiteralExpr&) {},
		[&](UnaryExpr &e)
		{
			substitute_in_expr(*e.sub, env, registry, opts);
		},
		[&](BinaryExpr &e)
		{
			substitute_in_expr(*e.left, env, registry, opts);
			substitute_in_expr(*e.right, env, registry, opts);
		},
		[&](AddressOfExpr &e)
		{
			substitute_in_expr(*e.object, env, registry, opts);
		},
		[&](DerefExpr &e)
		{
			substitute_in_expr(*e.addr, env, registry, opts);
		},
		[&](IndexExpr &e)
		{
			substitute_in_expr(*e.addr, env, registry, opts);
			substitute_in_expr(*e.index, env, registry, opts);
		},
		[&](MemberAccessExpr &e)
		{
			substitute_in_expr(*e.object, env, registry, opts);
		},
		[&](AssignmentExpr &e)
		{
			substitute_in_expr(*e.lhs, env, registry, opts);
			substitute_in_expr(*e.rhs, env, registry, opts);
		},
		[&](AsExpr &e)
		{
			substitute_in_expr(*e.src_expr, env, registry, opts);
			substitute_in_type(*e.target_type, env, registry, opts);
		},
		[&](ConstructorExpr &e)
		{
			substitute_in_type(*e.ctor, env, registry, opts);
		},
		[&](ProcExpr &e)
		{
			e.inst = substitute_in_proc(e.inst, env, registry, opts);
		},
		[&](CallExpr &e)
		{
			substitute_in_expr(*e.callable, env, registry, opts);

			for(Argument &arg: *e.args)
				substitute_in_expr(arg.expr, env, registry, opts);
		},
		[&](SizeOfExpr &e)
		{
			substitute_in_type(*e.subject, env, registry, opts);
		},
		[&](MakeExpr&)
		{
			assert(!"[TODO] substitute_types_in_expr: MakeExpr");
		},
		[&](UnionInitExpr &e)
		{
			substitute_in_expr(*e.alt_expr, env, registry, opts);
			substitute_in_type(*e.alt_type, env, registry, opts);
		},
		[&](VarExpr&) {},
		[&](GenericVarExpr &e)
		{
			if(optional<GenericArg> result = get_substituted(e.var, env, registry, opts))
				expr = std::get<Expr>(*result);
		},
		[&](Path&) { assert(!"substitute_types_in_expr: Path"); },
	};
}

void substitute_in_pattern(
	Pattern &pattern,
	TypeEnv const &subst,
	InstanceRegistry &registry,
	SubstitutionOptions opts
)
{
	substitute_in_type(pattern.type(), subst, registry, opts);

	if(pattern.provided_type)
		substitute_in_type(*pattern.provided_type, subst, registry, opts);

	pattern | match
	{
		[&](VarPattern &p)
		{
			// We must only update the type of the referenced Var during type deduction.
			// Once the type of the Var has been deduced, it is not supposed to change anymore.
			if(opts.phase == SubstitutionPhase::DEDUCTION)
				substitute_in_type(*p.var->type, subst, registry, opts);
		},
		[&](DerefPattern &p)
		{
			substitute_in_pattern(*p.sub, subst, registry, opts);
		},
		[&](AddressOfPattern &p)
		{
			substitute_in_pattern(*p.sub, subst, registry, opts);
		},
		[&](ConstructorPattern &p)
		{
			substitute_in_type(*p.ctor, subst, registry, opts);
			for(PatternArgument &arg: *p.args)
				substitute_in_pattern(arg.pattern, subst, registry, opts);
		},
		[&](WildcardPattern &) {},
		[&](VarPatternUnresolved&) { assert(!"substitute_types_in_pattern: VarPatternUnresolved"); },
	};
}

void substitute_in_stmt(Stmt &stmt, TypeEnv const &subst, InstanceRegistry &registry, SubstitutionOptions opts)
{
	stmt | match
	{
		[&](LetStmt &s)
		{
			substitute_in_pattern(*s.lhs, subst, registry, opts);
			substitute_in_expr(*s.init_expr, subst, registry, opts);
		},
		[&](ExprStmt &s)
		{
			substitute_in_expr(*s.expr, subst, registry, opts);
		},
		[&](BlockStmt &s)
		{
			for(Stmt &child_stmt: *s.stmts)
				substitute_in_stmt(child_stmt, subst, registry, opts);
		},
		[&](ReturnStmt &s)
		{
			substitute_in_expr(*s.ret_expr, subst, registry, opts);
		},
		[&](IfStmt &s)
		{
			substitute_in_expr(*s.condition, subst, registry, opts);
			substitute_in_stmt(*s.then, subst, registry, opts);
			if(s.else_)
				substitute_in_stmt(*s.else_, subst, registry, opts);
		},
		[&](WhileStmt &s)
		{
			substitute_in_expr(*s.condition, subst, registry, opts);
			substitute_in_stmt(*s.body, subst, registry, opts);
		},
		[&](DeclStmt&) {},
		[&](MatchStmt &s)
		{
			substitute_in_expr(*s.expr, subst, registry, opts);
			for(MatchArm &arm: *s.arms)
			{
				substitute_in_pattern(arm.capture, subst, registry, opts);
				substitute_in_stmt(arm.stmt, subst, registry, opts);
			}
		},
	};
}

void substitute_in_generic_arg(
	GenericArg &arg,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions opts
)
{
	arg | match
	{
		[&](Type &type) { substitute_in_type(type, env, registry, opts); },
		[&](Expr &expr)
		{
			substitute_in_expr(expr, env, registry, opts);
			validate_const_eval(expr, registry.mod(), opts);
		},
	};
}
