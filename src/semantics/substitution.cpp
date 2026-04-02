#include "semantics/error.hpp"
#include "semantics/instance_registry.hpp"
#include "semantics/type_properties.hpp"
#include "semantics/module.hpp"
#include "semantics/substitution.hpp"
#include "semantics/type_env.hpp"

#include <unordered_set>

using std::unordered_set;

void substitute(TypeArgList &args, TypeEnv const &env, InstanceRegistry &registry, SubstitutionMode mode)
{
	for(size_t i = 0; i < args.args->count; ++i)
		substitute(args.args->items[i], env, registry, mode);

	// Update args.occurring_vars and args.has_type_deduction_vars
	args.occurring_vars.clear();
	args.has_type_deduction_vars = false;
	for(GenericArg const &arg: *args.args)
	{
		TypeStatInfo info = gather_type_vars(arg, args.occurring_vars, false);
		args.has_type_deduction_vars |= info.has_type_deduction_vars;
		args.has_known_ints |= info.has_known_ints;
	}
}

void validate_unsubstituted_var(GenericVar var, SubstitutionMode mode, Module const &mod)
{
	assert(not is<FullSubstitution>(mode) and "VarType has not been substituted");

	if(FullDeductionSubstitution *m = std::get_if<FullDeductionSubstitution>(&mode))
	{
		if(is<GenericDeductionVar>(var))
			throw_sem_error("Type parameter could not be deduced", m->region_being_substituted.first, &mod);
	}
}

// Make sure that the kinds of variables that must be substituted actually have been substituted
void validate_unsubstituted_vars(
	unordered_set<GenericVar> const &vars,
	bool has_type_deduction_vars,
	TypeEnv const &env,
	SubstitutionMode mode,
	Module const &mod
)
{
	if(
		(is<FullDeductionSubstitution>(mode) and has_type_deduction_vars)
		or (is<FullSubstitution>(mode) and vars.size())
	)
	{
		for(GenericVar var: vars)
		{
			if(not env.try_lookup(var))
				validate_unsubstituted_var(var, mode, mod);
		}
	}
}

// If `inst` is deduction complete, apply `env` to it directly and return `inst`.
// Otherwise, leave `inst` unchanged and return the StructInstance that corresponds to `inst` where
// `env` has been applied to the type args of `inst`
StructInstance* substitute_types_in_struct(
	StructInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode,
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
				new_decl_parent = substitute_types_in_struct(parent_inst, env, registry, mode, &parent_modified);
			},
			[&](ProcInstance *parent_inst)
			{
				new_decl_parent = substitute_types_in_proc(parent_inst, env, registry, mode, &parent_modified);
			},
		};
	}

	if(parent_modified or inst->type_args().needs_substitution(env))
	{
		inst = registry.get_struct_instance(inst->struct_(), inst->type_args(), env, new_decl_parent, mode);

		LOGGER(logger, on_struct_substitution_replaced, inst);
		if(modified) *modified = true;
	}
	else
	{
		validate_unsubstituted_vars(
			inst->type_args().occurring_vars,
			inst->type_args().has_type_deduction_vars,
			env, mode, registry.mod()
		);

		LOGGER(logger, on_struct_substitution_noop);
		if(modified) *modified = false;
	}

	LOGGER(logger, on_struct_substitution_end);

	return inst;
}

ProcInstance* substitute_types_in_proc(
	ProcInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode,
	bool *modified
)
{
	if(inst->type_args().needs_substitution(env))
	{
		inst = registry.get_proc_instance(inst->proc(), inst->type_args(), env, mode);
		if(modified) *modified = true;
	}
	else
	{
		validate_unsubstituted_vars(
			inst->type_args().occurring_vars,
			inst->type_args().has_type_deduction_vars,
			env, mode, registry.mod()
		);
		if(modified) *modified = false;
	}

	return inst;
}

static ProcTypeInstance* substitute_types_in_proc_type(
	ProcTypeInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
)
{
	if(inst->needs_substitution(env))
		inst = registry.get_proc_type_instance(inst->params, inst->ret, env, mode);
	else
	{
		validate_unsubstituted_vars(
			inst->occurring_vars,
			inst->has_type_deduction_vars,
			env, mode, registry.mod()
		);
	}

	return inst;
}

static UnionInstance* substitute_types_in_union(
	UnionInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
)
{
	if(inst->needs_substitution(env))
	{
		vector<Type*> new_alts;
		new_alts.reserve(inst->alternatives().size());
		for(Type const *alt: inst->alternatives())
		{
			Type *new_alt = clone_ptr(alt, registry.arena());
			substitute(*new_alt, env, registry, mode);
			new_alts.push_back(new_alt);
		}

		canonicalize_union_alternatives(new_alts, registry.arena());
		inst = registry.get_union_instance(std::move(new_alts));
	}
	else
	{
		validate_unsubstituted_vars(
			inst->occurring_vars(),
			inst->has_type_deduction_vars(),
			env, mode, registry.mod()
		);
	}

	return inst;
}

static optional<GenericArg> get_substituted(
	GenericVar var,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
)
{
	if(GenericArg const *mapped_arg = env.try_lookup(var))
	{
		GenericArg result = clone(*mapped_arg, registry.arena());

		// Apply substitution recursively.
		// This is not how substitution is traditionally defined for e.g. first-order logic,
		// but is easier to implement. See note for the unify() function.
		substitute(result, env, registry, mode); // TODO Implement substitute() for GenericArg
		return result;
	}
	else
	{
		validate_unsubstituted_var(var, mode, registry.mod());
		return nullopt;
	}
}
void substitute(
	Type &type,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
)
{
	type | match
	{
		[&](BuiltinType&) {},
		[&](KnownIntType &t)
		{
			if(is<FullDeductionSubstitution>(mode))
				type = materialize_known_int(t);
		},
		[&](PointerType &t)
		{
			substitute(*t.pointee, env, registry, mode);
		},
		[&](ArrayType &t)
		{
			substitute(*t.element, env, registry, mode);
			substitute_types_in_expr(*t.count_arg, env, registry, mode);
		},
		[&](StructType &t)
		{
			t.inst = substitute_types_in_struct(t.inst, env, registry, mode);
		},
		[&](ProcType &t)
		{
			t.inst = substitute_types_in_proc_type(t.inst, env, registry, mode);
			t.callable = t.callable->get_substituted(env, registry, mode);
		},
		[&](UnionType &t)
		{
			t.inst = substitute_types_in_union(t.inst, env, registry, mode);
		},
		[&](VarType &t)
		{
			if(optional<GenericArg> result = get_substituted(t.var, env, registry, mode))
				type = std::get<Type>(*result);
		},
		[&](ProcTypeUnresolved const&) { UNREACHABLE; },
		[&](UnionTypeUnresolved const&) { UNREACHABLE; },
		[&](Path const&) { UNREACHABLE; },
		[&](InlineStructType const&) { UNREACHABLE; },
	};
}

void substitute_types_in_expr(Expr &expr, TypeEnv const &env, InstanceRegistry &registry, SubstitutionMode mode)
{
	if(Type *expr_type = expr.try_get_type())
		substitute(*expr_type, env, registry, mode);

	expr | match
	{
		[&](IntLiteralExpr&) {},
		[&](BoolLiteralExpr&) {},
		[&](StringLiteralExpr&) {},
		[&](UnaryExpr &e)
		{
			substitute_types_in_expr(*e.sub, env, registry, mode);
		},
		[&](BinaryExpr &e)
		{
			substitute_types_in_expr(*e.left, env, registry, mode);
			substitute_types_in_expr(*e.right, env, registry, mode);
		},
		[&](AddressOfExpr &e)
		{
			substitute_types_in_expr(*e.object, env, registry, mode);
		},
		[&](DerefExpr &e)
		{
			substitute_types_in_expr(*e.addr, env, registry, mode);
		},
		[&](IndexExpr &e)
		{
			substitute_types_in_expr(*e.addr, env, registry, mode);
			substitute_types_in_expr(*e.index, env, registry, mode);
		},
		[&](MemberAccessExpr &e)
		{
			substitute_types_in_expr(*e.object, env, registry, mode);
		},
		[&](AssignmentExpr &e)
		{
			substitute_types_in_expr(*e.lhs, env, registry, mode);
			substitute_types_in_expr(*e.rhs, env, registry, mode);
		},
		[&](AsExpr &e)
		{
			substitute_types_in_expr(*e.src_expr, env, registry, mode);
			substitute(*e.target_type, env, registry, mode);
		},
		[&](ConstructorExpr &e)
		{
			substitute(*e.ctor, env, registry, mode);
		},
		[&](ProcExpr &e)
		{
			e.inst = substitute_types_in_proc(e.inst, env, registry, mode);
		},
		[&](CallExpr &e)
		{
			substitute_types_in_expr(*e.callable, env, registry, mode);

			for(Argument &arg: *e.args)
				substitute_types_in_expr(arg.expr, env, registry, mode);
		},
		[&](SizeOfExpr &e)
		{
			substitute(*e.subject, env, registry, mode);
		},
		[&](MakeExpr&)
		{
			assert(!"[TODO] substitute_types_in_expr: MakeExpr");
		},
		[&](UnionInitExpr &e)
		{
			substitute_types_in_expr(*e.alt_expr, env, registry, mode);
			substitute(*e.alt_type, env, registry, mode);
		},
		[&](VarExpr&) {},
		[&](GenericVarExpr &e)
		{
			if(optional<GenericArg> result = get_substituted(e.var, env, registry, mode))
				expr = std::get<Expr>(*result);
		},
		[&](Path&) { assert(!"substitute_types_in_expr: Path"); },
	};
}

void substitute_types_in_pattern(
	Pattern &pattern,
	TypeEnv const &subst,
	InstanceRegistry &registry,
	SubstitutionMode mode
)
{
	substitute(pattern.type(), subst, registry, mode);

	if(pattern.provided_type)
		substitute(*pattern.provided_type, subst, registry, mode);

	pattern | match
	{
		[&](VarPattern &p)
		{
			substitute(*p.var->type, subst, registry, mode);
		},
		[&](DerefPattern &p)
		{
			substitute_types_in_pattern(*p.sub, subst, registry, mode);
		},
		[&](AddressOfPattern &p)
		{
			substitute_types_in_pattern(*p.sub, subst, registry, mode);
		},
		[&](ConstructorPattern &p)
		{
			substitute(*p.ctor, subst, registry, mode);
			for(PatternArgument &arg: *p.args)
				substitute_types_in_pattern(arg.pattern, subst, registry, mode);
		},
		[&](WildcardPattern &) {},
		[&](VarPatternUnresolved&) { assert(!"substitute_types_in_pattern: VarPatternUnresolved"); },
	};
}

void substitute_types_in_stmt(Stmt &stmt, TypeEnv const &subst, InstanceRegistry &registry, SubstitutionMode mode)
{
	stmt | match
	{
		[&](LetStmt &s)
		{
			substitute_types_in_pattern(*s.lhs, subst, registry, mode);
			substitute_types_in_expr(*s.init_expr, subst, registry, mode);
		},
		[&](ExprStmt &s)
		{
			substitute_types_in_expr(*s.expr, subst, registry, mode);
		},
		[&](BlockStmt &s)
		{
			for(Stmt &child_stmt: *s.stmts)
				substitute_types_in_stmt(child_stmt, subst, registry, mode);
		},
		[&](ReturnStmt &s)
		{
			substitute_types_in_expr(*s.ret_expr, subst, registry, mode);
		},
		[&](IfStmt &s)
		{
			substitute_types_in_expr(*s.condition, subst, registry, mode);
			substitute_types_in_stmt(*s.then, subst, registry, mode);
			if(s.else_)
				substitute_types_in_stmt(*s.else_, subst, registry, mode);
		},
		[&](WhileStmt &s)
		{
			substitute_types_in_expr(*s.condition, subst, registry, mode);
			substitute_types_in_stmt(*s.body, subst, registry, mode);
		},
		[&](DeclStmt&) {},
		[&](MatchStmt &s)
		{
			substitute_types_in_expr(*s.expr, subst, registry, mode);
			for(MatchArm &arm: *s.arms)
			{
				substitute_types_in_pattern(arm.capture, subst, registry, mode);
				substitute_types_in_stmt(arm.stmt, subst, registry, mode);
			}
		},
	};
}

void substitute(
	GenericArg &arg,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
)
{
	arg | match
	{
		[&](Type &type) { substitute(type, env, registry, mode); },
		[&](Expr &expr) { substitute_types_in_expr(expr, env, registry, mode); },
	};
}
