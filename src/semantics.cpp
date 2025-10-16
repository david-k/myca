#include "semantics.hpp"
#include "syntax.hpp"
#include "utils.hpp"
#include <memory>
#include <ranges>
#include <unordered_map>
#include <variant>

//#define DEBUG_PRINTING

//#ifdef DEBUG_PRINTING
#include <iostream>
//#endif


//==============================================================================
[[noreturn]] static void throw_sem_error(string const &msg, TokenIdx tok_idx, Module const *mod)
{
	if(tok_idx.value == INVALID_TOKEN_IDX.value)
		throw ParseError("error: " + msg);
	else
	{
		Token const &tok = mod->tokens[tok_idx.value];
		throw ParseError("|" + str(tok.span.begin) + "| error: " + msg);
	}
}

Expr clone(Expr const &expr, InstanceRegistry *registry);

Expr* clone_ptr(Expr const *expr, InstanceRegistry *registry)
{
	if(not expr)
		return nullptr;

	return registry->arena().alloc<Expr>(clone(*expr, registry));
}

DefaultValueExpr DefaultValueExpr::clone(InstanceRegistry *registry) const
{
	return *this | match
	{
		[&](NoDefaultValue) -> DefaultValueExpr { return NoDefaultValue(); },
		[&](ExprPending) -> DefaultValueExpr { return ExprPending(); },
		[&](Expr *expr) -> DefaultValueExpr
		{
			return clone_ptr(expr, registry);
		},
	};
}

ProcTypeParamAux clone(ProcTypeParamAux const &param_aux, InstanceRegistry *registry)
{
	return ProcTypeParamAux{
		.name = param_aux.name,
		.default_value = param_aux.default_value.clone(registry),
	};
}

FixedArray<ProcTypeParamAux>* clone(FixedArray<ProcTypeParamAux> const *param_aux, InstanceRegistry *registry)
{
	FixedArray<ProcTypeParamAux> *result = alloc_fixed_array<ProcTypeParamAux>(param_aux->count, registry->arena());
	for(size_t i = 0; i < param_aux->count; ++i)
		result->items[i] = clone(param_aux->items[i], registry);

	return result;
}

Type clone(Type const &type, InstanceRegistry *registry);

FixedArray<Type>* clone(FixedArray<Type> const *types, InstanceRegistry *registry)
{
	FixedArray<Type> *result = alloc_fixed_array<Type>(types->count, registry->arena());
	for(size_t i = 0; i < types->count; ++i)
		result->items[i] = clone(types->items[i], registry);

	return result;
}


Type* clone_ptr(Type const *type, InstanceRegistry *registry)
{
	if(not type)
		return nullptr;

	return registry->arena().alloc<Type>(clone(*type, registry));
}

TypeArgList clone(TypeArgList const &args, InstanceRegistry *registry);

Type clone(Type const &type, InstanceRegistry *registry)
{
	return type | match
	{
		[&](BuiltinType const &t) -> Type
		{
			return t;
		},
		[&](KnownIntType const &t) -> Type
		{
			return t;
		},
		[&](PointerType const &t) -> Type
		{
			return PointerType{
				.range = t.range,
				.pointee = clone_ptr(t.pointee, registry),
				.mutability = t.mutability,
			};
		},
		[&](ManyPointerType const &t) -> Type
		{
			return ManyPointerType{
				.range = t.range,
				.pointee = clone_ptr(t.pointee, registry),
				.mutability = t.mutability,
			};
		},
		[&](StructType const &t) -> Type
		{
			return t;
		},
		[&](ProcTypeUnresolved const &t) -> Type
		{
			return ProcTypeUnresolved{
				.range = t.range,
				.ret = clone_ptr(t.ret, registry),
				.params = clone(t.params, registry),
			};
		},
		[&](ProcType const &t) -> Type
		{
			ProcTypeInstance *new_inst = t.inst;
			if(t.inst->has_type_deduction_vars)
			{
				new_inst = registry->get_proc_type_instance(
					clone(t.inst->params, registry),
					clone_ptr(t.inst->ret, registry)
				);
			}

			return ProcType{
				.range = t.range,
				.inst = new_inst,
				.callable = t.callable
			};
		},
		[&](UnionTypeUnresolved const &) -> Type
		{
			assert(!"[TODO] clone: UnionTypeUnresolved");
		},
		[&](UnionType const &) -> Type
		{
			assert(!"[TODO] clone: UnionType");
		},
		[&](VarType const &t) -> Type
		{
			return t;
		},
		[&](Path const&) -> Type { UNREACHABLE; },
	};
}

TypeArgList clone(TypeArgList const &args, InstanceRegistry *registry)
{
	return TypeArgList{
		.args = clone(args.args, registry),
		.occurring_vars = args.occurring_vars,
		.has_type_deduction_vars = args.has_type_deduction_vars,
	};
}

Argument clone(Argument const &arg, InstanceRegistry *registry);

FixedArray<Argument>* clone(FixedArray<Argument> const *args, InstanceRegistry *registry)
{
	FixedArray<Argument> *result = alloc_fixed_array<Argument>(args->count, registry->arena());
	for(size_t i = 0; i < args->count; ++i)
		result->items[i] = clone(args->items[i], registry);

	return result;
}

Expr clone(Expr const &expr, InstanceRegistry *registry)
{
	return expr | match
	{
		[&](IntLiteralExpr const &e) -> Expr
		{
			return IntLiteralExpr{
				.range = e.range,
				.value = e.value,
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](BoolLiteralExpr const &e) -> Expr
		{
			return BoolLiteralExpr{
				.range = e.range,
				.value = e.value,
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](StringLiteralExpr const &e) -> Expr
		{
			return StringLiteralExpr{
				.range = e.range,
				.kind = e.kind,
				.value = e.value,
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](UnaryExpr const &e) -> Expr
		{
			return UnaryExpr{
				.range = e.range,
				.sub = clone_ptr(e.sub, registry),
				.op = e.op,
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](BinaryExpr const &e) -> Expr
		{
			return BinaryExpr{
				.range = e.range,
				.left = clone_ptr(e.left, registry),
				.right = clone_ptr(e.right, registry),
				.op = e.op,
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](AddressOfExpr const &e) -> Expr
		{
			return AddressOfExpr{
				.range = e.range,
				.object = clone_ptr(e.object, registry),
				.mutability = e.mutability,
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](DerefExpr const &e) -> Expr
		{
			return DerefExpr{
				.range = e.range,
				.addr = clone_ptr(e.addr, registry),
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](IndexExpr const &e) -> Expr
		{
			return IndexExpr{
				.range = e.range,
				.addr = clone_ptr(e.addr, registry),
				.index = clone_ptr(e.index, registry),
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](MemberAccessExpr const &e) -> Expr
		{
			return MemberAccessExpr{
				.range = e.range,
				.object = clone_ptr(e.object, registry),
				.member = e.member,
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](AssignmentExpr const &e) -> Expr
		{
			return AssignmentExpr{
				.range = e.range,
				.lhs = clone_ptr(e.lhs, registry),
				.rhs = clone_ptr(e.rhs, registry),
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](AsExpr const &e) -> Expr
		{
			return AsExpr{
				.range = e.range,
				.src_expr = clone_ptr(e.src_expr, registry),
				.target_type = clone_ptr(e.target_type, registry),
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](ConstructorExpr const &e) -> Expr
		{
			return ConstructorExpr{
				.range = e.range,
				.ctor = clone_ptr(e.ctor, registry),
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](ProcExpr const &e) -> Expr
		{
			// TODO If the ProcInstance has type deduction vars we need to clone it
			assert(not e.inst->type_args().has_type_deduction_vars);

			return ProcExpr{
				.range = e.range,
				.inst = e.inst, // TODO May need to be cloned
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](CallExpr const &e) -> Expr
		{
			return CallExpr{
				.range = e.range,
				.callable = clone_ptr(e.callable, registry),
				.args = clone(e.args, registry),
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](SizeOfExpr const &e) -> Expr
		{
			return SizeOfExpr{
				.range = e.range,
				.subject = clone_ptr(e.subject, registry),
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](MakeExpr const &e) -> Expr
		{
			return MakeExpr{
				.range = e.range,
				.init = clone_ptr(e.init, registry),
				.addr = clone_ptr(e.addr, registry),
				.type = clone_ptr(e.type, registry),
			};
		},
		[&](Path const&) -> Expr
		{
			assert(!"[TODO] clone(Expr): Path");
		},
		[&](VarExpr const &e) -> Expr
		{
			return VarExpr{
				.range = e.range,
				.var = e.var,
				.type = clone_ptr(e.type, registry),
			};
		},
	};
}

Argument clone(Argument const &arg, InstanceRegistry *registry)
{
	return Argument{
		.range = arg.range,
		.expr = clone(arg.expr, registry),
		.name = arg.name,
		.param_idx = arg.param_idx,
	};
}

Parameter clone(Parameter const &param, InstanceRegistry *registry)
{
	return Parameter{
		.range = param.range,
		.type = clone_ptr(param.type, registry),
		.default_value = param.default_value.clone(registry),
	};
}

Pattern* clone_ptr(Pattern const *pattern, InstanceRegistry *registry);
FixedArray<PatternArgument>* clone(FixedArray<PatternArgument> const *args, InstanceRegistry *registry);

Pattern clone(Pattern const &pattern, InstanceRegistry *registry)
{
	return pattern | match
	{
		[&](VarPatternUnresolved const &p)
		{
			return Pattern(
				VarPatternUnresolved(p.range, p.name, p.mutability),
				clone_ptr(pattern.provided_type, registry)
			);
		},
		[&](VarPattern const &p)
		{
			return Pattern(
				VarPattern(p.range, p.var, clone_ptr(p.type, registry)),
				clone_ptr(pattern.provided_type, registry)
			);
		},
		[&](DerefPattern const &p)
		{
			return Pattern(
				DerefPattern(p.range, clone_ptr(p.sub, registry), clone_ptr(p.type, registry)),
				clone_ptr(pattern.provided_type, registry)
			);
		},
		[&](AddressOfPattern const &p)
		{
			return Pattern(
				AddressOfPattern(
					p.range,
					clone_ptr(p.sub, registry),
					p.mutability,
					clone_ptr(p.type, registry)
				),
				clone_ptr(pattern.provided_type, registry)
			);
		},
		[&](ConstructorPattern const &p)
		{
			return Pattern(
				ConstructorPattern(
					p.range,
					clone_ptr(p.ctor, registry),
					clone(p.args, registry),
					p.has_parens,
					clone_ptr(p.type, registry)
				),
				clone_ptr(pattern.provided_type, registry)
			);
		},
		[&](WildcardPattern const &p)
		{
			return Pattern(
				WildcardPattern(p.range, clone_ptr(p.type, registry)),
				clone_ptr(pattern.provided_type, registry)
			);
		}
	};
}

Pattern* clone_ptr(Pattern const *pattern, InstanceRegistry *registry)
{
	if(not pattern)
		return nullptr;

	return registry->arena().alloc<Pattern>(clone(*pattern, registry));
}

PatternArgument clone(PatternArgument const &pat_arg, InstanceRegistry *registry)
{
	return PatternArgument{
		.pattern = clone(pat_arg.pattern, registry),
		.param_name = pat_arg.param_name,
		.param_idx = pat_arg.param_idx,
	};
}

FixedArray<PatternArgument>* clone(FixedArray<PatternArgument> const *args, InstanceRegistry *registry)
{
	FixedArray<PatternArgument> *result = alloc_fixed_array<PatternArgument>(args->count, registry->arena());
	for(size_t i = 0; i < args->count; ++i)
		result->items[i] = clone(args->items[i], registry);

	return result;
}

Stmt* clone_ptr(Stmt const *stmt, InstanceRegistry *registry);
FixedArray<Stmt>* clone(FixedArray<Stmt> const *stmts, InstanceRegistry *registry);

FixedArray<MatchArm>* clone(FixedArray<MatchArm> const *arms, InstanceRegistry *registry);

Stmt clone(Stmt const &stmt, InstanceRegistry *registry)
{
	return stmt | match
	{
		[&](LetStmt const &s) -> Stmt
		{
			return LetStmt{
				.range = s.range,
				.lhs = clone_ptr(s.lhs, registry),
				.init_expr = clone_ptr(s.init_expr, registry),
			};
		},
		[&](ExprStmt const &s) -> Stmt
		{
			return ExprStmt(s.range, clone_ptr(s.expr, registry));
		},
		[&](BlockStmt const &s) -> Stmt
		{
			return BlockStmt(s.range, clone(s.stmts, registry));
		},
		[&](ReturnStmt const &s) -> Stmt
		{
			return ReturnStmt(s.range, clone_ptr(s.ret_expr, registry));
		},
		[&](IfStmt const &s) -> Stmt
		{
			return IfStmt(
				s.range,
				clone_ptr(s.condition, registry),
				clone_ptr(s.then, registry),
				clone_ptr(s.else_, registry)
			);
		},
		[&](WhileStmt const &s) -> Stmt
		{
			return WhileStmt(
				s.range,
				clone_ptr(s.condition, registry),
				clone_ptr(s.body, registry)
			);
		},
		[&](MatchStmt const &s) -> Stmt
		{
			return MatchStmt(
				s.range,
				clone_ptr(s.expr, registry),
				clone(s.arms, registry)
			);
		},
	};
}

Stmt* clone_ptr(Stmt const *stmt, InstanceRegistry *registry)
{
	if(not stmt)
		return nullptr;

	return registry->arena().alloc<Stmt>(clone(*stmt, registry));
}

FixedArray<Stmt>* clone(FixedArray<Stmt> const *stmts, InstanceRegistry *registry)
{
	FixedArray<Stmt> *result = alloc_fixed_array<Stmt>(stmts->count, registry->arena());
	for(size_t i = 0; i < stmts->count; ++i)
		result->items[i] = clone(stmts->items[i], registry);

	return result;
}

MatchArm clone(MatchArm const &arm, InstanceRegistry *registry)
{
	return MatchArm{
		.capture = clone(arm.capture, registry),
		.stmt = clone(arm.stmt, registry),
		.discr = arm.discr,
	};
}

FixedArray<MatchArm>* clone(FixedArray<MatchArm> const *arms, InstanceRegistry *registry)
{
	FixedArray<MatchArm> *result = alloc_fixed_array<MatchArm>(arms->count, registry->arena());
	for(size_t i = 0; i < arms->count; ++i)
		result->items[i] = clone(arms->items[i], registry);

	return result;
}


Type* type_of(Expr &expr)
{
	return expr | match
	{
		[](Path&) -> Type* { assert(!"type_of: Path"); },
		[](auto &e) { return e.type; },
	};
}

Type const* type_of(Expr const &expr)
{
	return expr | match
	{
		[](Path const&) -> Type* { assert(!"type_of: Path"); },
		[](auto const &e) { return e.type; },
	};
}

Type& type_of(Pattern &pattern)
{
	return pattern | match
	{
		[](VarPatternUnresolved&) -> Type& { assert(!"type_of: VarPatternUnresolved"); },
		[](auto &e) -> Type& { assert(e.type); return *e.type; },
	};
}

Type const& type_of(Pattern const &pattern)
{
	return pattern | match
	{
		[](VarPatternUnresolved const&) -> Type const& { assert(!"type_of: VarPatternUnresolved"); },
		[](auto &e) -> Type const& { assert(e.type); return *e.type; },
	};
}


//==============================================================================
// Scope
//==============================================================================
Scope* Scope::new_child()
{
	children.push_back(std::make_unique<Scope>(mod, this));
	return children.back().get();
}

ScopeItem& Scope::declare(string_view name, ScopeItem &&item, TokenRange sloc)
{
	auto res = items_by_name.emplace(name, std::move(item));
	if(not res.second)
		throw_sem_error("An item with this name has already been declared: "s + name, sloc.first, mod);

	return res.first->second;
}

Var* Scope::declare_var(string_view name, IsMutable mutability, TokenRange sloc)
{
	ScopeItem &item = declare(name, Var(sloc, name, mutability), sloc);
	return &std::get<Var>(item);
}

TypeParameterVar* Scope::declare_type_var(string_view name, TokenRange sloc)
{
	ScopeItem &item = declare(name, TypeParameterVar(sloc, name), sloc);
	return &std::get<TypeParameterVar>(item);
}

void Scope::declare_struct(StructItem *struct_)
{
	declare(struct_->name, struct_, struct_->range);
}

void Scope::declare_proc(ProcItem *proc)
{
	declare(proc->name, proc, proc->range);
}

void Scope::declare_alias(AliasItem *alias)
{
	declare(alias->name, alias, alias->range);
}

ScopeItem& Scope::lookup(string_view const &name, TokenIdx sloc, bool traverse_upwards)
{
	auto it = items_by_name.find(name);
	if(it != items_by_name.end())
		return it->second;

	if(parent && traverse_upwards)
		return parent->lookup(name, sloc);

	throw_sem_error("Name has not been declared: "s + name, sloc, mod);
}


//==============================================================================
// Instance registry
//==============================================================================
static void substitute(TypeArgList &args, TypeEnv const &env, InstanceRegistry &registry);
static void substitute(Type &type, TypeEnv const &env, InstanceRegistry &registry);

bool operator == (ProcInstanceKey const &a, ProcInstanceKey const &b)
{
	if(a.proc != b.proc)
		return false;

	size_t type_arg_count = a.type_args ? a.type_args->count : 0;
	for(size_t i = 0; i < type_arg_count; ++i)
	{
		if(not equiv(a.type_args->items[i], b.type_args->items[i]))
			return false;
	}

	return true;
}

bool operator == (ProcTypeInstanceKey const &a, ProcTypeInstanceKey const &b)
{
	if(not equiv(*a.ret, *b.ret))
		return false;

	assert(a.params->count == b.params->count);
	for(size_t i = 0; i < a.params->count; ++i)
	{
		if(not equiv(a.params->items[i], b.params->items[i]))
			return false;
	}

	return true;
}


static bool gather_type_vars(Type const &type, unordered_set<VarType> &type_vars)
{
	return type | match
	{
		[&](BuiltinType const&) { return false; },
		[&](KnownIntType const&) { return false; },
		[&](PointerType const &t)
		{
			return gather_type_vars(*t.pointee, type_vars);
		},
		[&](ManyPointerType const &t)
		{
			return gather_type_vars(*t.pointee, type_vars);
		},
		[&](ProcType const&) -> bool
		{
			assert(!"gather_type_vars: ProcType");
		},
		[&](StructType const &t)
		{
			bool parent_has_type_deduction_vars = false;
			if(t.inst->parent())
				parent_has_type_deduction_vars = gather_type_vars(Type(StructType(UNKNOWN_TOKEN_RANGE, t.inst->parent())), type_vars);

			type_vars.insert(t.inst->type_args().occurring_vars.begin(), t.inst->type_args().occurring_vars.end());
			return parent_has_type_deduction_vars || t.inst->type_args().has_type_deduction_vars;
		},
		[&](UnionType const&) -> bool
		{
			assert(!"gather_type_vars: UnionType");
		},
		[&](VarType const &t)
		{
			type_vars.insert(t);
			return std::holds_alternative<TypeDeductionVar const*>(t);
		},
		[&](ProcTypeUnresolved const&) -> bool { assert(!"gather_type_vars: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> bool { assert(!"gather_type_vars: UnionTypeUnresolved"); },
		[&](Path const&) -> bool { assert(!"gather_type_vars: Path"); },
	};
}

static TypeArgList create_type_arg_list(FixedArray<Type> *NULLABLE type_args, Arena &arena)
{
	if(not type_args)
		type_args = alloc_fixed_array<Type>(0, arena);

	TypeArgList arg_list{.args = type_args};
	for(Type const &type: *type_args)
	{
		if(gather_type_vars(type, arg_list.occurring_vars))
			arg_list.has_type_deduction_vars = true;
	}

	return arg_list;
}


// `type_args` must already be resolved
StructInstance* InstanceRegistry::get_struct_instance(
	StructItem const *struct_,
	FixedArray<Type> *NULLABLE type_args,
	StructInstance *NULLABLE parent
)
{
	auto it = m_struct_instances.find(StructInstanceKey(struct_, type_args, parent));
	if(it != m_struct_instances.end())
		return &it->second;

	TypeArgList type_arg_list = create_type_arg_list(type_args, m_arena);
	return add_struct_instance(StructInstance(struct_, type_arg_list, parent, this));
}

StructInstance* InstanceRegistry::get_struct_instance(
	StructItem const *struct_,
	TypeArgList const &type_args,
	StructInstance *NULLABLE parent
)
{
	auto it = m_struct_instances.find(StructInstanceKey(struct_, type_args.args, parent));
	if(it != m_struct_instances.end())
		return &it->second;

	return add_struct_instance(StructInstance(struct_, type_args, parent, this));
}

StructInstance* InstanceRegistry::get_struct_instance(
	StructItem const *struct_,
	TypeArgList const &type_args,
	TypeEnv const &subst,
	StructInstance *NULLABLE parent
)
{
	void *original_mem_pos = m_arena.current_ptr();
	TypeArgList new_args = clone(type_args, this);
	substitute(new_args, subst, *this);

	auto it = m_struct_instances.find(StructInstanceKey(struct_, new_args.args, parent));
	if(it != m_struct_instances.end())
	{
		// Free `new_args` since we only needed it for lookup
		m_arena.set_current_ptr(original_mem_pos);
		return &it->second;
	}

	return add_struct_instance(StructInstance(struct_, new_args, parent, this));
}

StructInstance* InstanceRegistry::add_struct_instance(StructInstance &&new_inst)
{
	StructInstanceKey key = new_inst.key();
	StructInstance *inst = &m_struct_instances.emplace(key, std::move(new_inst)).first->second;
	LOGGER(m_mod.logger, on_register_struct, inst);

	for(InstanceRegistryListener *l: m_listeners)
		l->on_new_struct_instance(inst);

	return inst;
}

std::generator<StructInstance&> InstanceRegistry::struct_instances()
{
	for(auto &[_, struct_]: m_struct_instances)
		co_yield struct_;
}

std::generator<ProcInstance&> InstanceRegistry::proc_instances()
{
	for(auto &[_, proc]: m_proc_instances)
		co_yield proc;
}


// `type_args` must already be resolved and must not contain KnownIntTypes
ProcInstance* InstanceRegistry::get_proc_instance(ProcItem const *proc, FixedArray<Type> *NULLABLE type_args)
{
	auto it = m_proc_instances.find(ProcInstanceKey(proc, type_args));
	if(it != m_proc_instances.end())
		return &it->second;

	TypeArgList type_arg_list = create_type_arg_list(type_args, m_arena);
	return add_proc_instance(ProcInstance(proc, type_arg_list, this));
}

ProcInstance* InstanceRegistry::get_proc_instance(ProcItem const *proc, TypeArgList const &type_args)
{
	auto it = m_proc_instances.find(ProcInstanceKey(proc, type_args.args));
	if(it != m_proc_instances.end())
		return &it->second;

	return add_proc_instance(ProcInstance(proc, type_args, this));
}

ProcInstance* InstanceRegistry::get_proc_instance(
	ProcItem const *proc,
	TypeArgList const &type_args,
	TypeEnv const &subst
)
{
	void *original_mem_pos = m_arena.current_ptr();
	TypeArgList new_args = clone(type_args, this);
	substitute(new_args, subst, *this);

	auto it = m_proc_instances.find(ProcInstanceKey(proc, new_args.args));
	if(it != m_proc_instances.end())
	{
		// Free `new_args` since we only needed it for lookup
		m_arena.set_current_ptr(original_mem_pos);
		return &it->second;
	}

	return add_proc_instance(ProcInstance(proc, new_args, this));
}

ProcInstance* InstanceRegistry::add_proc_instance(ProcInstance &&new_inst)
{
	ProcInstanceKey key = new_inst.key();
	ProcInstance *inst = &m_proc_instances.emplace(key, std::move(new_inst)).first->second;
	LOGGER(m_mod.logger, on_register_proc, inst);

	for(InstanceRegistryListener *l: m_listeners)
		l->on_new_proc_instance(inst);

	return inst;
}


// `type_args` must already be resolved
ProcTypeInstance* InstanceRegistry::get_proc_type_instance(FixedArray<Type> *params, Type *ret)
{
	ProcTypeInstanceKey key{params, ret};
	auto it = m_proc_type_instances.find(key);
	if(it != m_proc_type_instances.end())
		return &it->second;

	return add_proc_type_instance(params, ret);
}

ProcTypeInstance* InstanceRegistry::get_proc_type_instance(
	FixedArray<Type> *params,
	Type *ret,
	TypeEnv const &subst
)
{
	void *original_mem_pos = m_arena.current_ptr();

	// Substitute params
	FixedArray<Type> *new_params = clone(params, this);
	for(Type &new_param: *new_params)
		substitute(new_param, subst, *this);

	// Substitute return type
	Type *new_ret = clone_ptr(ret, this);
	substitute(*new_ret, subst, *this);

	auto it = m_proc_type_instances.find(ProcTypeInstanceKey(new_params, new_ret));
	if(it != m_proc_type_instances.end())
	{
		// Free `new_params` and `new_ret` since we only needed them for lookup
		m_arena.set_current_ptr(original_mem_pos);
		return &it->second;
	}

	return add_proc_type_instance(new_params, new_ret);
}

ProcTypeInstance* InstanceRegistry::add_proc_type_instance(FixedArray<Type> *params, Type *ret)
{
	unordered_set<VarType> occurring_vars;
	bool has_type_deduction_vars = gather_type_vars(*ret, occurring_vars);
	for(Type const &type: *params)
	{
		if(gather_type_vars(type, occurring_vars))
			has_type_deduction_vars = true;
	}

	ProcTypeInstance *inst = &m_proc_type_instances.emplace(
		ProcTypeInstanceKey(params, ret), 
		ProcTypeInstance{
			.params = params,
			.ret = ret,
			.occurring_vars = std::move(occurring_vars),
			.has_type_deduction_vars = has_type_deduction_vars,
		}
	).first->second;

	return inst;
}


void InstanceRegistry::add_listener(InstanceRegistryListener *l)
{
	m_listeners.push_back(l);
}

void InstanceRegistry::remove_listener(InstanceRegistryListener *l)
{
	m_listeners.erase(
		std::remove(m_listeners.begin(), m_listeners.end(), l),
		m_listeners.end()
	);
}


static void create_type_env(
	FixedArray<TypeParameterVar*> const *type_params,
	TypeArgList const &type_args,
	TypeEnv &result)
{
	for(size_t i = 0; i < type_params->count; ++i)
	{
		TypeParameterVar const *type_param = type_params->items[i];
		result.add(type_param, type_args.args->items[i]);
	}
}


//--------------------------------------------------------------------
// StructInstance
//--------------------------------------------------------------------
static void substitute(Type &type, TypeEnv const &env, InstanceRegistry &registry);
static void substitute_types_in_expr(Expr &expr, TypeEnv const &env, InstanceRegistry &registry);

static Type* mk_builtin_type(BuiltinTypeDef builtin, Arena *arena);
static MemoryLayout get_layout(BuiltinTypeDef type);
MemoryLayout compute_layout(Type const &type, unordered_set<TypeInstance> *parent_type_deps);


Module const& StructInstance::mod() const
{
	return m_registry->mod();
}

TypeEnv StructInstance::create_type_env() const
{
	TypeEnv env;
	if(m_parent)
		env = m_parent->create_type_env();

	::create_type_env(m_struct->sema->type_params, m_type_args, env);

	return env;
}

bool StructInstance::is_concrete() const
{
	bool parent_concrete = m_parent ? m_parent->is_concrete() : true;
	return parent_concrete && m_type_args.occurring_vars.empty();
}

bool StructInstance::is_deduction_complete() const
{
	bool parent_is_deduction_complete = m_parent ?
		m_parent->is_deduction_complete() : true;

	return parent_is_deduction_complete and not m_type_args.has_type_deduction_vars;
}

Type* StructInstance::try_get_ctor_type()
{
	if(not m_dependent_properties_computed)
		compute_dependent_properties();

	return m_ctor_proc_type;
}

StructInstance* StructInstance::implicit_case()
{
	if(not m_dependent_properties_computed)
		compute_dependent_properties();

	return m_implicit_case;
}

int StructInstance::case_idx()
{
	assert(m_parent);

	if(not m_parent->m_dependent_properties_computed)
		m_parent->compute_dependent_properties();

	return m_case_idx;
}

std::generator<Parameter const&> StructInstance::own_var_members()
{
	if(not m_members)
		compute_dependent_properties();

	co_yield std::ranges::elements_of(initial_var_members());
	co_yield std::ranges::elements_of(trailing_var_members());
}

std::generator<Parameter&> StructInstance::initial_var_members()
{
	if(not m_members)
		compute_dependent_properties();

	for(int i = 0; i < m_struct->sema->num_initial_var_members; ++i)
		co_yield std::get<Parameter>(m_members->items[i]);
}

std::generator<StructInstance*> StructInstance::own_case_members()
{
	if(not m_members)
		compute_dependent_properties();

	int first_idx = m_struct->sema->num_initial_var_members;
	int end_idx = first_idx + m_struct->num_case_members;
	for(int i = first_idx; i < end_idx; ++i)
		co_yield std::get<StructInstance*>(m_members->items[i]);
}

std::generator<Parameter&> StructInstance::trailing_var_members()
{
	if(not m_members)
		compute_dependent_properties();

	size_t first_idx = m_struct->sema->num_initial_var_members + m_struct->num_case_members;
	for(size_t i = first_idx; i < m_members->count; ++i)
		co_yield std::get<Parameter>(m_members->items[i]);
}

std::generator<Parameter const&> StructInstance::all_var_members()
{
	if(m_parent)
		co_yield std::ranges::elements_of(m_parent->initial_var_members());

	co_yield std::ranges::elements_of(own_var_members());

	if(m_parent)
		co_yield std::ranges::elements_of(m_parent->trailing_var_members());
}


void StructInstance::compute_dependent_properties()
{
	TypeEnv env = create_type_env();

	// Initialize the struct's own members (m_members)
	m_members = alloc_fixed_array<InstanceMember>(m_struct->members->count, m_registry->arena());
	int case_idx = 0;
	for(size_t i = 0; i < m_struct->members->count; ++i)
	{
		m_struct->members->items[i] | match
		{
			[&](Parameter const &var_member)
			{
				Parameter inst_var_member{
					.range = var_member.range,
					.type = clone_ptr(var_member.type, m_registry),
					// Why ExprPending?
					// - First, when typechecking the *usage* of a struct, we only need to know
					//   whether a member *has* a default value, but it doesn't matter what that
					//   default value is. This is war ExprPending() accomplishes.
					// - Second, if we directly assigned the default value expression here, we would
					//   also need to substitute the type parameters in it. However,
					//   var_member.default_value has not been type-checked at this point, and in
					//   order to typecheck var_member.default_value we may have to typecheck the
					//   default values of other structs, which may well end up in a cycle
					.default_value = var_member.default_value ?
						DefaultValueExpr(ExprPending()) : DefaultValueExpr(NoDefaultValue()),
				};
				substitute(*inst_var_member.type, env, *m_registry);

				new (m_members->items+i) InstanceMember(inst_var_member);
			},
			[&](StructItem *case_member)
			{
				assert(case_member->type_params->count == 0);
				StructInstance *case_member_inst = m_registry->get_struct_instance(case_member, nullptr, this);
				case_member_inst->m_case_idx = case_idx;
				m_members->items[i] = InstanceMember(case_member_inst);
				if(case_member->is_implicit)
					m_implicit_case = case_member_inst;

				case_idx += 1;
			},
		};
	}

	// If the struct has a constructor, compute its ProcType (m_ctor_proc_type) and its list of
	// Parameters (m_ctor_params)
	if(m_struct->has_constructor())
	{
		size_t param_count = m_struct->sema->ctor_params->count;
		FixedArray<Type> *ctor_param_types = alloc_fixed_array<Type>(param_count, m_registry->arena());
		m_ctor_params = alloc_fixed_array<Parameter const*>(param_count, m_registry->arena());

		for(auto const &[idx, var_member]: all_var_members() | std::views::enumerate)
		{
			ctor_param_types->items[idx] = clone(*var_member.type, m_registry);
			m_ctor_params->items[idx] = &var_member;
		}

		Type *ctor_ret_type = m_registry->arena().alloc<Type>(StructType(UNKNOWN_TOKEN_RANGE, this));
		ProcTypeInstance *proc_type_inst = m_registry->get_proc_type_instance(ctor_param_types, ctor_ret_type);
		m_ctor_proc_type = m_registry->arena().alloc<Type>(ProcType{
			.range = UNKNOWN_TOKEN_RANGE,
			.inst = proc_type_inst,
			.callable = this,
		});
	}

	m_dependent_properties_computed = true;
}

size_t StructInstance::get_param_count()
{
	if(not m_ctor_params)
		compute_dependent_properties();

	return m_ctor_params->count;
}

Type* StructInstance::get_ctor_param_type_at(size_t idx)
{
	if(not m_ctor_params)
		compute_dependent_properties();

	return m_ctor_params->items[idx]->type;
}

string_view StructInstance::get_ctor_param_name_at(size_t idx)
{
	if(not m_ctor_params)
		compute_dependent_properties();

	return name_of(*m_ctor_params->items[idx], &m_registry->mod());
}

DefaultValueExpr StructInstance::get_ctor_param_default_value(size_t idx)
{
	if(not m_ctor_params)
		compute_dependent_properties();

	return m_ctor_params->items[idx]->default_value;
}

MemoryLayout StructInstance::compute_own_layout()
{
	if(m_layout_state == LayoutComputationState::DONE)
		return *m_own_layout;

	if(m_layout_state == LayoutComputationState::IN_PROGRESS)
		throw_sem_error("Cyclic type definition", m_struct->range.first, &m_registry->mod());

	LOGGER(m_registry->mod().logger, on_start_layout_computation, this);

	m_layout_state = LayoutComputationState::IN_PROGRESS;
	m_own_layout = MemoryLayout{};

	// Reserve space for the discriminator
	if(m_struct->num_case_members > 0)
	{
		BuiltinTypeDef discriminator_type = smallest_int_type_for(m_struct->num_case_members);
		m_discriminator_type = mk_builtin_type(discriminator_type, &m_registry->arena());
		m_own_layout->extend(get_layout(discriminator_type));
	}

	// Reserve space for initial variable members
	for(Parameter &var_member: initial_var_members())
	{
		MemoryLayout var_layout = compute_layout(*var_member.type, &m_type_deps);
		m_own_layout->extend(var_layout);
	}

	// Reserve space for case members
	if(m_struct->num_case_members > 0)
	{
		m_cases_layout = CaseMemberRegion{};
		m_cases_layout->start = m_own_layout->size;
		MemoryLayout case_members_layout{};
		for(StructInstance *case_member: own_case_members())
		{
			MemoryLayout case_layout = case_member->compute_own_layout();
			case_members_layout.size = std::max(case_members_layout.size, case_layout.size);
			case_members_layout.alignment = std::max(case_members_layout.alignment, case_layout.alignment);
		}
		size_t size_before_cases = m_own_layout->size;
		m_own_layout->extend(case_members_layout);
		m_cases_layout->size = m_own_layout->size - size_before_cases;
	}

	// Reserve space for trailing variable members (same as for initial variable members above)
	for(Parameter &var_member: trailing_var_members())
	{
		MemoryLayout var_layout = compute_layout(*var_member.type, &m_type_deps);
		m_own_layout->extend(var_layout);
	}

	LOGGER(m_registry->mod().logger, on_end_layout_computation);

	m_layout_state = LayoutComputationState::DONE;
	return *m_own_layout;
}

void StructInstance::finalize_typechecking()
{
	if(m_finalized)
		return;

	if(not m_dependent_properties_computed)
		compute_dependent_properties();

	if(m_parent)
		m_parent->finalize_typechecking();

	// TODO Store and re-use the `env` created in compute_dependent_properties()
	TypeEnv env = create_type_env();

	// Substitute type vars in default values
	for(size_t i = 0; i < m_struct->members->count; ++i)
	{
		if(Parameter const *var_member = std::get_if<Parameter>(&m_struct->members->items[i]))
		{
			Parameter &inst_var_member = std::get<Parameter>(m_members->items[i]);
			if(Expr *default_value = var_member->default_value.try_get_expr())
			{
				default_value = clone_ptr(default_value, m_registry);
				substitute_types_in_expr(*default_value, env, *m_registry);
				inst_var_member.default_value = default_value;
			}
		}
	}

	m_finalized = true;
}

MemoryLayout StructInstance::layout()
{
	if(m_parent)
		return m_parent->layout();

	return compute_own_layout();
}

Type const* StructInstance::discriminator_type() const
{
	assert(m_layout_state == LayoutComputationState::DONE);
	assert(m_discriminator_type);

	return m_discriminator_type;
}


bool operator == (StructInstanceKey const &a, StructInstanceKey const &b)
{
	if(a.struct_ != b.struct_)
		return false;

	if(a.parent != b.parent)
		return false;

	size_t type_arg_count = a.type_args ? a.type_args->count : 0;
	for(size_t i = 0; i < type_arg_count; ++i)
	{
		if(not equiv(a.type_args->items[i], b.type_args->items[i]))
			return false;
	}

	return true;
}


//--------------------------------------------------------------------
// ProcInstance
//--------------------------------------------------------------------
static Type* create_proc_type(ProcInstance *proc, TypeEnv const &env, InstanceRegistry &instances);

TypeEnv ProcInstance::create_type_env() const
{
	TypeEnv env;
	::create_type_env(m_proc->sema->type_params, m_type_args, env);
	return env;
}

void ProcInstance::compute_dependent_properties()
{
	TypeEnv env;
	for(size_t i = 0; i < m_proc->type_params->count; ++i)
	{
		TypeParameterVar const *type_param = m_proc->sema->type_params->items[i];
		env.add(type_param, m_type_args.args->items[i]);
	}

	m_type = create_proc_type(this, env, *m_registry);
}

size_t ProcInstance::get_param_count()
{
	return get_proc_type().inst->params->count;
}

Type* ProcInstance::get_param_type_at(size_t idx)
{
	return &get_proc_type().inst->params->items[idx];
}

string_view ProcInstance::get_param_name_at(size_t idx)
{
	return name_of(m_proc->params->items[idx], &m_registry->mod());
}

DefaultValueExpr ProcInstance::get_param_default_value(size_t idx)
{
	return m_proc->params->items[idx].default_value;
}


static Type* create_proc_type(ProcInstance *inst, TypeEnv const &env, InstanceRegistry &instances)
{
	ProcItem const *proc = inst->proc();
	FixedArray<Type> *ctor_params = alloc_fixed_array<Type>(proc->params->count, instances.arena());
	for(auto const &[idx, param]: *proc->params | std::views::enumerate)
	{
		Parameter new_param = clone(param, &instances);
		substitute(*new_param.type, env, instances);
		if(Expr *default_value = new_param.default_value.try_get_expr())
			substitute_types_in_expr(*default_value, env, instances);

		ctor_params->items[idx] = *new_param.type;
	}

	Type *new_ret = clone_ptr(proc->ret_type, &instances);
	substitute(*new_ret, env, instances);

	ProcTypeInstance *proc_type_inst = instances.get_proc_type_instance(ctor_params, new_ret);
	return instances.arena().alloc<Type>(ProcType{
		.range = UNKNOWN_TOKEN_RANGE,
		.inst = proc_type_inst,
		.callable = inst,
	});
}


//==============================================================================
// Sema context
//==============================================================================
struct DeductionContext
{
	explicit DeductionContext(Arena &arena) :
		vars(arena) {}

	TypeDeductionVar const* new_type_deduction_var()
	{
		return vars.append(TypeDeductionVar(vars.count()));
	}

	ListBuilder<TypeDeductionVar> vars;
};

// Holds all the state needed during semantic analysis
struct SemaContext
{
	SemaContext(Module &mod, Arena &arena) :
		mod(&mod),
		dc(arena),
		arena(&arena) {}

	Module *mod;
	ProcItem *NULLABLE proc = nullptr; // The current procedure being analyzed

	DeductionContext dc;
	Arena *arena;
};


//==============================================================================
// Pass 1: Declare procs, structs and aliases
//==============================================================================
static std::generator<Parameter&> initial_var_members(StructItem *struct_)
{
	for(int i = 0; i < struct_->sema->num_initial_var_members; ++i)
		co_yield std::get<Parameter>(struct_->members->items[i]);
}

static std::generator<Parameter&> trailing_var_members(StructItem *struct_)
{
	size_t first_idx = struct_->sema->num_initial_var_members + struct_->num_case_members;
	for(size_t i = first_idx; i < struct_->members->count; ++i)
		co_yield std::get<Parameter>(struct_->members->items[i]);
}

static std::generator<Parameter&> all_var_members(StructItem *struct_)
{
	if(struct_->sema->parent)
		co_yield std::ranges::elements_of(initial_var_members(struct_->sema->parent));

	co_yield std::ranges::elements_of(initial_var_members(struct_));
	co_yield std::ranges::elements_of(trailing_var_members(struct_));

	if(struct_->sema->parent)
		co_yield std::ranges::elements_of(trailing_var_members(struct_->sema->parent));
}

std::generator<StructItem*> own_case_members(StructItem *struct_)
{
	int first_idx = struct_->sema->num_initial_var_members;
	int end_idx = first_idx + struct_->num_case_members;
	for(int i = first_idx; i < end_idx; ++i)
		co_yield std::get<StructItem*>(struct_->members->items[i]);
}

static void declare_struct_item(StructItem *struct_, StructItem *NULLABLE parent, Scope *scope, SemaContext &ctx)
{
	assert(not struct_->ctor_without_parens or struct_->members->count == 0);

	scope->declare_struct(struct_);
	struct_->sema = ctx.arena->alloc<Struct>(parent, scope->new_child());

	// Declare type parameters
	size_t num_type_params = struct_->type_params->count;
	struct_->sema->type_params = alloc_fixed_array<TypeParameterVar*>(num_type_params, *ctx.arena);
	for(size_t i = 0; i < num_type_params; ++i)
	{
		TypeParameter const &type_param = struct_->type_params->items[i];
		// TODO Produce error if the type parameter has the same name as the struct
		TypeParameterVar *type_var = struct_->sema->type_scope->declare_type_var(type_param.name, type_param.range);
		struct_->sema->type_params->items[i] = type_var;
	}

	// Perform some checks on variable and case members:
	// - Check that they each have a unique name
	// - Check that there are no variable members between case members
	// - Check that case members that have been declared without braces indeed have no members, not
	//   even inherited ones
	enum MemberListState
	{
		INITIAL_VAR_MEMBERS,
		CASE_MEMBERS,
		TRAILING_VAR_MEMBERS,
	};

	std::unordered_set<string_view> member_names;
	MemberListState state = INITIAL_VAR_MEMBERS;
	for(Member const &member: *struct_->members)
	{
		member | match
		{
			[&](Parameter const &var_member)
			{
				if(state == INITIAL_VAR_MEMBERS)
					struct_->sema->num_initial_var_members += 1;

				if(state == CASE_MEMBERS)
					state = TRAILING_VAR_MEMBERS;

				bool inserted = member_names.insert(name_of(var_member, ctx.mod)).second;
				if(not inserted)
					throw_sem_error("A member with this name has already been declared", var_member.range.first, ctx.mod);
			},
			[&](StructItem *case_member)
			{
				if(state == INITIAL_VAR_MEMBERS)
					state = CASE_MEMBERS;
				else if(state != CASE_MEMBERS)
					throw_sem_error("Variable members must come before or after all case members", case_member->range.first, ctx.mod);

				bool inserted = member_names.insert(case_member->name).second;
				if(not inserted)
					throw_sem_error("A member with this name has already been declared", case_member->range.first, ctx.mod);

				declare_struct_item(case_member, struct_, struct_->sema->type_scope, ctx);

				if(case_member->ctor_without_parens and struct_->num_var_members())
					throw_sem_error("Struct must have braces because it inherits members from parent", case_member->range.first, ctx.mod);
			},
		};
	}

	// In the struct has a constructor, create an array with all the constructor parameters, i.e.,
	// all the variable members.
	if(struct_->has_constructor())
	{
		size_t param_count = 0;
		for(StructItem *cur_item = struct_; cur_item; cur_item = cur_item->sema->parent)
			param_count += cur_item->num_var_members();

		struct_->sema->ctor_params = alloc_fixed_array<Parameter const*>(param_count, *ctx.arena);
		for(auto const &[idx, var_member]: all_var_members(struct_) | std::views::enumerate)
			struct_->sema->ctor_params->items[idx] = &var_member;
	}
}

static void declare_items(SemaContext &ctx)
{
	Module *mod = ctx.mod;
	mod->sema = std::make_unique<SemaModule>(std::make_unique<Scope>(ctx.mod), *ctx.arena);

	for(TopLevelItem &item: to_range(ctx.mod->items))
	{
		item | match
		{
			[&](ProcItem &proc)
			{
				proc.sema = ctx.arena->alloc<Proc>(mod->sema->globals->new_child());
				proc.sema->param_vars = alloc_fixed_array<Var*>(proc.params->count, *ctx.arena);
				for(auto const& [idx, param]: *proc.params | std::views::enumerate)
				{
					Var *param_var = proc.sema->scope->declare_var(name_of(param, mod), IsMutable::NO, param.range);
					proc.sema->param_vars->items[idx] = param_var;
				}

				size_t num_type_params = proc.type_params->count;
				proc.sema->type_params = alloc_fixed_array<TypeParameterVar*>(num_type_params, *ctx.arena);
				for(size_t i = 0; i < num_type_params; ++i)
				{
					TypeParameter const &type_param = proc.type_params->items[i];
					TypeParameterVar *type_var = proc.sema->scope->declare_type_var(type_param.name, type_param.range);
					proc.sema->type_params->items[i] = type_var;
				}

				mod->sema->globals->declare_proc(&proc);
			},
			[&](StructItem &struct_)
			{
				declare_struct_item(&struct_, nullptr, mod->sema->globals.get(), ctx);
			},
			[&](AliasItem &alias)
			{
				alias.sema = ctx.arena->alloc<Alias>(mod->sema->globals->new_child());

				size_t num_type_params = alias.type_params->count;
				alias.sema->type_params = alloc_fixed_array<TypeParameterVar*>(num_type_params, *ctx.arena);
				for(size_t i = 0; i < num_type_params; ++i)
				{
					TypeParameter const &type_param = alias.type_params->items[i];
					TypeParameterVar *type_var = alias.sema->scope->declare_type_var(type_param.name, type_param.range);
					alias.sema->type_params->items[i] = type_var;
				}

				mod->sema->globals->declare_alias(&alias);
			},
		};
	}
}



//==============================================================================
// Pass 2: Path resolution
//==============================================================================
static void substitute(Type &type, TypeEnv const &env, InstanceRegistry &registry);
static void resolve_type(Type &type, Scope *scope, SemaContext &ctx);
static void resolve_expr(Expr &expr, Scope *scope, SemaContext &ctx);
static void resolve_alias(AliasItem &alias, SemaContext &ctx);

static Type resolve_path_to_type(Path const &path, Scope *scope, SemaContext &ctx);
static Expr resolve_path_to_expr(Path const &path, Scope *scope, SemaContext &ctx);

static FixedArray<Type>* resolve_type_args(
	FixedArray<Type> *args,
	size_t num_type_params,
	Scope *scope,
	SemaContext &ctx
)
{
	FixedArray<Type> *resolved_type_args = alloc_fixed_array<Type>(num_type_params, *ctx.arena);
	size_t cur_idx = 0;
	for(Type const &arg: *args)
	{
		resolved_type_args->items[cur_idx] = arg;
		resolve_type(resolved_type_args->items[cur_idx], scope, ctx);
		++cur_idx;
	}

	assert(args->count <= num_type_params);
	size_t num_missing_args = num_type_params - args->count;
	while(num_missing_args--)
		new (&resolved_type_args->items[cur_idx++]) Type(VarType(ctx.dc.new_type_deduction_var()));

	return resolved_type_args;
}


struct NoParent {};
using PathParent = variant<NoParent, StructInstance*>;

static StructInstance* try_get_struct(PathParent parent)
{
	return parent | match
	{
		[&](NoParent) { return (StructInstance*)nullptr; },
		[&](StructInstance *inst) { return inst; },
	};
}


static variant<Expr, Type> resolve_path(Path const &path, PathParent parent, Scope *scope, SemaContext &ctx)
{
	ScopeItem *resolved_item = parent | match
	{
		[&](NoParent)
		{
			string_view name = name_of(path, ctx.mod);
			if(name == "?")
				name = "Option";

			return &scope->lookup(name, path.range.first);
		},
		[&](StructInstance *parent_inst)
		{
			Scope *parent_scope = parent_inst->struct_()->sema->type_scope;
			return &parent_scope->lookup(name_of(path, ctx.mod), path.range.first, false);
		},
	};

	return *resolved_item | match
	{
		[&](StructItem *struct_) -> variant<Expr, Type>
		{
			size_t num_type_params = struct_->sema->type_params->count;
			if(path.type_args->count > num_type_params)
				throw_sem_error("Too many type arguments", path.range.first, ctx.mod);

			FixedArray<Type> *resolved_type_args = resolve_type_args(path.type_args, num_type_params, scope, ctx);
			StructInstance *inst = ctx.mod->sema->insts.get_struct_instance(struct_, resolved_type_args, try_get_struct(parent));

			if(path.child)
				return resolve_path(*path.child, inst, scope, ctx);

			return StructType(path.range, inst);
		},
		[&](ProcItem *proc) -> variant<Expr, Type>
		{
			if(path.child)
				throw_sem_error("Procedures cannot have any members", path.range.first, ctx.mod);

			size_t num_type_params = proc->sema->type_params->count;
			if(path.type_args->count > num_type_params)
				throw_sem_error("Too many type arguments", path.range.first, ctx.mod);

			FixedArray<Type> *resolved_type_args = resolve_type_args(path.type_args, num_type_params, scope, ctx);

			ProcInstance *inst = ctx.mod->sema->insts.get_proc_instance(proc, resolved_type_args);;
			return ProcExpr(path.range, inst);
		},
		[&](AliasItem *alias) -> variant<Expr, Type>
		{
			assert(is<NoParent>(parent));

			resolve_alias(*alias, ctx);

			size_t num_type_params = alias->sema->type_params->count;
			if(path.type_args->count > num_type_params)
				throw_sem_error("Too many type arguments", path.range.first, ctx.mod);

			FixedArray<Type> *resolved_type_args = resolve_type_args(path.type_args, num_type_params, scope, ctx);
			TypeEnv env;
			size_t arg_idx = 0;
			for(Type const &arg: *resolved_type_args)
			{
				env.add(alias->sema->type_params->items[arg_idx], clone(arg, &ctx.mod->sema->insts));
				arg_idx += 1;
			}
			while(arg_idx < num_type_params)
			{
				env.add(alias->sema->type_params->items[arg_idx], ctx.dc.new_type_deduction_var());
				arg_idx += 1;
			}

			Type type = clone(*alias->aliased_type, &ctx.mod->sema->insts);
			substitute(type, env, ctx.mod->sema->insts);

			if(path.child)
			{
				StructType const *struct_type = std::get_if<StructType>(&type);
				if(not struct_type)
					throw_sem_error("Type has no members", path.range.first, ctx.mod);

				return resolve_path(*path.child, struct_type->inst, scope, ctx);
			}

			return type;
		},
		[&](TypeParameterVar const &type_var) -> variant<Expr, Type>
		{
			if(path.type_args->count)
				throw_sem_error("Cannot apply type arguments to type variable", path.range.first, ctx.mod);

			if(path.child)
				throw_sem_error("Member selection of type parameters not supported", path.child->range.first, ctx.mod);

			return VarType(&type_var);
		},
		[&](Var const &var) -> variant<Expr, Type>
		{
			assert(is<NoParent>(parent));

			if(path.type_args->count)
				throw_sem_error("Cannot apply type arguments to variable", path.range.first, ctx.mod);

			Expr result = VarExpr(path.range, &var);
			Path const *child = path.child;
			while(child)
			{
				if(child->type_args->count)
					throw_sem_error("Cannot apply type arguments to field", child->range.first, ctx.mod);

				result = MemberAccessExpr{
					.range = {token_range_of(result).first, child->range.last},
					.object = ctx.arena->alloc<Expr>(result),
					.member = name_of(*child, ctx.mod)
				};

				child = child->child;
			}

			return result;
		},
	};
}

static Type resolve_path_to_type(Path const &path, Scope *scope, SemaContext &ctx)
{
	variant<Expr, Type> resolved_path = resolve_path(path, NoParent(), scope, ctx);
	return resolved_path | match
	{
		[&](Type const &type)
		{
			return type;
		},
		[&](Expr const&) -> Type
		{
			throw_sem_error("Expected type, got expression", path.range.first, ctx.mod);
		},
	};
}

static Expr resolve_path_to_expr(Path const &path, Scope *scope, SemaContext &ctx)
{
	variant<Expr, Type> resolved_path = resolve_path(path, NoParent(), scope, ctx);
	return resolved_path | match
	{
		[&](Type const &type) -> Expr
		{
			Expr expr = ConstructorExpr{
				.range = path.range,
				.ctor = ctx.arena->alloc<Type>(type),
			};

			if(StructType const *struct_type = std::get_if<StructType>(&type))
			{
				if(struct_type->inst->struct_()->ctor_without_parens)
				{
					expr = CallExpr{
						.range = token_range_of(expr),
						.callable = ctx.arena->alloc<Expr>(expr),
						.args = alloc_fixed_array<Argument>(0, *ctx.arena),
					};
				}
			}

			return expr;
		},
		[&](Expr const &expr)
		{
			return expr;
		},
	};
}

static void resolve_type(Type &type, Scope *scope, SemaContext &ctx)
{
	type | match
	{
		[&](BuiltinType&) {},
		[&](KnownIntType&) {},
		[&](PointerType &t)
		{
			resolve_type(*t.pointee, scope, ctx);
		},
		[&](ManyPointerType &t)
		{
			resolve_type(*t.pointee, scope, ctx);
		},
		[&](ProcTypeUnresolved &t)
		{
			for(Type &param: *t.params)
				resolve_type(param, scope, ctx);

			resolve_type(*t.ret, scope, ctx);

			assert(!"[TODO] resolve_type: ProcTypeUnresolved");
			//mod.sema->inst_reg.get_proc_type_instance
		},
		[&](UnionTypeUnresolved&)
		{
			assert(!"[TODO] resolve_type: UnionTypeUnresolved");
		},
		[&](Path &path)
		{
			type = resolve_path_to_type(path, scope, ctx);
		},
		[&](StructType const&) {},
		[&](ProcType const&) {},
		[&](UnionType const&) {},
		[&](VarType const&) {},
	};
}

static void resolve_expr(Expr &expr, Scope *scope, SemaContext &ctx)
{
	expr | match
	{
		[&](IntLiteralExpr const&) {},
		[&](BoolLiteralExpr const&) {},
		[&](StringLiteralExpr const&) {},
		[&](UnaryExpr const &e)
		{
			resolve_expr(*e.sub, scope, ctx);
		},
		[&](BinaryExpr const &e)
		{
			resolve_expr(*e.left, scope, ctx);
			resolve_expr(*e.right, scope, ctx);
		},
		[&](AddressOfExpr const &e)
		{
			resolve_expr(*e.object, scope, ctx);
		},
		[&](DerefExpr const &e)
		{
			resolve_expr(*e.addr, scope, ctx);
		},
		[&](IndexExpr const &e)
		{
			resolve_expr(*e.addr, scope, ctx);
			resolve_expr(*e.index, scope, ctx);
		},
		[&](MemberAccessExpr const &e)
		{
			resolve_expr(*e.object, scope, ctx);
		},
		[&](AssignmentExpr const &e)
		{
			resolve_expr(*e.lhs, scope, ctx);
			resolve_expr(*e.rhs, scope, ctx);
		},
		[&](AsExpr const &e)
		{
			resolve_expr(*e.src_expr, scope, ctx);
			resolve_type(*e.target_type, scope, ctx);
		},
		[&](ConstructorExpr const &e)
		{
			resolve_type(*e.ctor, scope, ctx);
		},
		[&](ProcExpr const&) {},
		[&](CallExpr const &e)
		{
			resolve_expr(*e.callable, scope, ctx);

			for(Argument &arg: *e.args)
				resolve_expr(arg.expr, scope, ctx);
		},
		[&](SizeOfExpr const &e)
		{
			resolve_type(*e.subject, scope, ctx);
		},
		[&](MakeExpr const &e)
		{
			resolve_expr(*e.addr, scope, ctx);
			resolve_expr(*e.init, scope, ctx);
		},
		[&](Path const &p)
		{
			expr = resolve_path_to_expr(p, scope, ctx);
		},
		[&](VarExpr const&) {},
	};
}

static void resolve_pattern(Pattern &pattern, Scope *scope, SemaContext &ctx)
{
	pattern | match
	{
		[&](VarPatternUnresolved const &p)
		{
			Var *var = scope->declare_var(p.name, p.mutability, p.range);
			pattern = Pattern(VarPattern(p.range, var), pattern.provided_type);
		},
		[&](VarPattern const&) {},
		[&](DerefPattern const &p)
		{
			resolve_pattern(*p.sub, scope, ctx);
		},
		[&](AddressOfPattern const &p)
		{
			resolve_pattern(*p.sub, scope, ctx);
		},
		[&](ConstructorPattern const &p)
		{
			resolve_type(*p.ctor, scope, ctx);
			for(PatternArgument &arg: *p.args)
				resolve_pattern(arg.pattern, scope, ctx);
		},
		[&](WildcardPattern const&) {}
	};

	if(pattern.provided_type)
		resolve_type(*pattern.provided_type, scope, ctx);
}

static void resolve_stmt(Stmt &stmt, Scope *scope, SemaContext &ctx)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			// Resolve init expr before the lhs pattern so the init expr cannot access the declared
			// variable
			if(s.init_expr)
				resolve_expr(*s.init_expr, scope, ctx);

			resolve_pattern(*s.lhs, scope, ctx);
		},
		[&](ExprStmt const &s)
		{
			resolve_expr(*s.expr, scope, ctx);
		},
		[&](BlockStmt const &s)
		{
			Scope *block_scope = scope->new_child();
			for(Stmt &child_stmt: *s.stmts)
				resolve_stmt(child_stmt, block_scope, ctx);
		},
		[&](ReturnStmt const &s)
		{
			resolve_expr(*s.ret_expr, scope, ctx);
		},
		[&](IfStmt const &s)
		{
			resolve_expr(*s.condition, scope, ctx);
			resolve_stmt(*s.then, scope, ctx);
			if(s.else_)
				resolve_stmt(*s.else_, scope, ctx);
		},
		[&](WhileStmt const &s)
		{
			resolve_expr(*s.condition, scope, ctx);
			resolve_stmt(*s.body, scope, ctx);
		},
		[&](MatchStmt const &s)
		{
			resolve_expr(*s.expr, scope, ctx);
			for(MatchArm &arm: *s.arms)
			{
				Scope *arm_scope = scope->new_child();
				resolve_pattern(arm.capture, arm_scope, ctx);
				resolve_stmt(arm.stmt, arm_scope, ctx);
			}
		},
	};
}

static void resolve_param(Parameter &param, Scope *scope, SemaContext &ctx)
{
	if(param.type)
		resolve_type(*param.type, scope, ctx);

	if(Expr *default_value = param.default_value.try_get_expr())
		resolve_expr(*default_value, scope, ctx);
}

static void resolve_struct(StructItem &struct_, SemaContext &ctx)
{
	for(Member &member: *struct_.members)
	{
		member | match
		{
			[&](Parameter &var_member)
			{
				assert(var_member.type);
				resolve_param(var_member, struct_.sema->type_scope, ctx);
			},
			[&](StructItem *case_member)
			{
				resolve_struct(*case_member, ctx);
			},
		};
	}
}

static void resolve_alias(AliasItem &alias, SemaContext &ctx)
{
	if(alias.sema->resolution_state == ResolutionState::DONE)
		return;

	if(alias.sema->resolution_state == ResolutionState::IN_PROGRESS)
		throw_sem_error("Alias is defined recursively", alias.range.first, ctx.mod);

	alias.sema->resolution_state = ResolutionState::IN_PROGRESS;
	resolve_type(*alias.aliased_type, alias.sema->scope, ctx);
	alias.sema->resolution_state = ResolutionState::DONE;
}

static void resolve_names(SemaContext &ctx)
{
	for(TopLevelItem &item: to_range(ctx.mod->items))
	{
		item | match
		{
			[&](ProcItem &proc)
			{
				for(Parameter &param: *proc.params)
					resolve_param(param, proc.sema->scope, ctx);

				resolve_type(*proc.ret_type, proc.sema->scope, ctx);

				if(proc.body)
					resolve_stmt(*proc.body, proc.sema->scope, ctx);
			},
			[&](StructItem &struct_)
			{
				resolve_struct(struct_, ctx);
			},
			[&](AliasItem &alias)
			{
				resolve_alias(alias, ctx);
			},
		};
	}
}


//--------------------------------------------------------------------
// Type substitution
//--------------------------------------------------------------------
static bool have_common_vars(unordered_set<VarType> const &occurring_vars, TypeEnv const &env)
{
	for(VarType const &var: occurring_vars)
	{
		if(env.try_lookup(var))
			return true;
	}

	return false;
}

static void substitute(Type &type, TypeEnv const &env, InstanceRegistry &registry);

static void substitute(TypeArgList &args, TypeEnv const &env, InstanceRegistry &registry)
{
	for(size_t i = 0; i < args.args->count; ++i)
		substitute(args.args->items[i], env, registry);

	// Update args.occurring_vars and args.has_type_deduction_vars
	args.occurring_vars.clear();
	args.has_type_deduction_vars = false;
	for(Type const &type: *args.args)
	{
		if(gather_type_vars(type, args.occurring_vars))
			args.has_type_deduction_vars = true;
	}
}

// If `inst` is deduction complete, apply `env` to it directly and return `inst`.
// Otherwise, leave `inst` unchanged and return the StructInstance that corresponds to `inst` where
// `env` has been applied to the type args of `inst`
static StructInstance* substitute_types_in_struct(
	StructInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	bool *modified = nullptr
)
{
	LOGGER(registry.mod().logger, on_struct_substitution_start, inst, env);

	StructInstance *new_parent = nullptr;
	bool parent_modified = false;
	if(inst->parent())
		new_parent = substitute_types_in_struct(inst->parent(), env, registry, &parent_modified);

	if(parent_modified or have_common_vars(inst->type_args().occurring_vars, env))
	{
		inst = registry.get_struct_instance(inst->struct_(), inst->type_args(), env, new_parent);

		LOGGER(registry.mod().logger, on_struct_substitution_replaced, inst);
		if(modified) *modified = true;
	}
	else
	{
		LOGGER(registry.mod().logger, on_struct_substitution_noop);
		if(modified) *modified = false;
	}

	LOGGER(registry.mod().logger, on_struct_substitution_end);

	return inst;
}

static ProcInstance* substitute_types_in_proc(
	ProcInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry
)
{
	if(have_common_vars(inst->type_args().occurring_vars, env))
		inst = registry.get_proc_instance(inst->proc(), inst->type_args(), env);

	return inst;
}

static ProcTypeInstance* substitute_types_in_proc_type(
	ProcTypeInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry
)
{
	if(have_common_vars(inst->occurring_vars, env))
		inst = registry.get_proc_type_instance(inst->params, inst->ret, env);

	return inst;
}

static void substitute(Type &type, TypeEnv const &env, InstanceRegistry &registry)
{
	type | match
	{
		[&](BuiltinType&) {},
		[&](KnownIntType&) {},
		[&](PointerType &t)
		{
			substitute(*t.pointee, env, registry);
		},
		[&](ManyPointerType &t)
		{
			substitute(*t.pointee, env, registry);
		},
		[&](StructType &t)
		{
			t.inst = substitute_types_in_struct(t.inst, env, registry);
		},
		[&](ProcType &t)
		{
			t.inst = substitute_types_in_proc_type(t.inst, env, registry);
			t.callable | match
			{
				[&](ProcInstance *proc)
				{
					t.callable = substitute_types_in_proc(proc, env, registry);
				},
				[&](StructInstance *struct_)
				{
					t.callable = substitute_types_in_struct(struct_, env, registry);
				},
			};
		},
		[&](UnionType &)
		{
			assert(!"[TODO] substitute: UnionType");
		},
		[&](VarType &t)
		{
			if(Type const *mapped_type = env.try_lookup(t))
			{
				type = clone(*mapped_type, &registry);

				// Apply substitution recursively.
				// This is not how substitution is traditionally defined for e.g. first-order logic,
				// but is easier to implement. See note for the unify() function.
				substitute(type, env, registry);

				if(KnownIntType const *known_int = std::get_if<KnownIntType>(&type))
					type = materialize_known_int(*known_int);
			}
		},
		[&](ProcTypeUnresolved const&) { UNREACHABLE; },
		[&](UnionTypeUnresolved const&) { UNREACHABLE; },
		[&](Path const&) { UNREACHABLE; },
	};
}

static void substitute_types_in_expr(Expr &expr, TypeEnv const &env, InstanceRegistry &registry)
{
	if(Type *expr_type = type_of(expr))
		substitute(*expr_type, env, registry);

	expr | match
	{
		[&](IntLiteralExpr&) {},
		[&](BoolLiteralExpr&) {},
		[&](StringLiteralExpr&) {},
		[&](UnaryExpr &e)
		{
			substitute_types_in_expr(*e.sub, env, registry);
		},
		[&](BinaryExpr &e)
		{
			substitute_types_in_expr(*e.left, env, registry);
			substitute_types_in_expr(*e.right, env, registry);
		},
		[&](AddressOfExpr &e)
		{
			substitute_types_in_expr(*e.object, env, registry);
		},
		[&](DerefExpr &e)
		{
			substitute_types_in_expr(*e.addr, env, registry);
		},
		[&](IndexExpr &e)
		{
			substitute_types_in_expr(*e.addr, env, registry);
			substitute_types_in_expr(*e.index, env, registry);
		},
		[&](MemberAccessExpr &e)
		{
			substitute_types_in_expr(*e.object, env, registry);
		},
		[&](AssignmentExpr &e)
		{
			substitute_types_in_expr(*e.lhs, env, registry);
			substitute_types_in_expr(*e.rhs, env, registry);
		},
		[&](AsExpr &e)
		{
			substitute_types_in_expr(*e.src_expr, env, registry);
			substitute(*e.target_type, env, registry);
		},
		[&](ConstructorExpr &e)
		{
			substitute(*e.ctor, env, registry);
		},
		[&](ProcExpr &e)
		{
			e.inst = substitute_types_in_proc(e.inst, env, registry);
		},
		[&](CallExpr &e)
		{
			substitute_types_in_expr(*e.callable, env, registry);

			for(Argument &arg: *e.args)
				substitute_types_in_expr(arg.expr, env, registry);
		},
		[&](SizeOfExpr &e)
		{
			substitute(*e.subject, env, registry);
		},
		[&](MakeExpr&)
		{
			assert(!"[TODO] substitute_types_in_expr: MakeExpr");
		},
		[&](VarExpr&) {},
		[&](Path&) { assert(!"substitute_types_in_expr: Path"); },
	};
}

static void substitute_types_in_pattern(Pattern &pattern, TypeEnv const &subst, InstanceRegistry &registry)
{
	substitute(type_of(pattern), subst, registry);

	if(pattern.provided_type)
		substitute(*pattern.provided_type, subst, registry);

	pattern | match
	{
		[&](VarPattern &p)
		{
			substitute(*p.var->type, subst, registry);
		},
		[&](DerefPattern &p)
		{
			substitute_types_in_pattern(*p.sub, subst, registry);
		},
		[&](AddressOfPattern &p)
		{
			substitute_types_in_pattern(*p.sub, subst, registry);
		},
		[&](ConstructorPattern &p)
		{
			substitute(*p.ctor, subst, registry);
			for(PatternArgument &arg: *p.args)
				substitute_types_in_pattern(arg.pattern, subst, registry);
		},
		[&](WildcardPattern &)
		{
			assert(!"[TODO] substitute_types_in_pattern: WildcardPattern");
		},
		[&](VarPatternUnresolved&) { assert(!"substitute_types_in_pattern: VarPatternUnresolved"); },
	};
}

void substitute_types_in_stmt(Stmt &stmt, TypeEnv const &subst, InstanceRegistry &registry)
{
	stmt | match
	{
		[&](LetStmt &s)
		{
			substitute_types_in_pattern(*s.lhs, subst, registry);
			substitute_types_in_expr(*s.init_expr, subst, registry);
		},
		[&](ExprStmt &s)
		{
			substitute_types_in_expr(*s.expr, subst, registry);
		},
		[&](BlockStmt &s)
		{
			for(Stmt &child_stmt: *s.stmts)
				substitute_types_in_stmt(child_stmt, subst, registry);
		},
		[&](ReturnStmt &s)
		{
			substitute_types_in_expr(*s.ret_expr, subst, registry);
		},
		[&](IfStmt &s)
		{
			substitute_types_in_expr(*s.condition, subst, registry);
			substitute_types_in_stmt(*s.then, subst, registry);
			if(s.else_)
				substitute_types_in_stmt(*s.else_, subst, registry);
		},
		[&](WhileStmt &s)
		{
			substitute_types_in_expr(*s.condition, subst, registry);
			substitute_types_in_stmt(*s.body, subst, registry);
		},
		[&](MatchStmt &s)
		{
			substitute_types_in_expr(*s.expr, subst, registry);
			for(MatchArm &arm: *s.arms)
			{
				substitute_types_in_pattern(arm.capture, subst, registry);
				substitute_types_in_stmt(arm.stmt, subst, registry);
			}
		},
	};
}


//==============================================================================
// Pass 3: Type checking
//==============================================================================

//--------------------------------------------------------------------
// Type properties
//--------------------------------------------------------------------
static bool is_builtin_int_type(BuiltinTypeDef type)
{
	switch(type)
	{
		case BuiltinTypeDef::NEVER: return false;
		case BuiltinTypeDef::UNIT:  return false;
		case BuiltinTypeDef::BOOL:  return false;
		case BuiltinTypeDef::I8:   return true;
		case BuiltinTypeDef::U8:   return true;
		case BuiltinTypeDef::I32:   return true;
		case BuiltinTypeDef::U32:   return true;
		case BuiltinTypeDef::ISIZE: return true;
		case BuiltinTypeDef::USIZE: return true;
	}

	UNREACHABLE;
}

static bool is_integer_type(Type const &type)
{
	return type | match
	{
		[&](BuiltinType const &t) { return is_builtin_int_type(t.builtin); },
		[&](KnownIntType const&) { return true; },
		[&](auto const&) { return false; },
	};
}

static bool is_signed(BuiltinTypeDef type)
{
	assert(is_builtin_int_type(type));

	switch(type)
	{
		case BuiltinTypeDef::NEVER: assert(!"is_signed: NEVER");
		case BuiltinTypeDef::UNIT:  assert(!"is_signed: UNIT");
		case BuiltinTypeDef::BOOL:  assert(!"is_signed: BOOL");
		case BuiltinTypeDef::I8:  return true;
		case BuiltinTypeDef::U8:  return false;
		case BuiltinTypeDef::I32:   return true;
		case BuiltinTypeDef::U32:   return false;
		case BuiltinTypeDef::ISIZE: return true;
		case BuiltinTypeDef::USIZE: return false;
	}

	UNREACHABLE;
}

static MemoryLayout get_layout(BuiltinTypeDef type)
{
	switch(type)
	{
		case BuiltinTypeDef::NEVER: return {.size = 0, .alignment = 0};
		case BuiltinTypeDef::UNIT:  return {.size = 1, .alignment = 1};
		case BuiltinTypeDef::BOOL:  return {.size = 1, .alignment = 1};
		case BuiltinTypeDef::I8:    return {.size = 1, .alignment = 1};
		case BuiltinTypeDef::U8:    return {.size = 1, .alignment = 1};
		case BuiltinTypeDef::I32:   return {.size = 4, .alignment = 4};
		case BuiltinTypeDef::U32:   return {.size = 4, .alignment = 4};
		case BuiltinTypeDef::ISIZE: return {.size = 8, .alignment = 8};
		case BuiltinTypeDef::USIZE: return {.size = 8, .alignment = 8};
	}

	UNREACHABLE;
}

static size_t size_of(BuiltinTypeDef type)
{
	return get_layout(type).size;
}

static bool builtin_losslessly_convertible(BuiltinTypeDef dest, BuiltinTypeDef src)
{
	if(!is_builtin_int_type(dest) || !is_builtin_int_type(src))
		return dest == src;

	bool dest_is_signed = is_signed(dest);
	bool src_is_signed = is_signed(src);
	if(dest_is_signed)
	{
		if(src_is_signed)
			// signed <- signed
			return size_of(dest) >= size_of(src);

		// signed <- unsigned
		return size_of(dest) >= size_of(src) + 1;
	}
	else
	{
		if(src_is_signed)
			// unsigned <- signed
			return false;

		// unsigned <- unsigned
		return size_of(dest) >= size_of(src);
	}
}

static bool integer_assignable_to(BuiltinTypeDef type, Int128 val)
{
	switch(type)
	{
		case BuiltinTypeDef::NEVER: assert(!"integer_assignable_to: NEVER");
		case BuiltinTypeDef::UNIT:  assert(!"integer_assignable_to: UNIT");
		case BuiltinTypeDef::BOOL:  assert(!"integer_assignable_to: BOOL");
		case BuiltinTypeDef::I8:    return -128 <= val && val <= 127;
		case BuiltinTypeDef::U8:    return 0 <= val && val <= 255;
		case BuiltinTypeDef::I32:   return std::numeric_limits<int32_t>::min() <= val && val <= std::numeric_limits<int32_t>::max();
		case BuiltinTypeDef::U32:   return 0 <= val && val <= std::numeric_limits<uint32_t>::max();
		case BuiltinTypeDef::ISIZE: return std::numeric_limits<int64_t>::min() <= val && val <= std::numeric_limits<int64_t>::max();
		case BuiltinTypeDef::USIZE: return 0 <= val && val <= std::numeric_limits<uint64_t>::max();
		default: UNREACHABLE;
	}
}


static optional<Type> common_int_type(Type const &a, Type const &b)
{
	assert(is_integer_type(a));
	assert(is_integer_type(b));

	BuiltinType const *a_bt = std::get_if<BuiltinType>(&a);
	BuiltinType const *b_bt = std::get_if<BuiltinType>(&b);
	KnownIntType const *a_known = std::get_if<KnownIntType>(&a);
	KnownIntType const *b_known = std::get_if<KnownIntType>(&b);

	if(a_bt && b_bt)
	{
		if(a_bt->builtin == b_bt->builtin)
			return *a_bt;

		if(builtin_losslessly_convertible(a_bt->builtin, b_bt->builtin))
			return *a_bt;

		if(builtin_losslessly_convertible(b_bt->builtin, a_bt->builtin))
			return *b_bt;
	}
	else if(a_bt && b_known)
	{
		if(
			integer_assignable_to(a_bt->builtin, b_known->low) and
			integer_assignable_to(a_bt->builtin, b_known->high)
		)
			return *a_bt;
	}
	else if(a_known && b_bt)
	{
		if(
			integer_assignable_to(b_bt->builtin, a_known->low) and
			integer_assignable_to(b_bt->builtin, a_known->high)
		)
			return *b_bt;
	}
	else
	{
		Int128 low = a_known->low < b_known->low ? a_known->low : b_known->low;
		Int128 high = a_known->high > b_known->high ? a_known->high : b_known->high;
		return Type(KnownIntType(low, high));
	}

	return nullopt;
}


struct AbstractInt
{
	BuiltinTypeDef smallest_type;
	bool is_negative;
};

static AbstractInt get_abstract_int(Int128 val)
{
	if(val < 0)
	{
		if(std::numeric_limits<int8_t>::min() <= val)
			return AbstractInt{BuiltinTypeDef::I8, true};

		if(std::numeric_limits<int32_t>::min() <= val)
			return AbstractInt{BuiltinTypeDef::I32, true};

		return AbstractInt{BuiltinTypeDef::ISIZE, true};
	}
	else
	{
		if(val <= std::numeric_limits<int8_t>::max())
			return AbstractInt{BuiltinTypeDef::I8, false};

		if(val <= std::numeric_limits<uint8_t>::max())
			return AbstractInt{BuiltinTypeDef::U8, false};

		if(val <= std::numeric_limits<int32_t>::max())
			return AbstractInt{BuiltinTypeDef::I32, false};

		if(val <= std::numeric_limits<uint32_t>::max())
			return AbstractInt{BuiltinTypeDef::U32, false};

		if(val <= std::numeric_limits<int64_t>::max())
			return AbstractInt{BuiltinTypeDef::ISIZE, false};

		return AbstractInt{BuiltinTypeDef::USIZE, false};
	}
}

static BuiltinTypeDef get_int_type_for(size_t size, bool is_signed)
{
	switch(size)
	{
		case 1:
			return is_signed ? BuiltinTypeDef::I8 : BuiltinTypeDef::U8;

		case 2:
		case 4:
			return is_signed ? BuiltinTypeDef::I32 : BuiltinTypeDef::U32;

		case 8:
			return is_signed ? BuiltinTypeDef::ISIZE : BuiltinTypeDef::USIZE;

		default: UNREACHABLE;
	}
}

BuiltinTypeDef smallest_int_type_for(Int128 low, Int128 high)
{
	AbstractInt low_abs = get_abstract_int(low);
	AbstractInt high_abs = get_abstract_int(high);

	if(low_abs.smallest_type == high_abs.smallest_type)
		return low_abs.smallest_type;

	if(low_abs.is_negative == high_abs.is_negative)
	{
		size_t required_size = std::max(
			get_layout(low_abs.smallest_type).size,
			get_layout(high_abs.smallest_type).size
		);

		bool use_signed_type = is_signed(low_abs.smallest_type) and is_signed(high_abs.smallest_type);
		return get_int_type_for(required_size, use_signed_type);
	}
	else
	{
		size_t required_size = std::max(
			get_layout(low_abs.smallest_type).size,
			get_layout(high_abs.smallest_type).size
		) * 2;

		return get_int_type_for(required_size, true);
	}
}

BuiltinTypeDef smallest_int_type_for(Int128 value)
{
	return smallest_int_type_for(value, value);
}


bool equiv(Type const &a, Type const &b)
{
	if(a.index() != b.index())
		return false;

	return a | match
	{
		[&](BuiltinType const &ta)
		{
			BuiltinType const &tb = std::get<BuiltinType>(b);
			return ta.builtin == tb.builtin;
		},
		[&](KnownIntType const &ta)
		{
			KnownIntType const &tb = std::get<KnownIntType>(b);
			return ta.low == tb.low && ta.high == tb.high;
		},
		[&](VarType const &ta)
		{
			VarType const &tb = std::get<VarType>(b);
			return ta == tb;
		},
		[&](PointerType const &ta)
		{
			PointerType const &tb = std::get<PointerType>(b);
			return ta.mutability == tb.mutability && equiv(*ta.pointee, *tb.pointee);
		},
		[&](ManyPointerType const &ta)
		{
			ManyPointerType const &tb = std::get<ManyPointerType>(b);
			return ta.mutability == tb.mutability && equiv(*ta.pointee, *tb.pointee);
		},
		[&](StructType const &ta)
		{
			StructType const &tb = std::get<StructType>(b);
			return ta.inst == tb.inst;
		},
		[&](ProcType const &ta)
		{
			ProcType const &tb = std::get<ProcType>(b);
			return ta.inst == tb.inst;
		},
		[&](UnionType const &ta)
		{
			UnionType const &tb = std::get<UnionType>(b);

			assert(!"[TODO] equiv: UnionType");
			(void)ta;
			(void)tb;

			return false;
		},
		[&](ProcTypeUnresolved const&) -> bool { assert(!"equiv: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> bool { assert(!"equiv: UnionTypeUnresolved"); },
		[&](Path const&) -> bool { assert(!"equiv: Path"); },
	};
}


bool is_builtin_type(Type const &type, BuiltinTypeDef builtin)
{
	return type | match
	{
		[&](BuiltinType t)
		{
			return t.builtin == builtin;
		},
		[](auto const&) { return false; },
	};
}

static bool is_bool(Type const &type)
{
	return is_builtin_type(type, BuiltinTypeDef::BOOL);
}

static Type* mk_builtin_type(BuiltinTypeDef builtin, Arena *arena)
{
	return arena->alloc<Type>(BuiltinType(UNKNOWN_TOKEN_RANGE, builtin));
}

static Type* mk_known_int_type(Int128 low, Int128 high, Arena *arena)
{
	return arena->alloc<Type>(KnownIntType(low, high));
}

Type materialize_known_int(KnownIntType known_int)
{
	if(integer_assignable_to(BuiltinTypeDef::I32, known_int.low) and integer_assignable_to(BuiltinTypeDef::I32, known_int.high))
		return BuiltinType(UNKNOWN_TOKEN_RANGE, BuiltinTypeDef::I32);
	else
		return BuiltinType(UNKNOWN_TOKEN_RANGE, smallest_int_type_for(known_int.low, known_int.high));
}

static Type* mk_pointer_type(Type *pointee, IsMutable mutability, Arena *arena)
{
	return arena->alloc<Type>(PointerType{
		.range = UNKNOWN_TOKEN_RANGE,
		.pointee = pointee,
		.mutability = mutability,
	});
}


//--------------------------------------------------------------------
// Unification
//--------------------------------------------------------------------
enum class UnificationMode
{
	VALUE_ASSIGNMENT,

	POINTER_ASSIGNMENT,

	// Find a common type while treating both sides equally. The common type might differ from both
	// the left and the right side (e.g., unifying i8 and u8 yields i16).
	// This is the least-upper bound to which both sides are assignable.
	COMMON_TYPE,

	EQUAL,
};

static void unify(
	Type const &left,
	Type const &right,
	TypeEnv &subst,
	UnificationMode mode,
	SemaContext const &ctx,
	Expr *right_expr = nullptr
);

[[noreturn]] static void throw_unification_error(Type const &left, Type const &right, UnificationMode mode, Module const *mod)
{
	switch(mode)
	{
		case UnificationMode::VALUE_ASSIGNMENT:
		case UnificationMode::POINTER_ASSIGNMENT:
			throw ParseError("Cannot assign " + str(right, *mod) + " to " + str(left, *mod));

		case UnificationMode::EQUAL:
		case UnificationMode::COMMON_TYPE:
			throw ParseError("Got " + str(right, *mod) + " and " + str(left, *mod));
	}

	UNREACHABLE;
}


// Unification of integer types
//--------------------------------------------------------------------
static bool try_unify_integer_types(Type const &left, Type const &right, UnificationMode mode, SemaContext const &ctx)
{
	if(not is_integer_type(left) or not is_integer_type(right))
		return false;

	optional<Type> common_type = common_int_type(left, right);
	if(not common_type)
		throw_unification_error(left, right, mode, ctx.mod);

	switch(mode)
	{
		case UnificationMode::VALUE_ASSIGNMENT:
		{
			if(std::get_if<KnownIntType>(&left)) {
				// Always okay
			}
			else
			{
				if(not equiv(left, *common_type))
					throw_unification_error(left, right, mode, ctx.mod);
			}
		} break;

		case UnificationMode::COMMON_TYPE:
			// Nothing left to do
			break;

		case UnificationMode::POINTER_ASSIGNMENT:
		case UnificationMode::EQUAL:
		{
			if(std::get_if<KnownIntType>(&left) or std::get_if<KnownIntType>(&right))
			{
				// Two KnownIntTypes always have a common type (TODO not if they are ISIZE_MIN and
				// ISIZE_MAX)
			}
			else
			{
				if(not equiv(left, *common_type))
					throw_unification_error(left, right, mode, ctx.mod);

				if(not equiv(right, *common_type))
					throw_unification_error(left, right, mode, ctx.mod);
			}
		} break;
	}

	return true;
}


// Unification of type deduction variables
//--------------------------------------------------------------------
bool type_var_occurs_in(TypeDeductionVar const *var, Type const &type)
{
	return type | match
	{
		[&](BuiltinType const&) { return false; },
		[&](KnownIntType const&) { return false; },
		[&](VarType const &t)
		{
			if(TypeDeductionVar const* const* dv = std::get_if<TypeDeductionVar const*>(&t))
				return *dv == var;

			return false;
		},
		[&](PointerType const &t)
		{
			return type_var_occurs_in(var, *t.pointee);
		},
		[&](ManyPointerType const &t)
		{
			return type_var_occurs_in(var, *t.pointee);
		},
		[&](ProcType const &t)
		{
			return t.inst->occurring_vars.contains(var);
		},
		[&](StructType const &t)
		{
			return t.inst->type_args().occurring_vars.contains(var);
		},
		[&](UnionType const&)
		{
			assert(!"[TODO] type_var_occurs_in: UnionType");
			return false;
		},

		[&](ProcTypeUnresolved const&) -> bool { assert(!"type_var_occurs_in: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> bool { assert(!"type_var_occurs_in: UnionTypeUnresolved"); },
		[&](Path const&) -> bool { assert(!"type_var_occurs_in: Path"); },
	};
}

static TypeDeductionVar const* get_if_type_deduction_var(Type const &type)
{
	VarType const *var_type = std::get_if<VarType>(&type);
	if(not var_type)
		return nullptr;

	TypeDeductionVar const* const* type_deduction_var = std::get_if<TypeDeductionVar const*>(var_type);
	if(not type_deduction_var)
		return nullptr;

	return *type_deduction_var;
}

static void update_type_var(TypeDeductionVar const *var, Type const &type, TypeEnv &subst)
{
	if(Type const *existing_type = subst.try_lookup(var))
	{
		if(is_integer_type(*existing_type))
		{
			Type new_type = common_int_type(*existing_type, type).value();
			subst.replace(var, new_type);
		}
	}
}

static bool try_unify_type_deduction_vars(
	Type const &left,
	Type const &right,
	TypeEnv &subst,
	UnificationMode mode,
	SemaContext const &ctx,
	Expr *right_expr
)
{
	TypeDeductionVar const *left_var = get_if_type_deduction_var(left);
	TypeDeductionVar const *right_var = get_if_type_deduction_var(right);

	if(left_var and right_var)
	{
		if(left_var != right_var)
		{
			if(Type const *left_existing = subst.try_lookup(left_var))
			{
				unify(*left_existing, right, subst, mode, ctx, right_expr);

				// Here we update `left_var`. The above call to unify() will update `right_var`
				if(Type const *right_existing = subst.try_lookup(right_var))
					update_type_var(left_var, *right_existing, subst);
			}
			else
				subst.add(left_var, clone(right, &ctx.mod->sema->insts));

		}

		return true;
	}

	if(left_var or right_var)
	{
		TypeDeductionVar const *var = left_var ? left_var : right_var;
		Type const *type = left_var ? &right : &left;

		assert(not type_var_occurs_in(var, *type));
		if(Type const *existing_type = subst.try_lookup(var))
		{
			bool args_swapped = not left_var;
			if(args_swapped)
				unify(*type, *existing_type, subst, mode, ctx, right_expr);
			else
				unify(*existing_type, *type, subst, mode, ctx, right_expr);
		}
		else
			subst.add(var, clone(*type, &ctx.mod->sema->insts));

		update_type_var(var, *type, subst);

		return true;
	}

	return false;
}


// Unification of struct types
//--------------------------------------------------------------------
static StructInstance* get_if_struct(Type const *type)
{
	StructType const *struct_type = std::get_if<StructType>(type);
	if(not struct_type)
		return nullptr;

	return struct_type->inst;
}

static void unify_type_args(TypeArgList const &left, TypeArgList const &right, TypeEnv &subst, SemaContext const &ctx)
{
	assert(left.args->count == right.args->count);
	for(size_t i = 0; i < left.args->count; ++i)
		unify(left.args->items[i], right.args->items[i], subst, UnificationMode::EQUAL, ctx);
}

static void unify_structs_eq(StructInstance *left, StructInstance *right, TypeEnv &subst, SemaContext const &ctx)
{
	if(left == right)
		return;

	if(left->is_deduction_complete() && right->is_deduction_complete())
	{
		if(left != right)
			throw_unification_error(
				StructType(UNKNOWN_TOKEN_RANGE, left),
				StructType(UNKNOWN_TOKEN_RANGE, right),
				UnificationMode::EQUAL,
				ctx.mod
			);
	}
	else
	{
		if(left->struct_() != right->struct_())
			throw_unification_error(
				StructType(UNKNOWN_TOKEN_RANGE, left),
				StructType(UNKNOWN_TOKEN_RANGE, right),
				UnificationMode::EQUAL,
				ctx.mod
			);

		do
		{
			unify_type_args(left->type_args(), right->type_args(), subst, ctx);
			left = left->parent();
			right = right->parent();
		}
		while(left);
	}
}

static bool try_unify_structs(
	Type const &left,
	Type const &right,
	TypeEnv &subst,
	UnificationMode mode,
	SemaContext const &ctx,
	Expr *right_expr
)
{
	StructInstance *left_struct = get_if_struct(&left);
	if(not left_struct)
		return false;

	switch(mode)
	{
		case UnificationMode::VALUE_ASSIGNMENT:
		case UnificationMode::POINTER_ASSIGNMENT:
		{
			try
			{
				StructInstance *right_struct = get_if_struct(&right);
				if(not right_struct)
					throw_unification_error(left, right, mode, ctx.mod);

				if(left_struct != right_struct)
				{
					while(left_struct->struct_() != right_struct->struct_())
					{
						if(not right_struct->parent())
							throw_unification_error(left, right, mode, ctx.mod);

						right_struct = right_struct->parent();
					}

				}

				unify_structs_eq(left_struct, right_struct, subst, ctx);
			}
			catch(ParseError const &exc)
			{
				if(mode == UnificationMode::VALUE_ASSIGNMENT and left_struct->implicit_case())
				{
					StructInstance *implicit_case = left_struct->implicit_case();
					ProcType const &implicit_ctor = std::get<ProcType>(*implicit_case->try_get_ctor_type());
					Type const &implicit_param_type = implicit_ctor.inst->params->items[0];

					unify(implicit_param_type, right, subst, UnificationMode::VALUE_ASSIGNMENT, ctx, right_expr);

					if(right_expr)
					{
						Expr *ctor_expr = ctx.arena->alloc<Expr>(ConstructorExpr{
							.range = UNKNOWN_TOKEN_RANGE,
							.ctor = ctx.arena->alloc<Type>(StructType(UNKNOWN_TOKEN_RANGE, implicit_case)),
							.type = implicit_case->try_get_ctor_type(),
						});

						FixedArray<Argument> *ctor_args = alloc_fixed_array<Argument>(1, *ctx.arena);
						ctor_args->items[0] = Argument{
							.range = UNKNOWN_TOKEN_RANGE,
							.expr = *right_expr,
						};

						Expr *call_expr = ctx.arena->alloc<Expr>(CallExpr{
							.range = UNKNOWN_TOKEN_RANGE,
							.callable = ctor_expr,
							.args = ctor_args,
							.type = ctx.arena->alloc<Type>(StructType(UNKNOWN_TOKEN_RANGE, implicit_case)),
						});

						*right_expr = *call_expr;
					}
				}
				else
					throw exc;
			}
		} break;

		case UnificationMode::COMMON_TYPE:
		{
			assert(!"[TODO] try_unify_structs: COMMON_TYPE");
		} break;

		case UnificationMode::EQUAL:
		{
			StructInstance *right_struct = get_if_struct(&right);
			if(not right_struct)
				throw_unification_error(left, right, mode, ctx.mod);

			unify_structs_eq(left_struct, right_struct, subst, ctx);
		} break;
	}

	return true;
}


// Unification of pointer types
//--------------------------------------------------------------------
static void unify_pointers(
	Type const &left_pointee, IsMutable left_mutability,
	Type const &right_pointee, IsMutable right_mutability,
	TypeEnv &subst,
	UnificationMode mode,
	SemaContext const &ctx
)
{
	switch(mode)
	{
		case UnificationMode::VALUE_ASSIGNMENT:
		{
			bool mut_compatible = right_mutability == IsMutable::YES or left_mutability == IsMutable::NO;
			if(not mut_compatible)
				throw ParseError("Incompatible pointer mutability");

			unify(left_pointee, right_pointee, subst, UnificationMode::POINTER_ASSIGNMENT, ctx);
		} break;

		case UnificationMode::POINTER_ASSIGNMENT:
		{
			assert(!"[TODO] unify_pointers: POINTER_ASSIGNMENT");
		} break;

		case UnificationMode::COMMON_TYPE:
		{
			assert(!"[TODO] unify_pointers: COMMON_TYPE");
		} break;

		case UnificationMode::EQUAL:
		{
			if(left_mutability != right_mutability)
				throw ParseError("Incompatible pointer mutability");

			unify(left_pointee, right_pointee, subst, mode, ctx);
		} break;
	}
}


// Unification
//--------------------------------------------------------------------
// See
// - https://eli.thegreenplace.net/2018/unification/
//   - The presented algorithm seems to be incorrect, or at least does not match the usual
//     definition for first-order logic. Assume we want to unify the following formulas:
//
//       f(x, y)
//       f(y, c)
//
//     where x and y are variables and c is a constant. Eli's algorithm would give the following
//     substitution:
//
//       subst_eli = [x ↦ y, y ↦ c]
//
//     However, the correct one would be:
//
//       subst_fol = [x ↦ c, y ↦ c]
//
//     The reason that subst_eli is incorrect is that substitutions are applied to all variables
//     simultaneously, or in other words: substitutions are *not* applied recursively to the
//     substituted term. Thus, under a classical definitition of substitution, subst_eli applied to
//     the above formulas would result in
//
//       f(y, c)
//       f(c, c)
//
//     As one can see, subst_eli did not actually unify the formulas. Eli's algorithm still seems to
//     work though because he *does* apply substitutions recursively. However, I don't know if this
//     has any other consequences.
//
//     Still, I'm following Eli here because it's easy to implement and seems to work for now.
//
// - https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm
//
//
// TODO During unification, the TypeEnv is eagerly updated, even if unification fails later. If
//      typechecking continues despite such an error, then the TypeEnv may contain spurious entries.
//      Find out if this is a problem.
static void unify(
	Type const &left,
	Type const &right,
	TypeEnv &subst,
	UnificationMode mode,
	SemaContext const &ctx,
	Expr *right_expr
)
{
	if(try_unify_type_deduction_vars(left, right, subst, mode, ctx, right_expr))
		return;

	if(try_unify_integer_types(left, right, mode, ctx))
		return;

	if(try_unify_structs(left, right, subst, mode, ctx, right_expr))
		return;

	left | match
	{
		[&](BuiltinType const &left_t)
		{
			BuiltinType const *right_t = std::get_if<BuiltinType>(&right);
			if(not right_t)
				throw_unification_error(left, right, mode, ctx.mod);

			// Integer types have already been handled above.
			// Non-integer builtin types (Never, Unit, Bool) can only be unified
			// if they are equal.
			if(left_t.builtin != right_t->builtin)
				throw_unification_error(left, right, mode, ctx.mod);
		},
		[&](KnownIntType const&) { UNREACHABLE; },
		[&](VarType const&)
		{
			// If we get here, we know `left_t` is a TypeParameterVar (because
			// we already handled TypeDeductionVars above).
			// Unification of TypeParameterVars only succeeds if both sides
			// refer to the same TypeParameterVar (because we don't know which
			// type they are referring to).

			if(not equiv(left, right))
				throw_unification_error(left, right, mode, ctx.mod);
		},
		[&](PointerType const &left_t)
		{
			PointerType const *right_t = std::get_if<PointerType>(&right);
			if(not right_t)
				throw_unification_error(left, right, mode, ctx.mod);

			unify_pointers(
				*left_t.pointee, left_t.mutability,
				*right_t->pointee, right_t->mutability,
				subst,
				mode,
				ctx
			);
		},
		[&](ManyPointerType const &left_t)
		{
			ManyPointerType const *right_t = std::get_if<ManyPointerType>(&right);
			if(not right_t)
				throw_unification_error(left, right, mode, ctx.mod);

			unify_pointers(
				*left_t.pointee, left_t.mutability,
				*right_t->pointee, right_t->mutability,
				subst,
				mode,
				ctx
			);
		},
		[&](ProcType const&)
		{
			assert(!"[TODO] unify: ProcType");
		},
		[&](UnionType const&)
		{
			assert(!"[TODO] unify: UnionType");
		},

		// Handled above
		[&](StructType const&) { UNREACHABLE; },

		[&](ProcTypeUnresolved const&) { assert(!"unify: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) { assert(!"unify: UnionTypeUnresolved"); },
		[&](Path const&) { assert(!"unify: Path"); },
	};
}


//--------------------------------------------------------------------
// Casting
//--------------------------------------------------------------------
static bool is_cast_ok(Type const &target_type, Expr &src_expr, SemaContext const &ctx)
{
	Type *src_type = type_of(src_expr);
	try {
		TypeEnv env;
		unify(target_type, *src_type, env, UnificationMode::VALUE_ASSIGNMENT, ctx, &src_expr);
		return true;
	}
	catch(ParseError const&) {}

	return *src_type | match
	{
		[&](BuiltinType const &src_t)
		{
			BuiltinType const *target_t = std::get_if<BuiltinType>(&target_type);
			if(not target_t)
				return false;

			return is_builtin_int_type(src_t.builtin) and is_builtin_int_type(target_t->builtin);
		},
		[&](KnownIntType const&) -> bool
		{
			return is_integer_type(target_type);
		},
		[&](VarType const&) -> bool
		{
			assert(!"[TODO] is_cast_ok: VarType");
		},
		[&](PointerType const &src_t) -> bool
		{
			if(PointerType const *target_t = std::get_if<PointerType>(&target_type))
				return src_t.mutability == IsMutable::YES || target_t->mutability == IsMutable::NO;

			if(ManyPointerType const *target_t = std::get_if<ManyPointerType>(&target_type))
				return src_t.mutability == IsMutable::YES || target_t->mutability == IsMutable::NO;

			return false;
		},
		[&](ManyPointerType const &src_t) -> bool
		{
			if(PointerType const *target_t = std::get_if<PointerType>(&target_type))
				return src_t.mutability == IsMutable::YES || target_t->mutability == IsMutable::NO;

			if(ManyPointerType const *target_t = std::get_if<ManyPointerType>(&target_type))
				return src_t.mutability == IsMutable::YES || target_t->mutability == IsMutable::NO;

			return false;
		},
		[&](StructType const&) -> bool
		{
			assert(!"[TODO] is_cast_ok: StructType");
		},
		[&](ProcType const&) -> bool
		{
			assert(!"[TODO] is_cast_ok: ProcType");
		},
		[&](UnionType const&) -> bool
		{
			assert(!"[TODO] is_cast_ok: UnionType");
		},

		[&](ProcTypeUnresolved const&) -> bool { assert(!"is_cast_ok: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> bool { assert(!"is_cast_ok: UnionTypeUnresolved"); },
		[&](Path const&) -> bool { assert(!"is_cast_ok: Path"); },
	};
}


//--------------------------------------------------------------------
// Computing the constructor/procedure dependencies of an expression.
// Needed to detect cycles in default value expressions.
//--------------------------------------------------------------------
struct DefaultValueDep
{
	variant<ProcItem const*, StructItem const*> callable;
	string_view field;

	friend bool operator == (DefaultValueDep const &a, DefaultValueDep const &b) = default;
};

template<>
struct std::hash<DefaultValueDep>
{
	size_t operator () (DefaultValueDep const &dep) const
	{
		size_t h = compute_hash(dep.field);
		dep.callable | match
		{
			[&](ProcItem const *proc) { combine_hashes(h, compute_hash(proc)); },
			[&](StructItem const *struct_) { combine_hashes(h, compute_hash(struct_)); },
		};

		return h;
	}
};

enum class VisitState
{
	IN_PROGRESS,
	DONE,
};


static void check_default_value_deps(
	variant<ProcItem const*, StructItem const*> callable,
	string_view field,
	Expr const *default_value,
	std::unordered_map<DefaultValueDep, VisitState> &deps,
	Module const &mod
);

static void expr_default_value_deps(Expr const &expr, std::unordered_map<DefaultValueDep, VisitState> &deps, Module const &mod)
{
	expr | match
	{
		[&](IntLiteralExpr const&) {},
		[&](BoolLiteralExpr const&) {},
		[&](StringLiteralExpr const&) {},
		[&](UnaryExpr const &e)
		{
			expr_default_value_deps(*e.sub, deps, mod);
		},
		[&](BinaryExpr const &e)
		{
			expr_default_value_deps(*e.left, deps, mod);
			expr_default_value_deps(*e.right, deps, mod);
		},
		[&](AddressOfExpr const &e)
		{
			expr_default_value_deps(*e.object, deps, mod);
		},
		[&](DerefExpr const &e)
		{
			expr_default_value_deps(*e.addr, deps, mod);
		},
		[&](IndexExpr const &e)
		{
			expr_default_value_deps(*e.addr, deps, mod);
			expr_default_value_deps(*e.index, deps, mod);
		},
		[&](MemberAccessExpr const &e)
		{
			expr_default_value_deps(*e.object, deps, mod);
		},
		[&](AssignmentExpr const &e)
		{
			expr_default_value_deps(*e.lhs, deps, mod);
			expr_default_value_deps(*e.rhs, deps, mod);
		},
		[&](AsExpr const &e)
		{
			expr_default_value_deps(*e.src_expr, deps, mod);
		},
		[&](ConstructorExpr const&) {},
		[&](ProcExpr const&) {},
		[&](CallExpr const &e)
		{
			ProcType const &proc_type = std::get<ProcType>(*type_of(*e.callable));

			vector<bool> provided_args(proc_type.param_count(), false);
			for(Argument const &arg: *e.args)
			{
				expr_default_value_deps(arg.expr, deps, mod);
				provided_args[arg.param_idx] = true;
			}

			for(auto const &[idx, arg_provided]: provided_args | std::views::enumerate)
			{
				if(not arg_provided)
				{
					proc_type.callable | match
					{
						[&](ProcInstance *proc)
						{
							ProcItem const *item = proc->proc();
							Parameter const &param = item->params->items[idx];
							
							check_default_value_deps(item, name_of(param, &mod), &param.default_value.get_expr(), deps, mod);
						},
						[&](StructInstance *struct_)
						{
							StructItem const *item = struct_->struct_();
							assert(item->sema->ctor_params);

							Parameter const *param = item->sema->ctor_params->items[idx];
							check_default_value_deps(item, name_of(*param, &mod), &param->default_value.get_expr(), deps, mod);
						},
					};
				}
			}
		},
		[&](SizeOfExpr const&) {},
		[&](MakeExpr const &e)
		{
			expr_default_value_deps(*e.addr, deps, mod);
			expr_default_value_deps(*e.init, deps, mod);
		},
		[&](VarExpr const&) {},
		[&](Path const&) { assert(!"expr_default_value_deps: Path"); },
	};
}

static void check_default_value_deps(
	variant<ProcItem const*, StructItem const*> callable,
	string_view field,
	Expr const *default_value,
	std::unordered_map<DefaultValueDep, VisitState> &deps,
	Module const &mod
)
{

	auto res = deps.emplace(DefaultValueDep(callable, field), VisitState::IN_PROGRESS);
	if(res.second) // inserted?
		expr_default_value_deps(*default_value, deps, mod);
	else
	{
		if(res.first->second == VisitState::IN_PROGRESS)
			throw_sem_error("Cyclic default arguments", token_range_of(*default_value).first, &mod);
	}

	res.first->second = VisitState::DONE;
}


//--------------------------------------------------------------------
// Type checking
//--------------------------------------------------------------------
struct TypingHint
{
	TypingHint() = default;

	explicit TypingHint(TypeEnv *subst) :
		subst(subst) {}

	explicit TypingHint(Type const *type, TypeEnv *subst) :
		type(type),
		subst(subst) {}

	Type const *type = nullptr;
	TypeEnv *subst;

	TypingHint with_type(Type const *new_type)
	{
		return TypingHint(new_type, subst);
	}

	TypingHint without_type()
	{
		return TypingHint(subst);
	}
};


optional<size_t> find_by_name(ProcType const &proc, string_view name)
{
	size_t count = proc.param_count();
	for(size_t i = 0; i < count; ++i)
	{
		if(proc.param_name_at(i) == name)
			return i;
	}

	return nullopt;
}

Parameter const* find_var_member(StructInstance *inst, string_view field)
{
	Module *mod = inst->struct_()->sema->type_scope->mod; // This looks disgusting

	for(Parameter const &var_member: inst->own_var_members())
	{
		if(name_of(var_member, mod) == field)
			return &var_member;
	}

	if(inst->parent())
		return find_var_member(inst->parent(), field);

	return nullptr;
}


optional<IsMutable> is_lvalue_expr(Expr const &expr)
{
	return expr | match
	{
		[](VarExpr const &e) -> optional<IsMutable>
		{
			return e.var->mutability;
		},
		[](DerefExpr const &e) -> optional<IsMutable>
		{
			return std::get<PointerType>(*type_of(*e.addr)).mutability;
		},
		[](IndexExpr const &e) -> optional<IsMutable>
		{
			return std::get<ManyPointerType>(*type_of(*e.addr)).mutability;
		},
		[](MemberAccessExpr const &e) -> optional<IsMutable>
		{
			return is_lvalue_expr(*e.object);
		},

		[](auto const&) -> optional<IsMutable> { return nullopt; },
	};
}

size_t ProcType::param_count() const
{
	return callable | match
	{
		[&](ProcInstance *proc) { return proc->get_param_count(); },
		[&](StructInstance *struct_) { return struct_->get_param_count(); },
	};
}

Type* ProcType::param_type_at(size_t idx) const
{
	return callable | match
	{
		[&](ProcInstance *proc) { return proc->get_param_type_at(idx); },
		[&](StructInstance *struct_) { return struct_->get_ctor_param_type_at(idx); },
	};
}

string_view ProcType::param_name_at(size_t idx) const
{
	return callable | match
	{
		[&](ProcInstance *proc) { return proc->get_param_name_at(idx); },
		[&](StructInstance *struct_) { return struct_->get_ctor_param_name_at(idx); },
	};
}

DefaultValueExpr ProcType::param_default_value_at(size_t idx) const
{
	return callable | match
	{
		[&](ProcInstance *proc) { return proc->get_param_default_value(idx); },
		[&](StructInstance *struct_) { return struct_->get_ctor_param_default_value(idx); },
	};
}


static Type* typecheck_subexpr(Expr &expr, TypingHint hint, SemaContext &ctx)
{
	LOGGER(ctx.mod->logger, on_expr_start, expr);

	auto res = expr | match
	{
		[&](IntLiteralExpr &e)
		{
			return e.type = ctx.arena->alloc<Type>(KnownIntType(e.value, e.value));
		},
		[&](BoolLiteralExpr &e)
		{
			return e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
		},
		[&](StringLiteralExpr &e)
		{
			ScopeItem &c_char_item = ctx.mod->sema->globals->lookup("c_char", INVALID_TOKEN_IDX);
			StructInstance *c_char = ctx.mod->sema->insts.get_struct_instance(std::get<StructItem*>(c_char_item), nullptr, nullptr);

			return e.type = ctx.arena->alloc<Type>(ManyPointerType{
				.range = UNKNOWN_TOKEN_RANGE,
				.pointee = ctx.arena->alloc<Type>(StructType(UNKNOWN_TOKEN_RANGE, c_char)),
				.mutability = IsMutable::NO,
			});
		},
		[&](UnaryExpr &e)
		{
			switch(e.op)
			{
				case UnaryOp::NOT:
				{
					Type const *sub_type = typecheck_subexpr(*e.sub, TypingHint(), ctx);
					if(not is_bool(*sub_type))
						throw_sem_error("Expected expression of type bool", e.range.first, ctx.mod);

					return e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
				}

				case UnaryOp::NEG:
				{
					Type const *sub_type = typecheck_subexpr(*e.sub, hint.without_type(), ctx);
					if(KnownIntType const *known_int = std::get_if<KnownIntType>(sub_type))
						e.type = mk_known_int_type(-known_int->high, -known_int->low, ctx.arena);
					else if(is_integer_type(*sub_type))
						e.type = ctx.arena->alloc<Type>(clone(*sub_type, &ctx.mod->sema->insts));
					else
						throw_sem_error("Expected integer type", e.range.first, ctx.mod);
				}
			}

			return e.type;
		},
		[&](BinaryExpr &e)
		{
			switch(e.op)
			{
				case BinaryOp::ADD:
				case BinaryOp::SUB:
				case BinaryOp::MUL:
				case BinaryOp::DIV:
				{
					Type const *left_type = typecheck_subexpr(*e.left, hint.without_type(), ctx);
					Type const *right_type = typecheck_subexpr(*e.right, hint.without_type(), ctx);

					try { 
						unify(*left_type, *right_type, *hint.subst, UnificationMode::COMMON_TYPE, ctx);
					} catch(ParseError const &exc) {
						throw_sem_error("Invalid arguments for binary operation: "s + exc.what(), e.range.first, ctx.mod);
					}

					KnownIntType const *left_known_int = std::get_if<KnownIntType>(left_type);
					KnownIntType const *right_known_int = std::get_if<KnownIntType>(right_type);
					if(left_known_int && right_known_int)
					{
						// TODO Implement interval arithmetic
						assert(left_known_int->low == left_known_int->high);
						assert(right_known_int->low == right_known_int->high);

						Int128 result;
						switch(e.op)
						{
							case BinaryOp::ADD: result = left_known_int->low + right_known_int->low; break;
							case BinaryOp::SUB: result = left_known_int->low - right_known_int->low; break;
							case BinaryOp::MUL: result = left_known_int->low * right_known_int->low; break;
							case BinaryOp::DIV: result = left_known_int->low / right_known_int->low; break;
							default: UNREACHABLE;
						}

						e.type = mk_known_int_type(result, result, ctx.arena);
					}
					else if(is_integer_type(*left_type) && is_integer_type(*right_type))
					{
						optional<Type> result_type = common_int_type(*left_type, *right_type);
						if(not result_type)
						{
							throw_sem_error(
								"Binary op: expected same integral types, got " + str(*left_type, *ctx.mod) + " and " + str(*right_type, *ctx.mod),
								e.range.first,
								ctx.mod
							);
						}

						e.type = ctx.arena->alloc<Type>(*result_type);
					}
					else
					{
						throw_sem_error(
							"Arithmetic operation requires integer types, got " + str(*left_type, *ctx.mod) + " and " + str(*right_type, *ctx.mod),
							e.range.first,
							ctx.mod
						);
					}
				} break;

				case BinaryOp::EQ:
				{
					Type const *left_type = typecheck_subexpr(*e.left, hint.without_type(), ctx);
					Type const *right_type = typecheck_subexpr(*e.right, hint.without_type(), ctx);

					try {
						unify(*left_type, *right_type, *hint.subst, UnificationMode::EQUAL, ctx);
					}
					catch(ParseError const &exc) {
						throw_sem_error("Equality operator requires equal types: "s + exc.what(), e.range.first, ctx.mod);
					}

					e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
				} break;

				case BinaryOp::LT:
				case BinaryOp::LE:
				case BinaryOp::GT:
				case BinaryOp::GE:
				{
					Type const *left_type = typecheck_subexpr(*e.left, hint.without_type(), ctx);
					Type const *right_type = typecheck_subexpr(*e.right, hint.without_type(), ctx);

					try {
						unify(*left_type, *right_type, *hint.subst, UnificationMode::COMMON_TYPE, ctx);
					}
					catch(ParseError const &exc) {
						throw_sem_error("Comparison operator requires compatible types: "s + exc.what(), e.range.first, ctx.mod);
					}

					if(not is_integer_type(*left_type))
						throw_sem_error("Comparison requires integer type, got " + str(*left_type, *ctx.mod), token_range_of(*e.left).first, ctx.mod);

					if(not is_integer_type(*right_type))
						throw_sem_error("Comparison requires integer type, got " + str(*right_type, *ctx.mod), token_range_of(*e.right).first, ctx.mod);

					e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
				} break;
			}

			return e.type;
		},
		[&](AddressOfExpr &e)
		{
			Type const *object_type = typecheck_subexpr(*e.object, hint.without_type(), ctx);
			optional<IsMutable> object_mutability = is_lvalue_expr(*e.object);
			if(not object_mutability)
				throw_sem_error("Address-of expression requires lvalue", e.range.first, ctx.mod);

			if(e.mutability == IsMutable::YES and object_mutability == IsMutable::NO)
				throw_sem_error("Cannot make mutable reference to const object", e.range.first, ctx.mod);

			return e.type = mk_pointer_type(clone_ptr(object_type, &ctx.mod->sema->insts), e.mutability, ctx.arena);
		},
		[&](DerefExpr &e)
		{
			Type const *addr_type = typecheck_subexpr(*e.addr, hint.without_type(), ctx);
			PointerType const *addr_ptr_type = std::get_if<PointerType>(addr_type);
			if(not addr_ptr_type)
				throw_sem_error("Expected pointer type", e.range.first, ctx.mod);

			return e.type = clone_ptr(addr_ptr_type->pointee, &ctx.mod->sema->insts);
		},
		[&](IndexExpr &e)
		{
			Type const *addr_type = typecheck_subexpr(*e.addr, hint.without_type(), ctx);
			Type const *index_type = typecheck_subexpr(*e.index, hint.without_type(), ctx);

			ManyPointerType const *addr_ptr_type = std::get_if<ManyPointerType>(addr_type);
			if(not addr_ptr_type)
				throw_sem_error("Expected many pointer type for indexing", token_range_of(*e.addr).first, ctx.mod);

			if(not is_integer_type(*index_type))
				throw_sem_error("Index expression must be of integer type", token_range_of(*e.index).first, ctx.mod);

			return e.type = clone_ptr(addr_ptr_type->pointee, &ctx.mod->sema->insts);
		},
		[&](MemberAccessExpr &e)
		{
			Type const *object_type = typecheck_subexpr(*e.object, hint.without_type(), ctx);
			StructType const *struct_type = std::get_if<StructType>(object_type);
			if(not struct_type)
				throw_sem_error("Expected object of struct type for member access", e.range.first, ctx.mod);

			Parameter const *var_member = find_var_member(struct_type->inst, e.member);
			if(not var_member)
				throw_sem_error("`"s + struct_type->inst->struct_()->name + "` has no field named `"s + e.member + "`", e.range.first, ctx.mod);

			return e.type = clone_ptr(var_member->type, &ctx.mod->sema->insts);
		},
		[&](AssignmentExpr &e)
		{
			Type const *lhs_type = typecheck_subexpr(*e.lhs, hint.without_type(), ctx);
			Type const *rhs_type = typecheck_subexpr(*e.rhs, hint.with_type(lhs_type), ctx);

			try {
				unify(*lhs_type, *rhs_type, *hint.subst, UnificationMode::VALUE_ASSIGNMENT, ctx, e.rhs);
			}
			catch(ParseError const &exc) {
				throw_sem_error(exc.what(), e.range.first, ctx.mod);
			}

			if(is_lvalue_expr(*e.lhs) != IsMutable::YES)
				throw_sem_error("LHS must denote a mutable lvalue", e.range.first, ctx.mod);

			return e.type = clone_ptr(lhs_type, &ctx.mod->sema->insts);
		},
		[&](AsExpr &e)
		{
			// See test `casting_and_type_hinting` for the reason we do not pass e.target_type as a
			// type hint
			typecheck_subexpr(*e.src_expr, hint.without_type(), ctx);

			if(not is_cast_ok(*e.target_type, *e.src_expr, ctx))
				throw_sem_error("Invalid cast", e.range.first, ctx.mod);

			return e.type = clone_ptr(e.target_type, &ctx.mod->sema->insts);
		},
		[&](ConstructorExpr &e)
		{
			StructType const *struct_type = std::get_if<StructType>(e.ctor);
			if(not struct_type)
				throw_sem_error("Expected struct type", e.range.first, ctx.mod);

			Type const *ctor_type = struct_type->inst->try_get_ctor_type();
			if(not ctor_type)
				throw_sem_error("Struct does not provide a constructor", e.range.first, ctx.mod);

			return e.type = clone_ptr(ctor_type, &ctx.mod->sema->insts);
		},
		[&](ProcExpr &e)
		{
			return e.type = clone_ptr(e.inst->get_type(), &ctx.mod->sema->insts);
		},
		[&](CallExpr &e)
		{
			Type const *callable_type = typecheck_subexpr(*e.callable, hint.without_type(), ctx);
			ProcType const *callable_proc_type = std::get_if<ProcType>(callable_type);
			if(not callable_proc_type)
				throw_sem_error("Expected callable expression", e.range.first, ctx.mod);

			FixedArray<Type> const *param_types = callable_proc_type->inst->params;
			if(e.args->count > param_types->count)
				throw_sem_error("Too many arguments", e.range.first, ctx.mod);

			if(hint.type)
			{
				try {
					unify(*hint.type, *callable_proc_type->inst->ret, *hint.subst, UnificationMode::VALUE_ASSIGNMENT, ctx);
				}
				catch(ParseError const&) {
					// It's just a hint, so if unification fails it's fine
				}
			}

			unordered_set<int> assigned_params;
			bool has_unordered_named_args = false;
			for(size_t i = 0; i < e.args->count; ++i)
			{
				Argument &arg = e.args->items[i];

				// Find the corresponding parameter depending on whether the argument is named or not
				if(arg.name.size())
				{
					if(optional<size_t> param_idx_opt = find_by_name(*callable_proc_type, arg.name))
					{
						arg.param_idx = *param_idx_opt;
						if(*param_idx_opt != i)
							has_unordered_named_args = true;
					}
					else
						throw_sem_error("Invalid parameter name `"s + arg.name + "`", arg.range.first, ctx.mod);
				}
				else
				{
					if(has_unordered_named_args)
						throw_sem_error("Positional arguments must come before named ones", arg.range.first, ctx.mod);

					arg.param_idx = i;
				}

				auto res = assigned_params.insert(arg.param_idx);
				if(!res.second)
					throw_sem_error("Multiple arguments for same parameter", arg.range.first, ctx.mod);

				Type const &param_type = param_types->items[arg.param_idx];
				Type *arg_type = typecheck_subexpr(arg.expr, hint.with_type(&param_type), ctx);
				try {
					unify(param_type, *arg_type, *hint.subst, UnificationMode::VALUE_ASSIGNMENT, ctx, &arg.expr);
				} catch(ParseError const &exc) {
					throw_sem_error("Invalid argument type: "s + exc.what(), token_range_of(arg.expr).first, ctx.mod);
				}
			}

			for(size_t param_idx = 0; param_idx < param_types->count; ++param_idx)
			{
				if(!assigned_params.contains(param_idx))
				{
					if(not callable_proc_type->param_default_value_at(param_idx))
						throw_sem_error("Missing argument for parameter "s + callable_proc_type->param_name_at(param_idx), e.range.first, ctx.mod);
				}
			}

			e.type = clone_ptr(callable_proc_type->inst->ret, &ctx.mod->sema->insts);

			// The only kinds of expressions that can introduve TypeDeductionVars are
			// ConstructorExprs and ProcExprs, which at the moment can only occur as subexpressions
			// of CallExpr. So maybe it is enough to call substitute_types_in_expr() here and we can
			// omit substitution in typecheck_expr()?
			substitute_types_in_expr(expr, *hint.subst, ctx.mod->sema->insts);

			return e.type;
		},
		[&](SizeOfExpr &e)
		{
			return e.type = mk_builtin_type(BuiltinTypeDef::USIZE, ctx.arena);
		},
		[&](MakeExpr&)
		{
			assert(!"[TODO] typecheck_subexpr: MakeExpr");
			return (Type*)nullptr;
		},
		[&](VarExpr &e)
		{
			assert(e.var->type && "typecheck_subexpr: VarExpr: Var::type is null");
			return e.type = clone_ptr(e.var->type, &ctx.mod->sema->insts);
		},
		[&](Path&) -> Type* { assert(!"typecheck_subexpr: Path"); },
	};

	LOGGER(ctx.mod->logger, on_expr_end);

	return res;
}

static Type const* typecheck_expr(Expr &expr, SemaContext &ctx)
{
	TypeEnv subst;
	Type const *type = typecheck_subexpr(expr, TypingHint(&subst), ctx);

	if(not subst.empty())
		substitute_types_in_expr(expr, subst, ctx.mod->sema->insts);

	return type;
}


static Type const* typecheck_pattern(
	Pattern &lhs_pattern,
	Type const &rhs_type,
	optional<IsMutable> rhs_is_lvalue,
	bool infallible_pattern_required,
	TypeEnv &subst,
	SemaContext &ctx,
	Expr *rhs_expr = nullptr
)
{
	Type const *expected_type = &rhs_type;
	if(lhs_pattern.provided_type)
	{
		try
		{
			if(infallible_pattern_required)
				unify(*lhs_pattern.provided_type, rhs_type, subst, UnificationMode::VALUE_ASSIGNMENT, ctx, rhs_expr);
			else
				unify(rhs_type, *lhs_pattern.provided_type, subst, UnificationMode::VALUE_ASSIGNMENT, ctx);
		}
		catch (ParseError const &e)
		{
			throw_sem_error(
				"Inferred type does not match specified type in let statement: "s + e.what(),
				token_range_of(lhs_pattern).first,
				ctx.mod
			);
		}

		expected_type = &*lhs_pattern.provided_type;
	}

	return lhs_pattern | match
	{
		[&](VarPattern &p)
		{
			if(KnownIntType const *known_int = std::get_if<KnownIntType>(expected_type))
				p.var->type = ctx.arena->alloc<Type>(materialize_known_int(*known_int));
			else
				p.var->type = clone_ptr(expected_type, &ctx.mod->sema->insts);

			return p.type = clone_ptr(p.var->type, &ctx.mod->sema->insts);
		},
		[&](DerefPattern &p)
		{
			PointerType const *pointer_type = std::get_if<PointerType>(expected_type);
			if(not pointer_type)
				throw_sem_error("Invalid deref pattern: target type not a pointer", p.range.first, ctx.mod);

			Type const *sub_type = typecheck_pattern(
				*p.sub,
				*pointer_type->pointee,
				rhs_is_lvalue, // TODO Is this correct to always pass `rhs_is_lvalue` unchanged?
				infallible_pattern_required,
				subst,
				ctx
			);
			return p.type = clone_ptr(sub_type, &ctx.mod->sema->insts);
		},
		[&](AddressOfPattern &p)
		{
			if(not rhs_is_lvalue)
				throw_sem_error("Can only take address of lvalue expression", p.range.first, ctx.mod);

			if(*rhs_is_lvalue == IsMutable::NO && p.mutability == IsMutable::YES)
				throw_sem_error("Cannot make mutable reference to const object", p.range.first, ctx.mod);

			Type sub_rhs_type(PointerType{
				.range = UNKNOWN_TOKEN_RANGE,
				.pointee = clone_ptr(expected_type, &ctx.mod->sema->insts),
				.mutability = p.mutability,
			});

			Type const *sub_type = typecheck_pattern(
				*p.sub,
				sub_rhs_type,
				nullopt, // TODO How do I pass along `rhs_is_lvalue`?
				infallible_pattern_required,
				subst,
				ctx
			);

			return p.type = clone_ptr(sub_type, &ctx.mod->sema->insts);
		},
		[&](ConstructorPattern &p) -> Type*
		{
			if(infallible_pattern_required)
			{
				// For now, we treat all constructor patterns as fallible
				throw_sem_error("Infallible pattern required, but encountered constructor pattern", p.range.first, ctx.mod);
			}

			try {
				unify(*expected_type, *p.ctor, subst, UnificationMode::VALUE_ASSIGNMENT, ctx);
			}
			catch(ParseError const &exc) {
				throw_sem_error("Invalid constructor pattern: "s + exc.what(), p.range.first, ctx.mod);
			}
			substitute(*p.ctor, subst, ctx.mod->sema->insts);

			// Validate constructor
			StructType const *ctor = std::get_if<StructType>(p.ctor);
			if(ctor)
			{
				if(p.has_parens and (ctor->inst->struct_()->ctor_without_parens or not ctor->inst->try_get_ctor_type()))
					throw_sem_error("Invalid parentheses after type in pattern", p.range.first, ctx.mod);

				// If no parens are given then we only match on the type
				if(p.has_parens and ctor->inst->try_get_ctor_type())
				{
					ProcType const &ctor_proc_type = std::get<ProcType>(*ctor->inst->try_get_ctor_type());
					if(p.args->count > ctor_proc_type.inst->params->count)
						throw_sem_error("Too many arguments provided for constructor in pattern", p.range.first, ctx.mod);

					if(p.args->count < ctor_proc_type.inst->params->count)
						throw_sem_error("TODO: Support default arguments in patterns", p.range.first, ctx.mod);

					for(size_t i = 0; i < p.args->count; ++i)
					{
						PatternArgument &arg = p.args->items[i];

						Type param_type = clone(ctor_proc_type.inst->params->items[i], &ctx.mod->sema->insts);

						if(arg.param_name.size())
							throw_sem_error("TODO: Support named arguments in constructor pattern", p.range.first, ctx.mod);

						typecheck_pattern(
							arg.pattern,
							param_type,
							rhs_is_lvalue, // TODO Can I just pass it along?
							infallible_pattern_required,
							subst,
							ctx
						);
						arg.param_idx = i;
					}
				}
			}

			return p.type = clone_ptr(p.ctor, &ctx.mod->sema->insts);
		},
		[&](WildcardPattern &) -> Type*
		{
			assert(!"[TODO] typecheck_pattern: WildcardPattern");
		},
		[&](VarPatternUnresolved&) -> Type* { assert(!"typecheck_pattern: VarPatternUnresolved"); },
	};
}


static void typecheck_stmt(Stmt &stmt, SemaContext &ctx)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			if(s.init_expr)
			{
				TypeEnv subst;
				Type const *init_type = typecheck_subexpr(*s.init_expr, TypingHint(s.lhs->provided_type, &subst), ctx);
				typecheck_pattern(*s.lhs, *init_type, is_lvalue_expr(*s.init_expr), true, subst, ctx, s.init_expr);

				subst.materialize();
				substitute_types_in_expr(*s.init_expr, subst, ctx.mod->sema->insts);
				substitute_types_in_pattern(*s.lhs, subst, ctx.mod->sema->insts);
			}
			else
			{
				assert(!"[TODO] typecheck_stmt: LetStmt: no init expr");
			}
		},
		[&](ExprStmt const &s)
		{
			typecheck_expr(*s.expr, ctx);
		},
		[&](BlockStmt const &s)
		{
			for(Stmt &child_stmt: *s.stmts)
				typecheck_stmt(child_stmt, ctx);
		},
		[&](ReturnStmt const &s)
		{
			if(s.ret_expr)
			{
				TypeEnv subst;
				Type const *ret_expr_type = typecheck_subexpr(*s.ret_expr, TypingHint(ctx.proc->ret_type, &subst), ctx);
				try {
					unify(*ctx.proc->ret_type, *ret_expr_type, subst, UnificationMode::VALUE_ASSIGNMENT, ctx, s.ret_expr);
					substitute_types_in_expr(*s.ret_expr, subst, ctx.mod->sema->insts);
				}
				catch(ParseError const &e) {
					throw_sem_error("Invalid return expression: "s + e.what(), token_range_of(*s.ret_expr).first, ctx.mod);
				}
			}
			else
			{
				if(not is_builtin_type(*ctx.proc->ret_type, BuiltinTypeDef::UNIT))
					throw_sem_error("Return statement must return unit value", s.range.first, ctx.mod);
			}
		},
		[&](IfStmt const &s)
		{
			TypeEnv subst;
			Type const *condition_type = typecheck_subexpr(*s.condition, TypingHint(&subst), ctx);
			subst.materialize();
			substitute_types_in_expr(*s.condition, subst, ctx.mod->sema->insts);

			if(not is_builtin_type(*condition_type, BuiltinTypeDef::BOOL))
				throw_sem_error("If-condition must be boolean", s.range.first, ctx.mod);

			typecheck_stmt(*s.then, ctx);
			if(s.else_)
				typecheck_stmt(*s.else_, ctx);
		},
		[&](WhileStmt const &s)
		{
			TypeEnv subst;
			Type const *condition_type = typecheck_subexpr(*s.condition, TypingHint(&subst), ctx);
			subst.materialize();
			substitute_types_in_expr(*s.condition, subst, ctx.mod->sema->insts);

			if(not is_builtin_type(*condition_type, BuiltinTypeDef::BOOL))
				throw_sem_error("While-condition must be boolean", s.range.first, ctx.mod);

			typecheck_stmt(*s.body, ctx);
		},
		[&](MatchStmt const &s)
		{
			TypeEnv subst;
			Type const *expr_type = typecheck_subexpr(*s.expr, TypingHint(&subst), ctx);
			*expr_type | match
			{
				[&](StructType const &struct_type)
				{
					assert(struct_type.inst->is_deduction_complete());

					StructInstance *subject = struct_type.inst;
					unordered_set<StructInstance const*> matched_cases;
					bool has_wildcard = false;
					for(MatchArm &arm: *s.arms)
					{
						if(has_wildcard)
							throw_sem_error("Pattern is following wildcard pattern and is therefore unreachable", token_range_of(arm.capture).first, ctx.mod);

						TypeEnv subst;
						typecheck_pattern(arm.capture, *type_of(*s.expr), is_lvalue_expr(*s.expr), false, subst, ctx);
						substitute_types_in_pattern(arm.capture, subst, ctx.mod->sema->insts);

						if(is<WildcardPattern>(arm.capture))
							has_wildcard = true;
						else
						{
							StructInstance *arm_inst = std::get<StructType>(type_of(arm.capture)).inst;
							if(arm_inst->parent() != subject)
								throw_sem_error("Must match against a case member", token_range_of(arm.capture).first, ctx.mod);

							if(!matched_cases.insert(arm_inst).second)
								throw_sem_error("Duplicate case value", token_range_of(arm.capture).first, ctx.mod);

							arm.discr = arm_inst->case_idx();
						}

						typecheck_stmt(arm.stmt, ctx);
					}

					if((int)matched_cases.size() != subject->struct_()->num_case_members and not has_wildcard)
						throw_sem_error("Match is not exhaustive", s.range.first, ctx.mod);
				},
				[&](auto const&)
				{
					throw_sem_error("The match subject must refer to a struct type", token_range_of(*s.expr).first, ctx.mod);
				},
			};
		},
	};
}


void typecheck_struct(StructItem *struct_, SemaContext &ctx)
{
	for(Member &m: *struct_->members)
	{
		m | match
		{
			[&](Parameter &var_member)
			{
				if(Expr *default_value = var_member.default_value.try_get_expr())
				{
					TypeEnv subst;
					Type const *default_value_type = typecheck_subexpr(*default_value, TypingHint(var_member.type, &subst), ctx);
					try {
						unify(*var_member.type, *default_value_type, subst, UnificationMode::VALUE_ASSIGNMENT, ctx, default_value);
					}
					catch(ParseError const &exc) {
						throw_sem_error("Invalid default value: "s + exc.what(), var_member.range.first, ctx.mod);
					}

					subst.materialize();
					substitute_types_in_expr(*default_value, subst, ctx.mod->sema->insts);
				}
			},
			[&](StructItem *case_member)
			{
				typecheck_struct(case_member, ctx);
			},
		};
	}
}

static void typecheck(SemaContext &ctx)
{
	for(TopLevelItem &item: to_range(ctx.mod->items))
	{
		item | match
		{
			[&](ProcItem &proc)
			{
				for(auto const &[idx, param]: *proc.params | std::views::enumerate)
				{
					proc.sema->param_vars->items[idx]->type = param.type;
					if(param.default_value)
					{
						assert(!"[TODO] typecheck: ProcItem: param.default_value");
					}
				}

				if(proc.body)
				{
					ctx.proc = &proc;
					typecheck_stmt(*proc.body, ctx);
					ctx.proc = nullptr;
				}
			},
			[&](StructItem &struct_)
			{
				typecheck_struct(&struct_, ctx);
			},
			[&](AliasItem&) {},
		};
	}

	for_each_struct_instance(ctx.mod->sema->insts, [&](StructInstance *struct_)
	{
		struct_->finalize_typechecking();
	});
}


//==============================================================================
// Pass 4: Ensure there are no cycles in default value expressions
//==============================================================================
void check_default_values_in_struct(
	StructItem const *struct_,
	std::unordered_map<DefaultValueDep, VisitState> &default_value_deps,
	SemaContext const &ctx
)
{
	for(Member const &m: *struct_->members)
	{
		m | match
		{
			[&](Parameter const &var_member)
			{
				if(Expr *default_value = var_member.default_value.try_get_expr())
					check_default_value_deps(struct_, name_of(var_member, ctx.mod), default_value, default_value_deps, *ctx.mod);
			},
			[&](StructItem const *case_member)
			{
				check_default_values_in_struct(case_member, default_value_deps, ctx);
			},
		};
	}
}

void check_default_values(SemaContext const &ctx)
{
	std::unordered_map<DefaultValueDep, VisitState> default_value_deps;
	for(TopLevelItem const &item: to_range(ctx.mod->items))
	{
		item | match
		{
			[&](ProcItem const&)
			{
				// TODO
			},
			[&](StructItem const &struct_)
			{
				check_default_values_in_struct(&struct_, default_value_deps, ctx);
			},
			[&](AliasItem const&) {},
		};
	}
}

//==============================================================================
// Pass 5: Compute memory layouts of types
//==============================================================================
Type const* is_optional_ptr(StructInstance const *struct_)
{
	while(struct_->parent())
		struct_ = struct_->parent();

	if(
		struct_->struct_()->name == "Option" and
		(
			is<PointerType>(struct_->type_args().args->items[0]) or
			is<ManyPointerType>(struct_->type_args().args->items[0])
		)
	)
		return &struct_->type_args().args->items[0];

	return nullptr;
}

Type const* is_optional_ptr(Type const &type)
{
	StructType const *struct_type = std::get_if<StructType>(&type);
	if(not struct_type)
		return nullptr;

	return is_optional_ptr(struct_type->inst);
}

MemoryLayout compute_layout(Type const &type, unordered_set<TypeInstance> *parent_type_deps)
{
	return type | match
	{
		[&](BuiltinType const &t)
		{
			return get_layout(t.builtin);
		},
		[&](VarType const&) -> MemoryLayout { assert(!"compute_layout: VarType"); },
		[&](PointerType const&)
		{
			return MemoryLayout{.size = 8, .alignment = 8};
		},
		[&](ManyPointerType const&)
		{
			return MemoryLayout{.size = 8, .alignment = 8};
		},
		[&](StructType const &t)
		{
			assert(t.inst->is_deduction_complete());
			if(parent_type_deps)
				parent_type_deps->insert(t.inst);

			StructInstance *cur = t.inst;
			while(cur->parent())
				cur = cur->parent();

			if(is_optional_ptr(cur))
				return MemoryLayout{.size = 8, .alignment = 8};

			return cur->compute_own_layout();
		},
		[&](ProcType const&) -> MemoryLayout { assert(!"compute_layout: ProcType"); },
		[&](UnionType const&) -> MemoryLayout
		{
			assert(!"[TODO] compute_layout: UnionType");
		},

		[&](KnownIntType const&) -> MemoryLayout { assert(!"compute_layout: KnownIntType"); },
		[&](ProcTypeUnresolved const&) -> MemoryLayout { assert(!"compute_layout: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> MemoryLayout { assert(!"compute_layout: UnionTypeUnresolved"); },
		[&](Path const&) -> MemoryLayout { assert(!"compute_layout: Path"); },
	};
}

MemoryLayout compute_layout(Type const &type)
{
	return compute_layout(type, nullptr);
}

void compute_type_layouts(Module &mod)
{
	for(StructInstance &struct_: mod.sema->insts.struct_instances())
	{
		if(not struct_.is_concrete())
			continue;

		struct_.compute_own_layout();
	}
}


//==============================================================================
// Go!
//==============================================================================
void sema(Module &mod, Arena &arena)
{
	SemaContext ctx(mod, arena);

	LOGGER(mod.logger, on_declare_items_start);
	{
		declare_items(ctx);
	}
	LOGGER(mod.logger, on_declare_items_end);

	LOGGER(mod.logger, on_resolve_names_start);
	{
		resolve_names(ctx);
	}
	LOGGER(mod.logger, on_resolve_names_end);

	LOGGER(mod.logger, on_typecheck_start);
	{
		typecheck(ctx);
	}
	LOGGER(mod.logger, on_typecheck_end);

	{
		check_default_values(ctx);
	}

	LOGGER(mod.logger, on_start_layout_computation_pass);
	{
		compute_type_layouts(mod);
	}
	LOGGER(mod.logger, on_end_layout_computation_pass);
}


//==============================================================================
// Event logger
//==============================================================================
void EventLogger::on_declare_items_start()
{
	m_os << "<h2>Pass 1: Declare items</h2>\n";
	m_os << "<ul>\n";
}

void EventLogger::on_declare_items_end()
{
	m_os << "</ul>\n";
}

void EventLogger::on_resolve_names_start()
{
	m_os << "<h2>Pass 2: Resolve names</h2>\n";
	m_os << "<ul>\n";
}

void EventLogger::on_resolve_names_end()
{
	m_os << "</ul>\n";
}

void EventLogger::on_typecheck_start()
{
	m_os << "<h2>Pass 3: Typecheck</h2>\n";
	m_os << "<ul>\n";
}

void EventLogger::on_typecheck_end()
{
	m_os << "</ul>\n";
}

void EventLogger::on_expr_start(Expr const &expr)
{
	m_os << "<li>Expression:\n";
		m_os << "<code>"; print(expr, *m_mod, m_os); m_os << "</code>";
		m_os << "<ul>\n";
}

void EventLogger::on_expr_end()
{
		m_os << "</ul>\n";
	m_os << "</li>\n";
}

void EventLogger::on_struct_substitution_start(StructInstance *inst, TypeEnv const &subst)
{
	m_os << "<li>Struct substitution: \n";
		m_os << "<code>"; print(StructType(UNKNOWN_TOKEN_RANGE, inst), *m_mod, m_os); m_os << "</code>";
		m_os << " <small><em>(" << inst << ")</em></small>\n";

		m_os << "<ul>\n";
			m_os << "<li>Substitution:\n";
				m_os << "<ul>\n";
					for(auto &[var, type]: subst.env())
					{
						m_os << "<li>";
							print(var, *m_mod, m_os);
							m_os << " ==> ";
							print(type, *m_mod, m_os);
						m_os << "</li>";
					}
				m_os << "</ul>\n";
			m_os << "</li>\n";
}

void EventLogger::on_struct_substitution_replaced(StructInstance *inst)
{
	(void)inst;
			m_os << "<li>Replaced: ";
			m_os << "<code>"; print(StructType(UNKNOWN_TOKEN_RANGE, inst), *m_mod, m_os); m_os << "</code>";
			m_os << " <small><em>(" << inst << ")</em></small>\n";
			m_os << "</li>\n";
}

void EventLogger::on_struct_substitution_noop()
{
			m_os << "<li>NOOP</li>\n";
}

void EventLogger::on_struct_substitution_end()
{
		m_os << "</ul>\n";
	m_os << "</li>\n";
}


void EventLogger::on_register_struct(StructInstance *inst)
{
	string_view deduction_state = "(partially deduced)";
	if(inst->is_deduction_complete())
		deduction_state = "(fully deduced)";

	m_os << "<li>Register struct " << deduction_state << ": \n";
		m_os << "<code>"; print(StructType(UNKNOWN_TOKEN_RANGE, inst), *m_mod, m_os); m_os << "</code>";
		m_os << " <small><em>(" << inst << ")</em></small>\n";
	m_os << "</li>\n";
}

void EventLogger::on_register_proc(ProcInstance *inst)
{
	string_view deduction_state = "(partially deduced)";
	if(inst->is_deduction_complete())
		deduction_state = "(fully deduced)";

	m_os << "<li>Register proc " << deduction_state << ": \n";
		m_os << "<code>" << inst->proc()->name << "</code>";
		m_os << " <small><em>(" << inst << ")</em></small>\n";
	m_os << "</li>\n";
}


void EventLogger::on_start_layout_computation_pass()
{
	m_os << "<h2>Pass 4: Layout computation</h2>\n";
	m_os << "<ul>\n";
}

void EventLogger::on_end_layout_computation_pass()
{
	m_os << "</ul>\n";
}

void EventLogger::on_start_layout_computation(StructInstance *inst)
{
	m_os << "<li>Compute layout: \n";
		m_os << "<code>"; print(StructType(UNKNOWN_TOKEN_RANGE, inst), *m_mod, m_os); m_os << "</code>";
		m_os << " <small><em>(" << inst << ")</em></small>\n";

		m_os << "<ul>\n";
			m_os << "<li>Members:\n";
			m_os << "<ul>\n";
				for(Parameter const &m: inst->own_var_members())
				{
					m_os << "<li><code>";
						m_os << name_of(m, m_mod) << ": ";
						print(*m.type, *m_mod, m_os);
					m_os << "</code></li>\n";
				}
			m_os << "</ul>\n";
			m_os << "</li>\n";

			m_os << "<li>Recursive layout computations:\n";
			m_os << "<ul>\n";

				m_os.flush();
}

void EventLogger::on_end_layout_computation()
{
				m_os << "</ul>\n";
			m_os << "</li>\n";
		m_os << "</ul>\n";
	m_os << "</li>\n";
}
