#include "semantics.hpp"
#include "syntax.hpp"
#include "utils.hpp"
#include <algorithm>
#include <memory>
#include <ostream>
#include <ranges>
#include <unordered_map>
#include <variant>


using std::pair;


//==============================================================================
Expr clone(Expr const &expr, Arena &arena);
FixedArray<MatchArm>* clone(FixedArray<MatchArm> const *arms, Arena &arena);

FixedArray<Stmt>* clone(FixedArray<Stmt> const *stmts, Arena &arena);
Stmt* clone_ptr(Stmt const *stmt, Arena &arena);


Expr* clone_ptr(Expr const *expr, Arena &arena)
{
	if(not expr)
		return nullptr;

	return arena.alloc<Expr>(clone(*expr, arena));
}

DefaultValueExpr clone(DefaultValueExpr const &default_val_expr, Arena &arena)
{
	return default_val_expr | match
	{
		[&](NoDefaultValue) -> DefaultValueExpr { return NoDefaultValue(); },
		[&](ExprPending) -> DefaultValueExpr { return ExprPending(); },
		[&](Expr *expr) -> DefaultValueExpr
		{
			return clone_ptr(expr, arena);
		},
	};
}


Type clone(Type const &type, Arena &arena);

FixedArray<Type>* clone(FixedArray<Type> const *types, Arena &arena)
{
	FixedArray<Type> *result = alloc_fixed_array<Type>(types->count, arena);
	for(size_t i = 0; i < types->count; ++i)
		result->items[i] = clone(types->items[i], arena);

	return result;
}


Type* clone_ptr(Type const *type, Arena &arena)
{
	if(not type)
		return nullptr;

	return arena.alloc<Type>(clone(*type, arena));
}

TypeArgList clone(TypeArgList const &args, Arena &arena);

Type clone(Type const &type, Arena &arena)
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
				.kind = t.kind,
				.pointee = clone_ptr(t.pointee, arena),
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
				.ret = clone_ptr(t.ret, arena),
				.params = clone(t.params, arena),
			};
		},
		[&](ProcType const &t) -> Type
		{
			return t;
		},
		[&](UnionTypeUnresolved const &t) -> Type
		{
			assert(!"clone(Type): UnionTypeUnresolved");
			return UnionTypeUnresolved{
				.range = t.range,
				.alternatives = clone(t.alternatives, arena),
			};
		},
		[&](UnionType const &t) -> Type
		{
			return t;
		},
		[&](VarType const &t) -> Type
		{
			return t;
		},
		[&](Path const&) -> Type { UNREACHABLE; },
		[&](InlineStructType const&) -> Type { UNREACHABLE; },
	};
}

TypeArgList clone(TypeArgList const &args, Arena &arena)
{
	return TypeArgList{
		.args = clone(args.args, arena),
		.occurring_vars = args.occurring_vars,
		.has_type_deduction_vars = args.has_type_deduction_vars,
	};
}

Argument clone(Argument const &arg, Arena &arena);

FixedArray<Argument>* clone(FixedArray<Argument> const *args, Arena &arena)
{
	FixedArray<Argument> *result = alloc_fixed_array<Argument>(args->count, arena);
	for(size_t i = 0; i < args->count; ++i)
		result->items[i] = clone(args->items[i], arena);

	return result;
}

Expr clone(Expr const &expr, Arena &arena)
{
	return expr | match
	{
		[&](IntLiteralExpr const &e) -> Expr
		{
			return IntLiteralExpr{
				.range = e.range,
				.value = e.value,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](BoolLiteralExpr const &e) -> Expr
		{
			return BoolLiteralExpr{
				.range = e.range,
				.value = e.value,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](StringLiteralExpr const &e) -> Expr
		{
			return StringLiteralExpr{
				.range = e.range,
				.kind = e.kind,
				.value = e.value,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](UnaryExpr const &e) -> Expr
		{
			return UnaryExpr{
				.range = e.range,
				.sub = clone_ptr(e.sub, arena),
				.op = e.op,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](BinaryExpr const &e) -> Expr
		{
			return BinaryExpr{
				.range = e.range,
				.left = clone_ptr(e.left, arena),
				.right = clone_ptr(e.right, arena),
				.op = e.op,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](AddressOfExpr const &e) -> Expr
		{
			return AddressOfExpr{
				.range = e.range,
				.object = clone_ptr(e.object, arena),
				.mutability = e.mutability,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](DerefExpr const &e) -> Expr
		{
			return DerefExpr{
				.range = e.range,
				.addr = clone_ptr(e.addr, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](IndexExpr const &e) -> Expr
		{
			return IndexExpr{
				.range = e.range,
				.addr = clone_ptr(e.addr, arena),
				.index = clone_ptr(e.index, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](MemberAccessExpr const &e) -> Expr
		{
			return MemberAccessExpr{
				.range = e.range,
				.object = clone_ptr(e.object, arena),
				.member = e.member,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](AssignmentExpr const &e) -> Expr
		{
			return AssignmentExpr{
				.range = e.range,
				.lhs = clone_ptr(e.lhs, arena),
				.rhs = clone_ptr(e.rhs, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](AsExpr const &e) -> Expr
		{
			return AsExpr{
				.range = e.range,
				.src_expr = clone_ptr(e.src_expr, arena),
				.target_type = clone_ptr(e.target_type, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](ConstructorExpr const &e) -> Expr
		{
			return ConstructorExpr{
				.range = e.range,
				.ctor = clone_ptr(e.ctor, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](ProcExpr const &e) -> Expr
		{
			return ProcExpr{
				.range = e.range,
				.inst = e.inst,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](CallExpr const &e) -> Expr
		{
			return CallExpr{
				.range = e.range,
				.callable = clone_ptr(e.callable, arena),
				.args = clone(e.args, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](SizeOfExpr const &e) -> Expr
		{
			return SizeOfExpr{
				.range = e.range,
				.subject = clone_ptr(e.subject, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](MakeExpr const &e) -> Expr
		{
			return MakeExpr{
				.range = e.range,
				.init = clone_ptr(e.init, arena),
				.addr = clone_ptr(e.addr, arena),
				.type = clone_ptr(e.type, arena),
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
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](UnionInitExpr const &e) -> Expr
		{
			return UnionInitExpr{
				.range = e.range,
				.alt_expr = clone_ptr(e.alt_expr, arena),
				.alt_type = clone_ptr(e.alt_type, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
	};
}

Argument clone(Argument const &arg, Arena &arena)
{
	return Argument{
		.range = arg.range,
		.expr = clone(arg.expr, arena),
		.name = arg.name,
		.param_idx = arg.param_idx,
	};
}

Parameter clone(Parameter const &param, Arena &arena)
{
	return Parameter{
		.range = param.range,
		.type = clone_ptr(param.type, arena),
		.default_value = clone(param.default_value, arena),
	};
}

Pattern* clone_ptr(Pattern const *pattern, Arena &arena);
FixedArray<PatternArgument>* clone(FixedArray<PatternArgument> const *args, Arena &arena);

Pattern clone(Pattern const &pattern, Arena &arena)
{
	return pattern | match
	{
		[&](VarPatternUnresolved const &p)
		{
			return Pattern(
				VarPatternUnresolved(p.range, p.name, p.mutability),
				clone_ptr(pattern.provided_type, arena)
			);
		},
		[&](VarPattern const &p)
		{
			return Pattern(
				VarPattern(p.range, p.var, clone_ptr(p.type, arena)),
				clone_ptr(pattern.provided_type, arena)
			);
		},
		[&](DerefPattern const &p)
		{
			return Pattern(
				DerefPattern(p.range, clone_ptr(p.sub, arena), clone_ptr(p.type, arena)),
				clone_ptr(pattern.provided_type, arena)
			);
		},
		[&](AddressOfPattern const &p)
		{
			return Pattern(
				AddressOfPattern(
					p.range,
					clone_ptr(p.sub, arena),
					p.mutability,
					clone_ptr(p.type, arena)
				),
				clone_ptr(pattern.provided_type, arena)
			);
		},
		[&](ConstructorPattern const &p)
		{
			return Pattern(
				ConstructorPattern(
					p.range,
					clone_ptr(p.ctor, arena),
					clone(p.args, arena),
					p.has_parens,
					clone_ptr(p.type, arena)
				),
				clone_ptr(pattern.provided_type, arena)
			);
		},
		[&](WildcardPattern const &p)
		{
			return Pattern(
				WildcardPattern(p.range, clone_ptr(p.type, arena)),
				clone_ptr(pattern.provided_type, arena)
			);
		}
	};
}

Pattern* clone_ptr(Pattern const *pattern, Arena &arena)
{
	if(not pattern)
		return nullptr;

	return arena.alloc<Pattern>(clone(*pattern, arena));
}

PatternArgument clone(PatternArgument const &pat_arg, Arena &arena)
{
	return PatternArgument{
		.pattern = clone(pat_arg.pattern, arena),
		.param_name = pat_arg.param_name,
		.param_idx = pat_arg.param_idx,
	};
}

FixedArray<PatternArgument>* clone(FixedArray<PatternArgument> const *args, Arena &arena)
{
	FixedArray<PatternArgument> *result = alloc_fixed_array<PatternArgument>(args->count, arena);
	for(size_t i = 0; i < args->count; ++i)
		result->items[i] = clone(args->items[i], arena);

	return result;
}


Stmt clone(Stmt const &stmt, Arena &arena)
{
	return stmt | match
	{
		[&](LetStmt const &s) -> Stmt
		{
			return LetStmt{
				.range = s.range,
				.lhs = clone_ptr(s.lhs, arena),
				.init_expr = clone_ptr(s.init_expr, arena),
			};
		},
		[&](ExprStmt const &s) -> Stmt
		{
			return ExprStmt(s.range, clone_ptr(s.expr, arena));
		},
		[&](BlockStmt const &s) -> Stmt
		{
			return BlockStmt(s.range, clone(s.stmts, arena));
		},
		[&](ReturnStmt const &s) -> Stmt
		{
			return ReturnStmt(s.range, clone_ptr(s.ret_expr, arena));
		},
		[&](IfStmt const &s) -> Stmt
		{
			return IfStmt(
				s.range,
				clone_ptr(s.condition, arena),
				clone_ptr(s.then, arena),
				clone_ptr(s.else_, arena)
			);
		},
		[&](WhileStmt const &s) -> Stmt
		{
			return WhileStmt(
				s.range,
				clone_ptr(s.condition, arena),
				clone_ptr(s.body, arena)
			);
		},
		[&](MatchStmt const &s) -> Stmt
		{
			return MatchStmt(
				s.range,
				clone_ptr(s.expr, arena),
				clone(s.arms, arena)
			);
		},
	};
}

Stmt* clone_ptr(Stmt const *stmt, Arena &arena)
{
	if(not stmt)
		return nullptr;

	return arena.alloc<Stmt>(clone(*stmt, arena));
}

FixedArray<Stmt>* clone(FixedArray<Stmt> const *stmts, Arena &arena)
{
	FixedArray<Stmt> *result = alloc_fixed_array<Stmt>(stmts->count, arena);
	for(size_t i = 0; i < stmts->count; ++i)
		result->items[i] = clone(stmts->items[i], arena);

	return result;
}

MatchArm clone(MatchArm const &arm, Arena &arena)
{
	return MatchArm{
		.capture = clone(arm.capture, arena),
		.stmt = clone(arm.stmt, arena),
		.discr = arm.discr,
	};
}

FixedArray<MatchArm>* clone(FixedArray<MatchArm> const *arms, Arena &arena)
{
	FixedArray<MatchArm> *result = alloc_fixed_array<MatchArm>(arms->count, arena);
	for(size_t i = 0; i < arms->count; ++i)
		result->items[i] = clone(arms->items[i], arena);

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


//==============================================================================
// Scope
//==============================================================================
Scope* Scope::new_child(bool child_accepts_item_decls)
{
	children.push_back(std::make_unique<Scope>(mod, child_accepts_item_decls, this));
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

void Scope::declare_type_var(TypeParameter *def)
{
	declare(def->name, def, def->range);
}

void Scope::declare_struct(StructItem *struct_)
{
	if(not accept_item_decls)
		return parent->declare_struct(struct_);

	declare(struct_->name, struct_, struct_->range);
}

void Scope::declare_proc(ProcItem *proc)
{
	if(not accept_item_decls)
		return parent->declare_proc(proc);

	declare(proc->name, proc, proc->range);
}

void Scope::declare_alias(AliasItem *alias)
{
	if(not accept_item_decls)
		return parent->declare_alias(alias);

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
static void substitute(
	Type &type,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
);
static void substitute(TypeArgList &args, TypeEnv const &env, InstanceRegistry &registry, SubstitutionMode mode);

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

	if(a.params->count != b.params->count)
		return false;

	for(size_t i = 0; i < a.params->count; ++i)
	{
		if(not equiv(a.params->items[i], b.params->items[i]))
			return false;
	}

	return true;
}

bool operator == (UnionInstanceKey const &a, UnionInstanceKey const &b)
{
	if(a.alternatives.size() != b.alternatives.size())
		return false;

	for(size_t i = 0; i < a.alternatives.size(); ++i)
	{
		if(not equiv(*a.alternatives[i], *b.alternatives[i]))
			return false;
	}

	return true;
}


static bool gather_type_vars(Type const &type, unordered_set<VarType> &type_vars, bool deduction_vars_only = false)
{
	return type | match
	{
		[&](BuiltinType const&) { return false; },
		[&](KnownIntType const&) { return false; },
		[&](PointerType const &t)
		{
			return gather_type_vars(*t.pointee, type_vars, deduction_vars_only);
		},
		[&](ProcType const&) -> bool
		{
			assert(!"gather_type_vars: ProcType");
		},
		[&](StructType const &t)
		{
			bool parent_has_type_deduction_vars = false;
			if(t.inst->parent())
				parent_has_type_deduction_vars = gather_type_vars(Type(StructType(UNKNOWN_TOKEN_RANGE, t.inst->parent())), type_vars, deduction_vars_only);

			for(VarType v: t.inst->type_args().occurring_vars)
				gather_type_vars(v, type_vars, deduction_vars_only);

			return parent_has_type_deduction_vars || t.inst->type_args().has_type_deduction_vars;
		},
		[&](UnionType const &t) -> bool
		{
			if(not deduction_vars_only or t.inst->has_type_deduction_vars())
			{
				for(VarType v: t.inst->occurring_vars())
					gather_type_vars(v, type_vars, deduction_vars_only);
			}

			return t.inst->has_type_deduction_vars();
		},
		[&](VarType const &t)
		{
			if(std::holds_alternative<TypeDeductionVar>(t) or not deduction_vars_only)
				type_vars.insert(t);

			return std::holds_alternative<TypeDeductionVar>(t);
		},
		[&](ProcTypeUnresolved const&) -> bool { assert(!"gather_type_vars: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> bool { assert(!"gather_type_vars: UnionTypeUnresolved"); },
		[&](Path const&) -> bool { assert(!"gather_type_vars: Path"); },
		[&](InlineStructType const&) -> bool { assert(!"gather_type_vars: InlineStructType"); },
	};
}

static void gather_type_deduction_vars(Type const &type, unordered_set<VarType> &type_vars)
{
	gather_type_vars(type, type_vars, true);
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
	return add_struct_instance(StructInstance(struct_, type_arg_list, parent, next_struct_id(), this));
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

	return add_struct_instance(StructInstance(struct_, type_args, parent, next_struct_id(), this));
}

StructInstance* InstanceRegistry::get_struct_instance(
	StructItem const *struct_,
	TypeArgList const &type_args,
	TypeEnv const &subst,
	StructInstance *NULLABLE parent,
	SubstitutionMode mode
)
{
	void *original_mem_pos = m_arena.current_ptr();
	TypeArgList new_args = clone(type_args, m_arena);
	substitute(new_args, subst, *this, mode);

	auto it = m_struct_instances.find(StructInstanceKey(struct_, new_args.args, parent));
	if(it != m_struct_instances.end())
	{
		// Free `new_args` since we only needed it for lookup
		m_arena.set_current_ptr(original_mem_pos);
		return &it->second;
	}

	return add_struct_instance(StructInstance(struct_, new_args, parent, next_struct_id(), this));
}

StructInstance* InstanceRegistry::add_struct_instance(StructInstance &&new_inst)
{
	StructInstanceKey key = new_inst.key();
	StructInstance *inst = &m_struct_instances.emplace(key, std::move(new_inst)).first->second;
	LOGGER(m_mod.logger, on_struct_register, inst);

	for(InstanceRegistryListener *l: m_listeners)
		l->on_new_struct_instance(inst);

	return inst;
}

std::generator<StructInstance&> InstanceRegistry::struct_instances()
{
	for(auto &[_, struct_]: m_struct_instances)
		co_yield struct_;
}

std::generator<UnionInstance&> InstanceRegistry::union_instances()
{
	for(auto &[_, union_]: m_union_instances)
		co_yield union_;
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
	TypeEnv const &subst,
	SubstitutionMode mode
)
{
	void *original_mem_pos = m_arena.current_ptr();
	TypeArgList new_args = clone(type_args, m_arena);
	substitute(new_args, subst, *this, mode);

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
	LOGGER(m_mod.logger, on_proc_register, inst);

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
	TypeEnv const &subst,
	SubstitutionMode mode
)
{
	void *original_mem_pos = m_arena.current_ptr();

	// Substitute params
	FixedArray<Type> *new_params = clone(params, m_arena);
	for(Type &new_param: *new_params)
		substitute(new_param, subst, *this, mode);

	// Substitute return type
	Type *new_ret = clone_ptr(ret, m_arena);
	substitute(*new_ret, subst, *this, mode);

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

UnionInstance* InstanceRegistry::get_union_instance(vector<Type const*> &&alternatives)
{
	UnionInstanceKey key{alternatives};
	auto it = m_union_instances.find(key);
	if(it != m_union_instances.end())
		return &it->second;

	return add_union_instance(std::move(alternatives));
}

bool operator < (Type const &a, Type const &b);

UnionInstance* InstanceRegistry::add_union_instance(vector<Type const*> &&alternatives)
{
	unordered_set<VarType> occurring_vars;
	bool has_type_deduction_vars = false;
	for(Type const *alt: alternatives)
	{
		if(gather_type_vars(*alt, occurring_vars))
			has_type_deduction_vars = true;
	}

	UnionInstanceKey key(alternatives);
	UnionInstance *inst = &m_union_instances.emplace(
		key,
		UnionInstance(std::move(alternatives), std::move(occurring_vars), has_type_deduction_vars)
	).first->second;

	for(InstanceRegistryListener *l: m_listeners)
		l->on_new_union_instance(inst);

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
	FixedArray<TypeParameter> const *type_params,
	FixedArray<Type> const *type_args,
	TypeEnv &result,
	InstanceRegistry &registry)
{
	for(size_t i = 0; i < type_params->count; ++i)
	{
		TypeParameterVar type_param(UNKNOWN_TOKEN_RANGE, &type_params->items[i]);
		Type cloned = type_args->items[i];

		// See tests struct_recursive_type_param_<N> for why we call substitute() and perform the
		// type_var_occurs_in() check
		substitute(cloned, result, registry, BestEffortSubstitution());
		if(not equiv(type_param, cloned))
		{
			if(type_var_occurs_in(type_param, cloned))
				throw_sem_error(
					"The type argument " + str(type_args->items[i], registry.mod()) + " causes the type term to grow indefinitely",
					token_range_of(type_args->items[i]).first,
					&registry.mod()
				);

			result.add(type_param, cloned);
		}
	}
}

static TypeEnv create_type_env(
	FixedArray<TypeParameter> const *type_params,
	FixedArray<Type> const *type_args,
	InstanceRegistry &registry)
{
	TypeEnv env;
	create_type_env(type_params, type_args, env, registry);
	return env;
}


//--------------------------------------------------------------------
// StructInstance
//--------------------------------------------------------------------
static Type* mk_builtin_type(BuiltinTypeDef builtin, Arena &arena);
static MemoryLayout get_layout(BuiltinTypeDef type);
MemoryLayout compute_layout(Type const &type, unordered_set<TypeInstance> *parent_type_deps);
static void substitute_types_in_expr(
	Expr &expr,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
);


Module const& StructInstance::mod() const
{
	return m_registry->mod();
}

TypeEnv StructInstance::create_type_env() const
{
	TypeEnv env;
	if(m_parent)
		env = m_parent->create_type_env();

	::create_type_env(m_struct->type_params, m_type_args.args, env, *m_registry);

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
					.type = clone_ptr(var_member.type, m_registry->arena()),
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
				substitute(*inst_var_member.type, env, *m_registry, BestEffortSubstitution());

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
			ctor_param_types->items[idx] = clone(*var_member.type, m_registry->arena());
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

	LOGGER(m_registry->mod().logger, on_struct_layout_computation_start, this);

	m_layout_state = LayoutComputationState::IN_PROGRESS;
	m_own_layout = MemoryLayout{};

	// Reserve space for the discriminator
	if(m_struct->num_case_members > 0)
	{
		BuiltinTypeDef discriminator_type = smallest_int_type_for(m_struct->num_case_members);
		m_discriminator_type = mk_builtin_type(discriminator_type, m_registry->arena());
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

	LOGGER(m_registry->mod().logger, on_struct_layout_computation_end);

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
				default_value = clone_ptr(default_value, m_registry->arena());
				substitute_types_in_expr(*default_value, env, *m_registry, BestEffortSubstitution());
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
// UnionInstance
//--------------------------------------------------------------------
void flatten_union_alternatives(vector<Type const*> &alternatives)
{
	auto it = alternatives.begin();
	while(it != alternatives.end())
	{
		if(UnionType const *sub_union = std::get_if<UnionType>(*it))
		{
			it = alternatives.erase(it);
			it = alternatives.insert(
				it,
				sub_union->inst->alternatives().begin(),
				sub_union->inst->alternatives().end()
			);
			it += sub_union->inst->alternatives().size();
		}
		else
			++it;
	}
}

void canonicalize_union_alternatives(vector<Type const*> &alternatives)
{
	// 1. Flatten
	// 2. Sort
	// 3. Remove duplicates

	flatten_union_alternatives(alternatives);

	std::ranges::sort(alternatives, [](Type const *a, Type const *b)
	{
		return *a < *b;
	});

	auto new_end = std::ranges::unique(alternatives, [](Type const *a, Type const *b)
	{
		return equiv(*a, *b);
	}).begin();
	alternatives.erase(new_end, alternatives.end());
}

vector<Type const*> canonicalize_union_alternatives(FixedArray<Type> *alternatives)
{
	vector<Type const*> result;
	result.reserve(alternatives->count);
	for(Type const &alt: *alternatives)
		result.push_back(&alt);

	canonicalize_union_alternatives(result);

	return result;
}

MemoryLayout UnionInstance::layout()
{
	assert(is_concrete());

	compute_properties();

	if(m_layout_state == LayoutComputationState::DONE)
		return *m_layout;

	assert(m_layout_state != LayoutComputationState::IN_PROGRESS);
	m_layout_state = LayoutComputationState::IN_PROGRESS;

	BuiltinTypeDef discr_type = smallest_int_type_for(m_alternatives.size());
	m_layout = MemoryLayout();
	m_layout->extend(get_layout(discr_type));

	MemoryLayout alt_layouts;
	for(Type const *alt: m_alternatives)
		alt_layouts.extend(compute_layout(*alt, &m_type_deps));

	m_layout->extend(alt_layouts);
	m_layout_state = LayoutComputationState::DONE;

	return *m_layout;
}

void UnionInstance::compute_properties()
{
	if(m_properties_computed)
		return;

	for(auto const &[idx, alt]: m_alternatives | std::views::enumerate)
		m_alt_to_idx.emplace(*alt, idx);

	m_properties_computed = true;
}


//--------------------------------------------------------------------
// ProcInstance
//--------------------------------------------------------------------
static Type* create_proc_type(ProcInstance *proc, TypeEnv const &env, InstanceRegistry &instances);

TypeEnv ProcInstance::create_type_env() const
{
	TypeEnv env;
	::create_type_env(m_proc->type_params, m_type_args.args, env, *m_registry);
	return env;
}

void ProcInstance::compute_dependent_properties()
{
	TypeEnv env = create_type_env();
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
		Parameter new_param = clone(param, instances.arena());
		substitute(*new_param.type, env, instances, BestEffortSubstitution());
		if(Expr *default_value = new_param.default_value.try_get_expr())
			substitute_types_in_expr(*default_value, env, instances, BestEffortSubstitution());

		ctor_params->items[idx] = *new_param.type;
	}

	Type *new_ret = clone_ptr(proc->ret_type, instances.arena());
	substitute(*new_ret, env, instances, BestEffortSubstitution());

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

// Holds all the state needed during semantic analysis
struct SemaContext
{
	SemaContext(Module &mod, Arena &arena) :
		mod(&mod),
		arena(arena) {}

	Module *mod;
	Arena &arena;
	ProcItem *NULLABLE proc = nullptr; // The current procedure being analyzed

	TypeDeductionVar new_type_deduction_var()
	{
		return TypeDeductionVar(next_deduction_id++);
	}

	uint32_t next_deduction_id = 0;
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
	struct_->sema = ctx.arena.alloc<Struct>(parent, scope->new_child(true));
	struct_->sema->depth = parent ? parent->sema->depth + 1 : 0;

	// Declare type parameters
	size_t num_type_params = struct_->type_params->count;
	for(size_t i = 0; i < num_type_params; ++i)
	{
		TypeParameter &type_param = struct_->type_params->items[i];
		// TODO Produce error if the type parameter has the same name as the struct
		struct_->sema->type_scope->declare_type_var(&type_param);
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

		struct_->sema->ctor_params = alloc_fixed_array<Parameter const*>(param_count, ctx.arena);
		for(auto const &[idx, var_member]: all_var_members(struct_) | std::views::enumerate)
			struct_->sema->ctor_params->items[idx] = &var_member;
	}
}

static void declare_items(SemaContext &ctx)
{
	Module *mod = ctx.mod;
	mod->sema = std::make_unique<SemaModule>(std::make_unique<Scope>(ctx.mod, true), ctx.arena);

	for(TopLevelItem &item: to_range(ctx.mod->items))
	{
		item | match
		{
			[&](ProcItem &proc)
			{
				proc.sema = ctx.arena.alloc<Proc>(mod->sema->globals->new_child(true));
				proc.sema->param_vars = alloc_fixed_array<Var*>(proc.params->count, ctx.arena);
				for(auto const& [idx, param]: *proc.params | std::views::enumerate)
				{
					Var *param_var = proc.sema->scope->declare_var(name_of(param, mod), IsMutable::NO, param.range);
					proc.sema->param_vars->items[idx] = param_var;
				}

				size_t num_type_params = proc.type_params->count;
				for(size_t i = 0; i < num_type_params; ++i)
				{
					TypeParameter &type_param = proc.type_params->items[i];
					proc.sema->scope->declare_type_var(&type_param);
				}

				mod->sema->globals->declare_proc(&proc);
			},
			[&](StructItem &struct_)
			{
				declare_struct_item(&struct_, nullptr, mod->sema->globals.get(), ctx);
			},
			[&](AliasItem &alias)
			{
				alias.sema = ctx.arena.alloc<Alias>(mod->sema->globals->new_child(false));

				size_t num_type_params = alias.type_params->count;
				for(size_t i = 0; i < num_type_params; ++i)
				{
					TypeParameter &type_param = alias.type_params->items[i];
					alias.sema->scope->declare_type_var(&type_param);
				}

				mod->sema->globals->declare_alias(&alias);
			},
		};
	}
}



//==============================================================================
// Pass 2: Path resolution
//==============================================================================
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
	FixedArray<Type> *resolved_type_args = alloc_fixed_array<Type>(num_type_params, ctx.arena);
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
		new (&resolved_type_args->items[cur_idx++]) Type(VarType(ctx.new_type_deduction_var()));

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
			else if(name == "!")
				name = "Result";

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
			size_t num_type_params = struct_->type_params->count;
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

			size_t num_type_params = proc->type_params->count;
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

			size_t num_type_params = alias->type_params->count;
			if(path.type_args->count > num_type_params)
				throw_sem_error("Too many type arguments", path.range.first, ctx.mod);

			FixedArray<Type> *resolved_type_args = resolve_type_args(path.type_args, num_type_params, scope, ctx);
			TypeEnv env = create_type_env(alias->type_params, resolved_type_args, ctx.mod->sema->insts);

			Type type = clone(*alias->aliased_type, ctx.arena);
			substitute(type, env, ctx.mod->sema->insts, BestEffortSubstitution());

			if(path.child)
			{
				StructType const *struct_type = std::get_if<StructType>(&type);
				if(not struct_type)
					throw_sem_error("Type has no members", path.range.first, ctx.mod);

				return resolve_path(*path.child, struct_type->inst, scope, ctx);
			}

			return type;
		},
		[&](TypeParameter *type_var) -> variant<Expr, Type>
		{
			if(path.type_args->count)
				throw_sem_error("Cannot apply type arguments to type variable", path.range.first, ctx.mod);

			if(path.child)
				throw_sem_error("Member selection of type parameters not supported", path.child->range.first, ctx.mod);

			return VarType(TypeParameterVar(path.range, type_var));
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
					.object = ctx.arena.alloc<Expr>(result),
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
				.ctor = ctx.arena.alloc<Type>(type),
			};

			if(StructType const *struct_type = std::get_if<StructType>(&type))
			{
				if(struct_type->inst->struct_()->ctor_without_parens)
				{
					expr = CallExpr{
						.range = token_range_of(expr),
						.callable = ctx.arena.alloc<Expr>(expr),
						.args = alloc_fixed_array<Argument>(0, ctx.arena),
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
		[&](ProcTypeUnresolved &t)
		{
			for(Type &param: *t.params)
				resolve_type(param, scope, ctx);

			resolve_type(*t.ret, scope, ctx);

			assert(!"[TODO] resolve_type: ProcTypeUnresolved");
			//mod.sema->inst_reg.get_proc_type_instance
		},
		[&](UnionTypeUnresolved &t)
		{
			for(Type &alt: *t.alternatives)
				resolve_type(alt, scope, ctx);

			vector<Type const*> canonical_alts = canonicalize_union_alternatives(t.alternatives);

			type = UnionType{
				.range = t.range,
				.inst = ctx.mod->sema->insts.get_union_instance(std::move(canonical_alts)),
			};
		},
		[&](Path &path)
		{
			type = resolve_path_to_type(path, scope, ctx);
		},
		[&](InlineStructType &t)
		{
			declare_struct_item(t.struct_, nullptr, scope, ctx);
			StructInstance *inst = ctx.mod->sema->insts.get_struct_instance(t.struct_, nullptr, nullptr);

			type = StructType(t.range, inst);
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
		[&](UnionInitExpr const &e)
		{
			resolve_expr(*e.alt_expr, scope, ctx);
		},
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
			Scope *block_scope = scope->new_child(true);
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
				Scope *arm_scope = scope->new_child(true);
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

static void substitute(TypeArgList &args, TypeEnv const &env, InstanceRegistry &registry, SubstitutionMode mode)
{
	for(size_t i = 0; i < args.args->count; ++i)
		substitute(args.args->items[i], env, registry, mode);

	// Update args.occurring_vars and args.has_type_deduction_vars
	args.occurring_vars.clear();
	args.has_type_deduction_vars = false;
	for(Type const &type: *args.args)
	{
		if(gather_type_vars(type, args.occurring_vars))
			args.has_type_deduction_vars = true;
	}
}

void validate_unsubstituted_var(VarType var, SubstitutionMode mode, Module const &mod)
{
	if(is<FullSubsitution>(mode)) {
		assert(!"VarType has not been substituted");
	}

	if(FullDeductionSubsitution *m = std::get_if<FullDeductionSubsitution>(&mode))
	{
		if(is<TypeDeductionVar>(var))
			throw_sem_error("Type parameter could not be deduced", m->region_being_substituted.first, &mod);
	}
}

// Make sure that the kinds of variables that must be substituted actually have been substituted
void validate_unsubstituted_vars(
	unordered_set<VarType> const &vars,
	bool has_type_deduction_vars,
	TypeEnv const &env,
	SubstitutionMode mode,
	Module const &mod
)
{
	if(
		(is<FullDeductionSubsitution>(mode) and has_type_deduction_vars)
		or (is<FullSubsitution>(mode) and vars.size())
	)
	{
		for(VarType var: vars)
		{
			if(not env.try_lookup(var))
				validate_unsubstituted_var(var, mode, mod);
		}
	}
}

// If `inst` is deduction complete, apply `env` to it directly and return `inst`.
// Otherwise, leave `inst` unchanged and return the StructInstance that corresponds to `inst` where
// `env` has been applied to the type args of `inst`
static StructInstance* substitute_types_in_struct(
	StructInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode,
	bool *modified = nullptr
)
{
	LOGGER(registry.mod().logger, on_struct_substitution_start, inst);
	LOGGER(registry.mod().logger, on_data, env);

	StructInstance *new_parent = nullptr;
	bool parent_modified = false;
	if(inst->parent())
		new_parent = substitute_types_in_struct(inst->parent(), env, registry, mode, &parent_modified);

	if(parent_modified or have_common_vars(inst->type_args().occurring_vars, env))
	{
		inst = registry.get_struct_instance(inst->struct_(), inst->type_args(), env, new_parent, mode);

		LOGGER(registry.mod().logger, on_struct_substitution_replaced, inst);
		if(modified) *modified = true;
	}
	else
	{
		validate_unsubstituted_vars(
			inst->type_args().occurring_vars,
			inst->type_args().has_type_deduction_vars,
			env, mode, registry.mod()
		);

		LOGGER(registry.mod().logger, on_struct_substitution_noop);
		if(modified) *modified = false;
	}

	LOGGER(registry.mod().logger, on_struct_substitution_end);

	return inst;
}

static ProcInstance* substitute_types_in_proc(
	ProcInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
)
{
	if(have_common_vars(inst->type_args().occurring_vars, env))
		inst = registry.get_proc_instance(inst->proc(), inst->type_args(), env, mode);
	else
		validate_unsubstituted_vars(
			inst->type_args().occurring_vars,
			inst->type_args().has_type_deduction_vars,
			env, mode, registry.mod()
		);


	return inst;
}

static ProcTypeInstance* substitute_types_in_proc_type(
	ProcTypeInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
)
{
	if(have_common_vars(inst->occurring_vars, env))
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
	if(have_common_vars(inst->occurring_vars(), env))
	{
		vector<Type const*> new_alts;
		new_alts.reserve(inst->alternatives().size());
		for(Type const *alt: inst->alternatives())
		{
			Type *new_alt = clone_ptr(alt, registry.arena());
			substitute(*new_alt, env, registry, mode);
			new_alts.push_back(new_alt);
		}

		canonicalize_union_alternatives(new_alts);

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


static void substitute(
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
			type = materialize_known_int(t);
		},
		[&](PointerType &t)
		{
			substitute(*t.pointee, env, registry, mode);
		},
		[&](StructType &t)
		{
			t.inst = substitute_types_in_struct(t.inst, env, registry, mode);
		},
		[&](ProcType &t)
		{
			t.inst = substitute_types_in_proc_type(t.inst, env, registry, mode);
			t.callable | match
			{
				[&](ProcInstance *proc)
				{
					t.callable = substitute_types_in_proc(proc, env, registry, mode);
				},
				[&](StructInstance *struct_)
				{
					t.callable = substitute_types_in_struct(struct_, env, registry, mode);
				},
			};
		},
		[&](UnionType &t)
		{
			t.inst = substitute_types_in_union(t.inst, env, registry, mode);
		},
		[&](VarType &t)
		{
			if(Type const *mapped_type = env.try_lookup(t))
			{
				type = clone(*mapped_type, registry.arena());

				// Apply substitution recursively.
				// This is not how substitution is traditionally defined for e.g. first-order logic,
				// but is easier to implement. See note for the unify() function.
				substitute(type, env, registry, mode);
			}
			else
				validate_unsubstituted_var(t, mode, registry.mod());
		},
		[&](ProcTypeUnresolved const&) { UNREACHABLE; },
		[&](UnionTypeUnresolved const&) { UNREACHABLE; },
		[&](Path const&) { UNREACHABLE; },
		[&](InlineStructType const&) { UNREACHABLE; },
	};
}

static void substitute_types_in_expr(Expr &expr, TypeEnv const &env, InstanceRegistry &registry, SubstitutionMode mode)
{
	if(Type *expr_type = type_of(expr))
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
		[&](Path&) { assert(!"substitute_types_in_expr: Path"); },
	};
}

static void substitute_types_in_pattern(
	Pattern &pattern,
	TypeEnv const &subst,
	InstanceRegistry &registry,
	SubstitutionMode mode
)
{
	substitute(type_of(pattern), subst, registry, mode);

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

static bool integer_assignable_to(BuiltinTypeDef target_type, Int128 val)
{
	switch(target_type)
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
			return ta.kind == tb.kind and ta.mutability == tb.mutability and equiv(*ta.pointee, *tb.pointee);
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
			return ta.inst == tb.inst;
		},
		[&](ProcTypeUnresolved const&) -> bool { assert(!"equiv: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> bool { assert(!"equiv: UnionTypeUnresolved"); },
		[&](Path const&) -> bool { assert(!"equiv: Path"); },
		[&](InlineStructType const&) -> bool { assert(!"equiv: InlineStructType"); },
	};
}

bool operator < (Type const &a, Type const &b)
{
	if(a.index() != b.index())
		return a.index() < b.index();

	return a | match
	{
		[&](BuiltinType const &ta)
		{
			BuiltinType const &tb = std::get<BuiltinType>(b);
			return ta.builtin < tb.builtin;
		},
		[&](KnownIntType const &ta)
		{
			KnownIntType const &tb = std::get<KnownIntType>(b);
			return std::tie(ta.low, ta.high) < std::tie(tb.low, tb.high);
		},
		[&](VarType const &ta)
		{
			VarType const &tb = std::get<VarType>(b);
			if(ta.index() != tb.index())
				return ta.index() < tb.index();

			return ta | match
			{
				[&](TypeParameterVar va)
				{
					// TODO Use something other than pointers to make comparison deterministic
					return va.def < std::get<TypeParameterVar>(tb).def;
				},
				[&](TypeDeductionVar va)
				{
					return va.id < std::get<TypeDeductionVar>(tb).id;
				},
			};
		},
		[&](PointerType const &ta)
		{
			PointerType const &tb = std::get<PointerType>(b);
			return std::tie(ta.kind, ta.mutability, *ta.pointee) < std::tie(tb.kind, tb.mutability, *tb.pointee);
		},
		[&](StructType const &ta)
		{
			StructType const &tb = std::get<StructType>(b);

			// TODO Use something other than pointers to make comparison deterministic
			return ta.inst < tb.inst;
		},
		[&](ProcType const &ta)
		{
			ProcType const &tb = std::get<ProcType>(b);

			// TODO Use something other than pointers to make comparison deterministic
			return ta.inst < tb.inst;
		},
		[&](UnionType const &ta)
		{
			UnionType const &tb = std::get<UnionType>(b);

			// TODO Use something other than pointers to make comparison deterministic
			return ta.inst < tb.inst;
		},
		[&](ProcTypeUnresolved const&) -> bool { assert(!"operator <: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> bool { assert(!"operator <: UnionTypeUnresolved"); },
		[&](Path const&) -> bool { assert(!"operator <: Path"); },
		[&](InlineStructType const&) -> bool { assert(!"operator <: InlineStructType"); },
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


static Type* mk_builtin_type(BuiltinTypeDef builtin, Arena &arena)
{
	return arena.alloc<Type>(BuiltinType(UNKNOWN_TOKEN_RANGE, builtin));
}

static Type* mk_known_int_type(Int128 low, Int128 high, Arena &arena)
{
	return arena.alloc<Type>(KnownIntType(low, high));
}

Type materialize_known_int(KnownIntType known_int)
{
	if(integer_assignable_to(BuiltinTypeDef::I32, known_int.low) and integer_assignable_to(BuiltinTypeDef::I32, known_int.high))
		return BuiltinType(UNKNOWN_TOKEN_RANGE, BuiltinTypeDef::I32);
	else
		return BuiltinType(UNKNOWN_TOKEN_RANGE, smallest_int_type_for(known_int.low, known_int.high));
}

static Type* mk_pointer_type(Type *pointee, IsMutable mutability, Arena &arena)
{
	return arena.alloc<Type>(PointerType{
		.range = UNKNOWN_TOKEN_RANGE,
		.kind = PointerType::SINGLE,
		.pointee = pointee,
		.mutability = mutability,
	});
}


//--------------------------------------------------------------------
// Unification
//--------------------------------------------------------------------
struct ConstraintSystem;

using ThrowExprErrorFn = void (*)(Expr const &expr, string const &reason, Module const &mod);
using ThrowPatternErrorFn = void (*)(Pattern const &pattern, string const &reason, Module const &mod);

class LazyErrorMsg
{
public:
	LazyErrorMsg(Expr const *expr, ThrowExprErrorFn fn) :
		m_error_fns(std::pair(expr, fn)) {}

	LazyErrorMsg(Pattern const *pattern, ThrowPatternErrorFn fn) :
		m_error_fns(std::pair(pattern, fn)) {}

	[[noreturn]] void throw_error(string const &reason, Module const &mod) const
	{
		m_error_fns | match
		{
			[&](std::pair<Expr const*, ThrowExprErrorFn> expr_fn)
			{
				expr_fn.second(*expr_fn.first, reason, mod);
			},
			[&](std::pair<Pattern const*, ThrowPatternErrorFn> pattern_fn)
			{
				pattern_fn.second(*pattern_fn.first, reason, mod);
			},
		};

		assert(!"ThrowErrorFn returned");
	}

private:
	variant <
		std::pair<Expr const*, ThrowExprErrorFn>,
		std::pair<Pattern const*, ThrowPatternErrorFn>
	> m_error_fns;

};

enum class TypeConversion
{
	// Allows:
	// - Merging of KnownIntTypes
	// - Conversion of KnownIntTypes to those concrete integer types that can hold it
	NONE,

	// Allows:
	// - Everything allowed by NONE
	// - Any type conversions where the memory layout remains the same and the subtyping relation is
	//   respected. For example, the conversion Option.Some ==> Option is allowed, but i32 ==> u32 is
	//   not.
	TRIVIAL,

	// Allows:
	// - Everything allowed by TRIVIAL
	// - Implicit struct constructors
	// - Union constructors
	// - Safe integer promotions
	IMPLICIT_CTOR,
};

// The final goal of unification is to map each TypeDeductionVar to a concrete type (or report an
// error if that's impossible). This process consists of two phases:
//
// 1. Constraint generation phase: If a TypeDeductionVar is encountered during this phase of
//    unification, a constraint is generated that the TypeDeductionVar must satisfy in order for
//    unification to succeed
//
// 2. Checking phase: If a TypeDeductionVar is encountered it is looked up in the TypeEnv and
//    unification proceeds
using UnificationPhase = variant<
	// The constraint gathering phase
	ConstraintSystem*,

	// The checking phase
	TypeEnv const*
>;


struct ConstructorConversion
{
	StructInstance *ctor;
};

struct UnionConversion
{
	Type const *union_;
	Type const *alt;
};

using TypeConversionEvent = variant<ConstructorConversion, UnionConversion>;

static void apply_conversion(TypeConversionEvent const &conv, Expr *expr, Arena &arena);


struct ConversionCallback
{
	using Fn = void (*) (TypeConversionEvent const &conv, void *data, Arena &arena);

	void operator () (TypeConversionEvent const &conv, Arena &arena)
	{
		fn(conv, data, arena);
	}

	explicit operator bool () const { return fn != nullptr; }

	Fn fn = nullptr;
	void *data = nullptr;
};

static ConversionCallback mk_expr_converter(Expr *expr)
{
	return ConversionCallback
	{
		.fn = [](TypeConversionEvent const &conv, void *expr, Arena &arena)
		{
			apply_conversion(conv, (Expr*)expr, arena);
		},
		.data = (void*)expr,
	};
}


struct UnifyOutput
{
	ConversionCallback on_type_conversion_left{};
	ConversionCallback on_type_conversion_right{};
	Type *result = nullptr;

	UnifyOutput sides_swapped()
	{
		return {
			.on_type_conversion_left = on_type_conversion_right,
			.on_type_conversion_right = on_type_conversion_left,
			.result = result,
		};
	}

	UnifyOutput with_result(Type *new_result)
	{
		return {
			.on_type_conversion_left = on_type_conversion_left,
			.on_type_conversion_right = on_type_conversion_right,
			.result = new_result,
		};
	}
};

static void unify(
	Type const &left,
	Type const &right,
	TypeConversion conv_left,
	TypeConversion conv_right,
	UnificationPhase phase,
	UnifyOutput out,
	SemaContext const &ctx,
	optional<LazyErrorMsg> err
);


[[noreturn]] static void throw_unification_error(
	Type const &left,
	Type const &right,
	optional<LazyErrorMsg> error_msg,
	Module const *mod
)
{
	string reason = "Incompatible types " + str(left, *mod) + " and " + str(right, *mod);
	if(error_msg)
		error_msg->throw_error(reason, *mod);
	else
		throw ParseError(reason);

	UNREACHABLE;
}


//--------------------------------------------------------------------
// Constraints
//--------------------------------------------------------------------
struct IntegerCheck
{
	LazyErrorMsg error_msg;
	Type const *type;
};

struct CastCheck
{
	Expr *expr;
	Type const *target_type;
};

struct LValueCheck
{
	Expr *expr;
	IsMutable mutability;
};

// A check that is performed on a type after all TypeDeductionVars have been deduced
using TypeCheck = variant<
	IntegerCheck,
	CastCheck,
	LValueCheck
>;

struct GeneralConstraint { Type const *type; };
struct ManyPointeeConstraint { TypeDeductionVar array_var; };
struct SinglePointeeConstraint { TypeDeductionVar pointer_var; };
struct MemberConstraint { TypeDeductionVar struct_var; string_view member; };

using ConstraintKind = variant<
	GeneralConstraint,
	ManyPointeeConstraint,
	SinglePointeeConstraint,
	MemberConstraint
>;

struct UnificationConstraint
{
	ConstraintKind kind;
	TypeConversion own_conv = TypeConversion::NONE;
	TypeConversion other_conv = TypeConversion::NONE;
	ConversionCallback own_conv_cb{};
	ConversionCallback other_conv_cb{};
	optional<LazyErrorMsg> error_msg = nullopt;

	Type const& get_type(TypeEnv const &env, SemaContext const &ctx) const;

	void compute_dependencies(unordered_set<VarType> &deps) const
	{
		kind | match
		{
			[&](GeneralConstraint c) { gather_type_deduction_vars(*c.type, deps); },
			[&](ManyPointeeConstraint c) { deps.insert(c.array_var); },
			[&](SinglePointeeConstraint c) { deps.insert(c.pointer_var); },
			[&](MemberConstraint c) { deps.insert(c.struct_var); },
		};
	}
};

struct VarConstraintSet
{
	vector<UnificationConstraint> constraints;

	// Only used during constraint checking
	enum State
	{
		PENDING,
		IN_PROGRESS,
		DONE,
	};
	State state = PENDING;
	unordered_set<VarType> deps{};
	unordered_set<VarType> reverse_deps{};
};

static optional<TypeDeductionVar> get_if_type_deduction_var(Type const &type);

Type const* follow_type_var(Type const *type, TypeEnv const &env)
{
	if(optional<TypeDeductionVar> var = get_if_type_deduction_var(*type))
		return &env.lookup(*var);

	return type;
}

optional<IsMutable> is_lvalue_expr(Expr const &expr, TypeEnv const &env)
{
	return expr | match
	{
		[&](VarExpr const &e) -> optional<IsMutable>
		{
			return e.var->mutability;
		},
		[&](DerefExpr const &e) -> optional<IsMutable>
		{
			return std::get<PointerType>(*follow_type_var(type_of(*e.addr), env)).mutability;
		},
		[&](IndexExpr const &e) -> optional<IsMutable>
		{
			PointerType const &p = std::get<PointerType>(*follow_type_var(type_of(*e.addr), env));
			assert(p.kind == PointerType::MANY);

			return p.mutability;
		},
		[&](MemberAccessExpr const &e) -> optional<IsMutable>
		{
			return is_lvalue_expr(*e.object, env);
		},

		[](auto const&) -> optional<IsMutable> { return nullopt; },
	};
}

static bool is_cast_ok(Type const &target_type, Expr &src_expr, TypeEnv const &env, SemaContext const &ctx)
{
	Type *src_type = type_of(src_expr);
	try {
		unify(
			target_type, *src_type,
			TypeConversion::NONE, TypeConversion::IMPLICIT_CTOR,
			&env,
			{.on_type_conversion_right = mk_expr_converter(&src_expr)},
			ctx, nullopt
		);
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
		[&](VarType const &src_t) -> bool
		{
			return is_cast_ok(env.lookup(src_t), src_expr, env, ctx);
		},
		[&](PointerType const &src_t) -> bool
		{
			if(PointerType const *target_t = std::get_if<PointerType>(&target_type))
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
		[&](InlineStructType const&) -> bool { assert(!"is_cast_ok: InlineStructType"); },
	};
}

void do_check(TypeCheck const &check, TypeEnv const &env, SemaContext const &ctx)
{
	check | match
	{
		[&](IntegerCheck c)
		{
			Type const *type = follow_type_var(c.type, env);
			if(not is_integer_type(*type))
				c.error_msg.throw_error("Expected integer, got " + str(*type, *ctx.mod), *ctx.mod);
		},
		[&](CastCheck const &c)
		{
			if(not is_cast_ok(*c.target_type, *c.expr, env, ctx))
				throw_sem_error("Invalid cast", token_range_of(*c.expr).first, ctx.mod);
		},
		[&](LValueCheck const &c)
		{
			optional<IsMutable> object_mutability = is_lvalue_expr(*c.expr, env);
			if(not object_mutability)
				throw_sem_error("Address-of expression requires lvalue", token_range_of(*c.expr).first, ctx.mod);

			if(c.mutability == IsMutable::YES and object_mutability == IsMutable::NO)
				throw_sem_error("Cannot make mutable reference to const object", token_range_of(*c.expr).first, ctx.mod);
		},
	};
}

static optional<TypeDeductionVar> get_if_type_deduction_var(Type const &type);

template<>
struct std::hash<TypeDeductionVar>
{
	size_t operator () (TypeDeductionVar var) const
	{
		return compute_hash(var.id);
	}
};

string_view type_conv_symbol(TypeConversion conv)
{
	switch(conv)
	{
		case TypeConversion::NONE: return "=";
		case TypeConversion::TRIVIAL: return "^";
		case TypeConversion::IMPLICIT_CTOR: return "^^";
	}

	UNREACHABLE;
}

struct ConstraintSystem
{
	explicit ConstraintSystem(Module const &mod) :
		mod(mod) {}

	void add_check(TypeCheck const &check)
	{
		checks.push_back(check);
	}

	void add_relational_constraint(TypeDeductionVar var, UnificationConstraint const &c)
	{
		constraints[var].constraints.push_back(c);
	}

	// Lookup may fail for unconstrained TypeDeductionVars. For example, consider the following
	// definition:
	//
	//     struct Foo'T {}
	//
	// If one tries to unify Foo'$_0 with Foo'$_1 with no further information available, then only
	// one of $_0 and $_1 will have an associated VarConstraintSet.
	VarConstraintSet* try_lookup(TypeDeductionVar var)
	{
		auto it = constraints.find(var);
		if(it == constraints.end())
			return nullptr;

		return &it->second;
	}

	void print(std::ostream &os) const
	{
		vector<std::pair<TypeDeductionVar, VarConstraintSet const*>> list;
		for(auto const &[var, constr]: constraints)
			list.push_back({var, &constr});

		std::ranges::sort(list, [](auto const &a, auto const &b)
		{
			return a.first.id < b.first.id;
		});

		for(auto const &[var, constr]: list)
		{
			Type var_type{VarType(var)};

			os << str(var_type, mod);

			for(UnificationConstraint const &c: constr->constraints)
			{
				os << "\n  " << type_conv_symbol(c.other_conv) << ":" << type_conv_symbol(c.own_conv) << " ";

				c.kind | match
				{
					[&](GeneralConstraint const &k)
					{
						os << str(*k.type, mod);
					},
					[&](ManyPointeeConstraint const &k)
					{
						os << "element_type_of(";
						::print(VarType(k.array_var), os);
						os << ")";
					},
					[&](SinglePointeeConstraint const &k)
					{
						os << "pointee_of(";
						::print(VarType(k.pointer_var), os);
						os << ")";
					},
					[&](MemberConstraint const &k)
					{
						os << "member_of(";
						::print(VarType(k.struct_var), os);
						os << ", " << k.member << ")";
					},
				};
			}

			os << std::endl;
		}

		if(checks.size())
		{
			if(list.size()) os << std::endl;

			for(TypeCheck const &check: checks)
			{
				check | match
				{
					[&](IntegerCheck c)
					{
						os << "is_integer(" << str(*c.type, mod) << ")";
					},
					[&](CastCheck c)
					{
						::print(*c.expr, mod, os);
						os << " as " << str(*c.target_type, mod);
					},
					[&](LValueCheck const &c)
					{
						if(c.mutability == IsMutable::YES)
							os << "is_mut_lvalue(";
						else
							os << "is_lvalue(";

						::print(*c.expr, mod, os);
						os << ")";
					},
				};

				os << std::endl;
			}
		}
	}

	Module const &mod;
	unordered_map<TypeDeductionVar, VarConstraintSet> constraints;
	vector<TypeCheck> checks;
};


static TypeConversion combine_conv(TypeConversion a, TypeConversion b)
{
	return (TypeConversion)std::min((int)a, (int)b);
}

static ConversionCallback mk_meta_converter(vector<ConversionCallback> *callbacks)
{
	return ConversionCallback
	{
		.fn = [](TypeConversionEvent const &conv, void *data, Arena &arena)
		{
			vector<ConversionCallback> *conv_cbs = (vector<ConversionCallback>*)data;
			for(ConversionCallback cb: *conv_cbs)
				cb(conv, arena);
		},
		.data = (void*)callbacks,
	};
}

// Information flows bottom-up
static void reduce(TypeDeductionVar var, ConstraintSystem &sys, TypeEnv &env, SemaContext const &ctx)
{
	VarConstraintSet *c = sys.try_lookup(var);
	if(not c or c->state == VarConstraintSet::DONE)
		return;

	// Check for cycles
	assert(c->state == VarConstraintSet::PENDING);
	c->state = VarConstraintSet::IN_PROGRESS;

	// Compute and visit dependencies
	for(UnificationConstraint const &bc: c->constraints)
		bc.compute_dependencies(c->deps);

	for(VarType v: c->deps)
	{
		reduce(std::get<TypeDeductionVar>(v), sys, env, ctx);
		if(VarConstraintSet *dep = sys.try_lookup(std::get<TypeDeductionVar>(v)))
			dep->reverse_deps.insert(var);
	}

	// Reduce `c->constraints` to a single Type `result` by unify()ing them all together.
	//
	// As an example, assume we have constraints C1, C2 and C3 that constrain `var` to types the A,
	// B and C, respectively. In order to compute `result` we essentially do the following:
	//
	//     (1) result = unify(C1, C2)      // result contains the common type of A and B
	//     (2) result = unify(result, C3)  // result contains the common type of A, B and C
	//
	// Care must be taken that potential type conversions are always correctly inserted. To
	// demonstrate this, assume the following relationships: A <c B <c C, where A <c B means that A
	// can be converted to B, but we do need to insert the corresponding conversion function.
	//
	// Now, when we do step (1) above, the unification needs to insert a conversion for the
	// expression associated with C1 from A to B. In step (2), we need to insert conversions for
	// both C1 and C2 from B to A. In other words, we need to apply conversions to all previously
	// visited constraints.
	//
	// See for example tests/common_type_struct_2
	
	assert(c->constraints.size());
	UnificationConstraint &first = c->constraints.front();

	// `conv_cbs` contains the ConversionCallbacks of all the constraints we have visited so far:
	// Thus, if a constraint requires a conversion, we can apply that conversion to the expressions
	// of all previous constraints.
	vector<ConversionCallback> conv_cbs;
	if(first.own_conv_cb) conv_cbs.push_back(first.own_conv_cb);
	if(first.other_conv_cb) conv_cbs.push_back(first.other_conv_cb);

	Type result = first.get_type(env, ctx);
	TypeConversion result_conv = first.own_conv;
	for(UnificationConstraint const &uc: c->constraints | std::views::drop(1))
	{
		if(uc.other_conv_cb) conv_cbs.push_back(uc.other_conv_cb);

		Type new_result;
		unify(
			result, uc.get_type(env, ctx),
			result_conv, uc.own_conv,
			&env,
			{
				.on_type_conversion_left = mk_meta_converter(&conv_cbs),
				.on_type_conversion_right = uc.own_conv_cb,
				.result = &new_result
			},
			ctx,
			uc.error_msg
		);

		if(uc.own_conv_cb) conv_cbs.push_back(uc.own_conv_cb);

		result = new_result;
		result_conv = combine_conv(result_conv, uc.own_conv);
	}

	c->state = VarConstraintSet::DONE;
	env.add(var, result);
}

// Information flows top-down
static optional<Type> materialize(TypeDeductionVar var, ConstraintSystem &sys, TypeEnv &env, SemaContext const &ctx)
{
	Type &type = env.lookup(var);
	KnownIntType const *known_int = std::get_if<KnownIntType>(&type);
	if(not known_int)
	{
		if(is_integer_type(type))
			return type;

		return nullopt;
	}

	VarConstraintSet &c = sys.constraints.at(var);
	bool constrained = false;
	for(VarType rdep: c.reverse_deps)
	{
		TypeDeductionVar rdep_var = std::get<TypeDeductionVar>(rdep);
		if(optional<Type> const &used_in_type = materialize(rdep_var, sys, env, ctx))
		{
			if(constrained)
			{
				if(not equiv(type, *used_in_type))
					throw_sem_error("Type deduction ambiguous", INVALID_TOKEN_IDX, ctx.mod);
			}
			else
			{
				BuiltinType b = std::get<BuiltinType>(*used_in_type);
				assert(integer_assignable_to(b.builtin, known_int->low));
				assert(integer_assignable_to(b.builtin, known_int->high));
				type = *used_in_type;
			}
		}
	}

	if(constrained)
		return type;

	return nullopt;
}

// Reduces each VarConstraintSet into a single Type that can then be inserted into the TypeEnv.
//
// To this end, it is ensured that the reduction does not violate any of the stated constraints. For
// example, if we have a VarConstraintSet called C and a reduced type T, then
// - all the types in C.value_assignments need to be VALUE_ASSIGNABLE to T,
// - all the types in C.equalities need to be EQUAL to T,
// - etc.
static TypeEnv create_subst_from_constraints(ConstraintSystem &constraints, SemaContext const &ctx)
{
	TypeEnv result;
	for(auto const &[var, _]: constraints.constraints)
		reduce(var, constraints, result, ctx);

	for(auto const &[var, _]: constraints.constraints)
		materialize(var, constraints, result, ctx);

	for(TypeCheck const &c: constraints.checks)
		do_check(c, result, ctx);

	return result;
}


static Type const* get_multi_element_pointee(
	Type const *type,
	TokenRange range,
	SemaContext const &ctx
)
{
	PointerType const *pointer_type = std::get_if<PointerType>(type);
	if(not pointer_type or pointer_type->kind != PointerType::MANY)
		throw_sem_error("Expected multi-element pointer type, got "s + str(*type, *ctx.mod), range.first, ctx.mod);

	return pointer_type->pointee;
}


static Type const* get_single_element_pointee(
	Type const *type,
	TokenRange range,
	SemaContext const &ctx
)
{
	PointerType const *pointer_type = std::get_if<PointerType>(type);
	if(not pointer_type or pointer_type->kind != PointerType::SINGLE)
		throw_sem_error("Expected pointer type, got "s + str(*type, *ctx.mod), range.first, ctx.mod);

	return pointer_type->pointee;
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

static Type const* get_member_type(
	Type const *type,
	string_view member,
	TokenRange range,
	SemaContext const &ctx
)
{
	StructType const *struct_type = std::get_if<StructType>(type);
	if(not struct_type)
		throw_sem_error("Expected object of struct type for member access, got " + str(*type, *ctx.mod), range.first, ctx.mod);

	Parameter const *var_member = find_var_member(struct_type->inst, member);
	if(not var_member)
		throw_sem_error("`"s + struct_type->inst->struct_()->name + "` has no field named `"s + member + "`", range.first, ctx.mod);

	return var_member->type;
}


Type const& UnificationConstraint::get_type(TypeEnv const &env, SemaContext const &ctx) const
{
	return kind | match
	{
		[&](GeneralConstraint c) -> Type const&
		{
			return *c.type;
		},
		[&](ManyPointeeConstraint c) -> Type const&
		{
			Type const &pointer_type = env.lookup(c.array_var);
			Type const *pointee = get_multi_element_pointee(
				&pointer_type,
				token_range_of(pointer_type),
				ctx
			);

			return *pointee;
		},
		[&](SinglePointeeConstraint c) -> Type const&
		{
			Type const &pointer_type = env.lookup(c.pointer_var);
			Type const *pointee = get_single_element_pointee(
				&pointer_type,
				token_range_of(pointer_type),
				ctx
			);

			return *pointee;
		},
		[&](MemberConstraint c) -> Type const&
		{
			Type const &struct_type = env.lookup(c.struct_var);
			Type const *member_type = get_member_type(&struct_type, c.member, token_range_of(struct_type), ctx);

			return *member_type;
		},
	};
}


// Unification of integer types
//--------------------------------------------------------------------
static bool try_convert_integer_type(
	Type const &src_type,
	Type const &target_type,
	TypeConversion conv
)
{
	assert(is_integer_type(src_type));
	assert(is_integer_type(target_type));

	if(is<KnownIntType>(target_type))
		return true;

	if(KnownIntType const *src = std::get_if<KnownIntType>(&src_type))
	{
		BuiltinTypeDef target = std::get<BuiltinType>(target_type).builtin;
		return integer_assignable_to(target, src->low) and integer_assignable_to(target, src->high);
	}

	switch(conv)
	{
		case TypeConversion::NONE:
		case TypeConversion::TRIVIAL:
			return equiv(src_type, target_type);

		case TypeConversion::IMPLICIT_CTOR:
		{
			BuiltinTypeDef src = std::get<BuiltinType>(src_type).builtin;
			BuiltinTypeDef target = std::get<BuiltinType>(target_type).builtin;
			return builtin_losslessly_convertible(target, src);
		}
	}

	UNREACHABLE;
}

static bool try_unify_integer_types(
	Type const &left,
	Type const &right,
	TypeConversion conv_left,
	TypeConversion conv_right,
	UnifyOutput out,
	SemaContext const &ctx,
	optional<LazyErrorMsg> err
)
{
	if(not is_integer_type(left) or not is_integer_type(right))
		return false;

	optional<Type> common_type = common_int_type(left, right);
	if(not common_type)
		throw_unification_error(left, right, err, ctx.mod);

	if(not try_convert_integer_type(left, *common_type, conv_left))
		throw_unification_error(left, right, err, ctx.mod);

	if(not try_convert_integer_type(right, *common_type, conv_right))
		throw_unification_error(left, right, err, ctx.mod);

	if(out.result) *out.result = *common_type;

	return true;
}


// Unification of type deduction variables
//--------------------------------------------------------------------
bool type_var_occurs_in(VarType var, Type const &type)
{
	return type | match
	{
		[&](BuiltinType const&) { return false; },
		[&](KnownIntType const&) { return false; },
		[&](VarType const &t)
		{
			return t == var;
		},
		[&](PointerType const &t)
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
		[&](UnionType const &t)
		{
			return t.inst->occurring_vars().contains(var);
		},

		[&](ProcTypeUnresolved const&) -> bool { assert(!"type_var_occurs_in: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> bool { assert(!"type_var_occurs_in: UnionTypeUnresolved"); },
		[&](Path const&) -> bool { assert(!"type_var_occurs_in: Path"); },
		[&](InlineStructType const&) -> bool { assert(!"type_var_occurs_in: InlineStructType"); },
	};
}

static optional<TypeDeductionVar> get_if_type_deduction_var(Type const &type)
{
	VarType const *var_type = std::get_if<VarType>(&type);
	if(not var_type)
		return nullopt;

	TypeDeductionVar const* type_deduction_var = std::get_if<TypeDeductionVar>(var_type);
	if(not type_deduction_var)
		return nullopt;

	return *type_deduction_var;
}

static bool try_unify_type_deduction_vars(
	Type const &left,
	Type const &right,
	TypeConversion conv_left,
	TypeConversion conv_right,
	UnificationPhase phase,
	UnifyOutput out,
	SemaContext const &ctx,
	optional<LazyErrorMsg> err
)
{
	optional<TypeDeductionVar> left_var = get_if_type_deduction_var(left);
	optional<TypeDeductionVar> right_var = get_if_type_deduction_var(right);

	if(left_var and right_var)
	{
		if(left_var == right_var)
		{
			if(out.result)
				*out.result = std::get<TypeEnv const*>(phase)->lookup(*left_var);
		}
		else
		{
			phase | match
			{
				[&](ConstraintSystem *constraints)
				{
					UnificationConstraint uni_constr{
						.kind = GeneralConstraint(&right),
						.own_conv = conv_right,
						.other_conv = conv_left,
						.own_conv_cb = out.on_type_conversion_right,
						.other_conv_cb = out.on_type_conversion_right,
						.error_msg = err,
					};
					constraints->add_relational_constraint(*left_var, uni_constr);
					if(out.result) *out.result = left;
				},
				[&](TypeEnv const *env)
				{
					unify(env->lookup(*left_var), right, conv_left, conv_right, phase, out, ctx, err);
				},
			};
		}

		return true;
	}

	if(left_var or right_var)
	{
		TypeDeductionVar var = left_var ? *left_var : *right_var;
		Type const *type = left_var ? &right : &left;
		bool args_swapped = not left_var;

		if(args_swapped)
		{
			std::swap(conv_left, conv_right);
			std::swap(out.on_type_conversion_left, out.on_type_conversion_right);
		}

		assert(not type_var_occurs_in(var, *type));

		phase | match
		{
			[&](ConstraintSystem *constraints)
			{
				UnificationConstraint uni_constr{
					.kind = GeneralConstraint(type),
					.own_conv = conv_right,
					.other_conv = conv_left,
					.own_conv_cb = out.on_type_conversion_right,
					.other_conv_cb = out.on_type_conversion_left,
					.error_msg = err,
				};
				constraints->add_relational_constraint(var, uni_constr);
				if(out.result) *out.result = Type(VarType(var));
			},
			[&](TypeEnv const *env)
			{
				unify(env->lookup(var), *type, conv_left, conv_right, phase, out, ctx, err);
			},
		};

		return true;
	}

	return false;
}


// Unification of struct types
//--------------------------------------------------------------------
static Type const* try_find_matching_alt_for(
	UnionInstance *union_,
	Type const &type,
	UnificationPhase phase,
	UnifyOutput out,
	SemaContext const &ctx,
	optional<LazyErrorMsg> err
);

static StructInstance* get_if_struct(Type const *type)
{
	StructType const *struct_type = std::get_if<StructType>(type);
	if(not struct_type)
		return nullptr;

	return struct_type->inst;
}

static UnionInstance* get_if_union(Type const *type)
{
	UnionType const *union_type = std::get_if<UnionType>(type);
	if(not union_type)
		return nullptr;

	return union_type->inst;
}

static void unify_type_args(
	TypeArgList const &left,
	TypeArgList const &right,
	UnificationPhase phase,
	SemaContext const &ctx,
	optional<LazyErrorMsg> err
)
{
	assert(left.args->count == right.args->count);
	for(size_t i = 0; i < left.args->count; ++i)
		unify(left.args->items[i], right.args->items[i], TypeConversion::NONE, TypeConversion::NONE, phase, {}, ctx, err);
}

static void unify_structs_eq(
	StructInstance *left,
	StructInstance *right,
	UnificationPhase phase,
	SemaContext const &ctx,
	optional<LazyErrorMsg> err
)
{
	if(left != right)
	{
		if(left->is_deduction_complete() && right->is_deduction_complete())
		{
			throw_unification_error(
				StructType(UNKNOWN_TOKEN_RANGE, left),
				StructType(UNKNOWN_TOKEN_RANGE, right),
				err,
				ctx.mod
			);
		}
		else
		{
			if(left->struct_() != right->struct_())
			{
				throw_unification_error(
					StructType(UNKNOWN_TOKEN_RANGE, left),
					StructType(UNKNOWN_TOKEN_RANGE, right),
					err,
					ctx.mod
				);
			}

			do
			{
				unify_type_args(left->type_args(), right->type_args(), phase, ctx, err);
				left = left->parent();
				right = right->parent();
			}
			while(left);
		}
	}
}

static bool try_convert_struct_to_parent(
	StructInstance *struct_,
	StructInstance *parent,
	TypeConversion conv
)
{
	switch(conv)
	{
		case TypeConversion::NONE:
			return struct_ == parent;

		case TypeConversion::TRIVIAL:
		case TypeConversion::IMPLICIT_CTOR:
			return true;
	}

	UNREACHABLE;
}

static std::pair<StructInstance*, StructInstance*> common_parent(StructInstance *left, StructInstance *right)
{
	int min_depth = std::min(left->depth(), right->depth());
	while(left->depth() > min_depth)
		left = left->parent();

	while(right->depth() > min_depth)
		right = right->parent();

	do
	{
		if(left->struct_() == right->struct_())
			return {left, right};

		left = left->parent();
		right = right->parent();
	} while(left);

	return {nullptr, nullptr};
}

StructInstance* try_get_implicit_ctor_for(
	StructInstance *struct_,
	Type const &arg_type,
	ConversionCallback arg_conv_cb,
	UnificationPhase phase,
	SemaContext const &ctx
)
{
	StructInstance *implicit_case = struct_->implicit_case();
	if(not implicit_case)
		return nullptr;

	ProcType const &implicit_ctor = std::get<ProcType>(*implicit_case->try_get_ctor_type());
	Type const &implicit_param_type = implicit_ctor.inst->params->items[0];

	try {
		unify(implicit_param_type, arg_type, TypeConversion::NONE, TypeConversion::IMPLICIT_CTOR, phase, {.on_type_conversion_right = arg_conv_cb}, ctx, nullopt);
		return implicit_case;
	}
	catch(ParseError const&) {}

	return nullptr;
}

Expr call_implicit_ctor(StructInstance *implicit_case, Expr const &arg, Arena &arena)
{
	assert(implicit_case->try_get_ctor_type());

	Expr *ctor_expr = arena.alloc<Expr>(ConstructorExpr{
		.range = UNKNOWN_TOKEN_RANGE,
		.ctor = arena.alloc<Type>(StructType(UNKNOWN_TOKEN_RANGE, implicit_case)),
		.type = implicit_case->try_get_ctor_type(),
	});

	FixedArray<Argument> *ctor_args = alloc_fixed_array<Argument>(1, arena);
	ctor_args->items[0] = Argument{
		.range = UNKNOWN_TOKEN_RANGE,
		.expr = arg,
	};

	return CallExpr{
		.range = UNKNOWN_TOKEN_RANGE,
		.callable = ctor_expr,
		.args = ctor_args,
		.type = arena.alloc<Type>(StructType(UNKNOWN_TOKEN_RANGE, implicit_case)),
	};
}

static void apply_conversion(TypeConversionEvent const &conv, Expr *expr, Arena &arena)
{
	conv | match
	{
		[&](ConstructorConversion c)
		{
			*expr = call_implicit_ctor(c.ctor, *expr, arena);
		},
		[&](UnionConversion c)
		{
			*expr = Expr(UnionInitExpr{
				.range = token_range_of(*expr),
				.alt_expr = clone_ptr(expr, arena),
				.alt_type = clone_ptr(c.alt, arena),
				.type = clone_ptr(c.union_, arena),
			});
		},
	};
}

static bool try_unify_structs(
	Type const &left,
	Type const &right,
	TypeConversion conv_left,
	TypeConversion conv_right,
	UnificationPhase phase,
	UnifyOutput out,
	SemaContext const &ctx,
	optional<LazyErrorMsg> err
)
{
	StructInstance *left_struct = get_if_struct(&left);
	StructInstance *right_struct = get_if_struct(&right);
	if(not left_struct)
	{
		if(not right_struct)
			return false;

		// Swap left and right
		return try_unify_structs(right, left, conv_right, conv_left, phase, out.sides_swapped(), ctx, err);
	}

	// If either common_parent_left or common_parent_right is set, then the other one is also set,
	// and they both refer to the same StructItem
	StructInstance *common_parent_left = nullptr;
	StructInstance *common_parent_right = nullptr;
	if(right_struct)
		std::tie(common_parent_left, common_parent_right) = common_parent(left_struct, right_struct);

	if(common_parent_left)
	{
		// So `left` and `right` have a common parent. The only way unification can succeed is if
		// both sides can be converted to that same parent.
		if(
			not try_convert_struct_to_parent(left_struct, common_parent_left, conv_left)
			or not try_convert_struct_to_parent(right_struct, common_parent_right, conv_right)
		) {
			throw_unification_error(left, right, err, ctx.mod);
		}

		unify_structs_eq(common_parent_left, common_parent_right, phase, ctx, err);

		if(out.result)
			*out.result = StructType(UNKNOWN_TOKEN_RANGE, common_parent_left);
	}
	else if(UnionInstance *right_union = get_if_union(&right))
	{
		StructInstance *implicit_case_left = nullptr;
		Type const *matching_alt_right = nullptr;

		if(conv_right == TypeConversion::IMPLICIT_CTOR)
			implicit_case_left = try_get_implicit_ctor_for(left_struct, right, out.on_type_conversion_right, phase, ctx);

		if(conv_left == TypeConversion::IMPLICIT_CTOR)
			matching_alt_right = try_find_matching_alt_for(right_union, left, phase, out, ctx, err);

		if(not implicit_case_left and not matching_alt_right)
			throw_unification_error(left, right, err, ctx.mod);

		// This shouldn't be possible at the moment because it would imply left_struct and
		// right_union are defined recursively. However, this could change once user-defined
		// constructors are supported.
		if(implicit_case_left and matching_alt_right)
		{
			string reason = "Ambiguous common type for " + str(left, *ctx.mod) + " and " + str(right, *ctx.mod);
			if(err)
				err->throw_error(reason, *ctx.mod);
			else
				throw ParseError(reason);
		}

		if(implicit_case_left)
		{
			if(out.result)
				*out.result = left;

			if(out.on_type_conversion_right)
				out.on_type_conversion_right(ConstructorConversion(implicit_case_left), ctx.arena);
		}

		if(matching_alt_right)
		{
			if(out.result)
				*out.result = right;

			if(out.on_type_conversion_left)
				out.on_type_conversion_left(UnionConversion(&right, matching_alt_right), ctx.arena);
		}
	}
	else
	{
		// No common parent, so the only way forward is using an implicit ctor

		StructInstance *implicit_case_right = nullptr;
		StructInstance *implicit_case_left = nullptr;

		if(conv_right == TypeConversion::IMPLICIT_CTOR)
			implicit_case_left = try_get_implicit_ctor_for(left_struct, right, out.on_type_conversion_right, phase, ctx);

		if(conv_left == TypeConversion::IMPLICIT_CTOR and right_struct)
			implicit_case_right = try_get_implicit_ctor_for(right_struct, left, out.on_type_conversion_left, phase, ctx);

		if(implicit_case_left and implicit_case_right)
		{
			string reason = "Ambiguous common type for " + str(left, *ctx.mod) + " and " + str(right, *ctx.mod);
			if(err)
				err->throw_error(reason, *ctx.mod);
			else
				throw ParseError(reason);
		}

		if(not implicit_case_left and not implicit_case_right)
			throw_unification_error(left, right, err, ctx.mod);


		// Fill in `out`

		if(implicit_case_left)
		{
			if(out.result)
				*out.result = left;

			if(out.on_type_conversion_right)
				out.on_type_conversion_right(ConstructorConversion(implicit_case_left), ctx.arena);
		}

		if(implicit_case_right)
		{
			if(out.result)
				*out.result = right;

			if(out.on_type_conversion_left)
				out.on_type_conversion_left(ConstructorConversion(implicit_case_right), ctx.arena);
		}
	}

	return true;
}


// Unification of union types
//--------------------------------------------------------------------
static Type const* try_find_matching_alt_for(
	UnionInstance *union_,
	Type const &type,
	UnificationPhase phase,
	UnifyOutput out,
	SemaContext const &ctx,
	optional<LazyErrorMsg> err
)
{
	Type const *matched_alt = nullptr;
	for(Type const *alt: union_->alternatives())
	{
		bool success = false;
		try {
			unify(*alt, type, TypeConversion::NONE, TypeConversion::TRIVIAL, phase, out, ctx, err);
			success = true;
		}
		catch(ParseError const&) {}

		if(success)
		{
			if(matched_alt)
				throw_sem_error("Assignment to union is ambiguous", token_range_of(type).first, ctx.mod);

			matched_alt = alt;
		}
	}

	return matched_alt;
}

static bool try_unify_unions(
	Type const &left,
	Type const &right,
	TypeConversion conv_left,
	TypeConversion conv_right,
	UnificationPhase phase,
	UnifyOutput out,
	SemaContext const &ctx,
	optional<LazyErrorMsg> err
)
{
	UnionInstance *left_union = get_if_union(&left);
	UnionInstance *right_union = get_if_union(&right);
	if(not left_union)
	{
		if(not right_union)
			return false;

		return try_unify_unions(right, left, conv_right, conv_left, phase, out.sides_swapped(), ctx, err);
	}
	assert(left_union);

	if(left_union and right_union)
	{
		// Unions cannot be nested, so if we have two unions then they can only be unified if they
		// are equal

		if(not equiv(left, right))
			throw_unification_error(left, right, err, ctx.mod);

		if(out.result)
			*out.result = left;
	}
	else
	{
		// If we get here we know `left` is a union type and `right` could be anything except a
		// union. The only way unification can succeed here is if `right` can be unified with one of
		// the alternatives of `left`. (Note that the case where `right` is a struct with an
		// implicit constructor for `left` has already been handled in `try_unify_structs()`).

		if(conv_right != TypeConversion::IMPLICIT_CTOR)
			throw_unification_error(left, right, err, ctx.mod);

		Type const *matched_alt = try_find_matching_alt_for(left_union, right, phase, out, ctx, err);
		if(not matched_alt)
			throw_unification_error(left, right, err, ctx.mod);

		if(out.result)
			*out.result = left;

		if(out.on_type_conversion_right)
			out.on_type_conversion_right(UnionConversion(&left, matched_alt), ctx.arena);
	}

	return true;
}


// Unification of pointer types
//--------------------------------------------------------------------
IsMutable mutability_lub(IsMutable a, IsMutable b)
{
	if(a == IsMutable::YES and b == IsMutable::YES)
		return IsMutable::YES;

	return IsMutable::NO;
}

bool can_convert_mutability(IsMutable from, IsMutable to, TypeConversion conv)
{
	switch(conv)
	{
		case TypeConversion::NONE:
			return from == to;

		case TypeConversion::TRIVIAL:
		case TypeConversion::IMPLICIT_CTOR:
			return to == IsMutable::NO or from == IsMutable::YES;
	}

	UNREACHABLE;
}

static bool try_unify_pointers(
	Type const &left,
	Type const &right,
	TypeConversion conv_left,
	TypeConversion conv_right,
	UnificationPhase phase,
	UnifyOutput out,
	SemaContext const &ctx,
	optional<LazyErrorMsg> err
)
{
	PointerType const *left_pointer = std::get_if<PointerType>(&left);
	PointerType const *right_pointer = std::get_if<PointerType>(&right);
	if(not left_pointer or not right_pointer)
		return false;

	if(left_pointer->kind != right_pointer->kind)
		throw_unification_error(left, right, err, ctx.mod);

	IsMutable common_mutability = mutability_lub(left_pointer->mutability, right_pointer->mutability);
	if(
		not can_convert_mutability(left_pointer->mutability, common_mutability, conv_left)
		or not can_convert_mutability(right_pointer->mutability, common_mutability, conv_right)
	) {
		throw_unification_error(left, right, err, ctx.mod);
	}

	Type common_pointee;
	unify(
		*left_pointer->pointee,
		*right_pointer->pointee,
		conv_left == TypeConversion::IMPLICIT_CTOR ? TypeConversion::TRIVIAL : conv_left,
		conv_right == TypeConversion::IMPLICIT_CTOR ? TypeConversion::TRIVIAL : conv_right,
		phase,
		out.with_result(&common_pointee),
		ctx, err
	);

	if(out.result)
	{
		*out.result = PointerType{
			.range = UNKNOWN_TOKEN_RANGE,
			.kind = left_pointer->kind,
			.pointee = clone_ptr(&common_pointee, ctx.arena),
			.mutability = common_mutability,
		};
	}

	return true;
}


// Unification
//--------------------------------------------------------------------
// See
// - https://eli.thegreenplace.net/2018/unification/
//   - The presented algorithm seems to be incorrect, or at least does not match the usual
//     definition for first-order logic. For example, assume we want to unify the following
//     formulas:
//
//       f(x, y)
//       f(y, c)
//
//     where x and y are variables and c is a constant. Eli's algorithm would give the following
//     substitution:
//
//       subst_eli = [x ↦ y, y ↦ c]
//
//     However, the traditional algorithm for FOL would result in
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
// - https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm
//
static void unify(
	Type const &left,
	Type const &right,
	TypeConversion conv_left,
	TypeConversion conv_right,
	UnificationPhase phase,
	UnifyOutput out,
	SemaContext const &ctx,
	optional<LazyErrorMsg> err
)
{
	// The order in which (1), (3), and (4) are executed is important because their covered cases
	// overlap.

	// (1) Cases covered: is<TypeDeductionVar>(left) OR is<TypeDeductionVar>(right)
	if(try_unify_type_deduction_vars(left, right, conv_left, conv_right, phase, out, ctx, err))
		return;

	// (2) Cases covered: is_integer_type(left) AND is_integer_type(right)
	if(try_unify_integer_types(left, right, conv_left, conv_right, out, ctx, err))
		return;

	// (3) Cases covered: is<StructType>(left) OR is<StructType>(right)
	if(try_unify_structs(left, right, conv_left, conv_right, phase, out, ctx, err))
		return;

	// (4) Cases covered: is<UnionType>(left) OR is<UnionType>(right)
	if(try_unify_unions(left, right, conv_left, conv_right, phase, out, ctx, err))
		return;

	// (5) Cases covered: is<PointerType>(left) AND is<PointerType>(right)
	if(try_unify_pointers(left, right, conv_left, conv_right, phase, out, ctx, err))
		return;

	left | match
	{
		[&](BuiltinType const &left_t)
		{
			BuiltinType const *right_t = std::get_if<BuiltinType>(&right);
			if(not right_t)
				throw_unification_error(left, right, err, ctx.mod);

			// The case where both left and right are integer types has already been handled above.
			// Non-integer builtin types (Never, Unit, Bool) can only be unified if they are equal.
			if(left_t.builtin != right_t->builtin)
				throw_unification_error(left, right, err, ctx.mod);

			if(out.result) *out.result = left;
		},
		[&](VarType const&)
		{
			// If we get here, we know left and right are TypeParameterVars (because we already
			// handled TypeDeductionVars above).
			//
			// Unification of TypeParameterVars only succeeds if both sides refer to the same
			// TypeParameterVar as they may be isntantiated with any type.

			if(not equiv(left, right))
				throw_unification_error(left, right, err, ctx.mod);

			if(out.result) *out.result = left;
		},
		[&](ProcType const&)
		{
			assert(!"[TODO] unify: ProcType");
		},

		// Handled above
		[&](KnownIntType const&) { throw_unification_error(left, right, err, ctx.mod); },
		[&](PointerType const&) { throw_unification_error(left, right, err, ctx.mod); },
		[&](StructType const&) { throw_unification_error(left, right, err, ctx.mod); },
		[&](UnionType const&) { throw_unification_error(left, right, err, ctx.mod); },

		[&](ProcTypeUnresolved const&) { assert(!"unify: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) { assert(!"unify: UnionTypeUnresolved"); },
		[&](Path const&) { assert(!"unify: Path"); },
		[&](InlineStructType const&) { assert(!"unify: InlineStructType"); },
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
		[&](UnionInitExpr const &e)
		{
			expr_default_value_deps(*e.alt_expr, deps, mod);
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

static Type* typecheck_subexpr(Expr &expr, ConstraintSystem &constraints, SemaContext &ctx)
{
	auto res = expr | match
	{
		[&](IntLiteralExpr &e)
		{
			return e.type = ctx.arena.alloc<Type>(KnownIntType(e.value, e.value));
		},
		[&](BoolLiteralExpr &e)
		{
			return e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
		},
		[&](StringLiteralExpr &e)
		{
			ScopeItem &c_char_item = ctx.mod->sema->globals->lookup("c_char", INVALID_TOKEN_IDX);
			StructInstance *c_char = ctx.mod->sema->insts.get_struct_instance(std::get<StructItem*>(c_char_item), nullptr, nullptr);

			return e.type = ctx.arena.alloc<Type>(PointerType{
				.range = UNKNOWN_TOKEN_RANGE,
				.kind = PointerType::MANY,
				.pointee = ctx.arena.alloc<Type>(StructType(UNKNOWN_TOKEN_RANGE, c_char)),
				.mutability = IsMutable::NO,
			});
		},
		[&](UnaryExpr &e)
		{
			switch(e.op)
			{
				case UnaryOp::NOT:
				{
					e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
					Type const *sub_type = typecheck_subexpr(*e.sub, constraints, ctx);

					auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Invalid operand for not operator: " + reason, token_range_of(expr).first, &mod);
					};
					unify(*e.type, *sub_type, TypeConversion::NONE, TypeConversion::NONE, &constraints, {}, ctx, LazyErrorMsg(e.sub, error_msg));
				} break;

				case UnaryOp::NEG:
				{
					// Example where type_of(e.sub) is a TypeDeductionVar that is not mapped to any
					// concrete type:
					//
					//     proc foo'S() -> S {}
					//     proc bar'T(a: T, b: T) {}
					//
					//     bar(-foo(), 1)

					Type const *sub_type = typecheck_subexpr(*e.sub, constraints, ctx);
					if(KnownIntType const *known_int = std::get_if<KnownIntType>(sub_type))
					{
						// TODO Check for overflow
						e.type = mk_known_int_type(-known_int->high, -known_int->low, ctx.arena);
					}
					else
					{
						e.type = ctx.arena.alloc<Type>(clone(*sub_type, ctx.arena));

						auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
						{
							throw_sem_error("Invalid operand for negation operator: " + reason, token_range_of(expr).first, &mod);
						};
						constraints.add_check(IntegerCheck(LazyErrorMsg(e.sub, error_msg), e.type));
					}
				} break;
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
					Type const *left_type = typecheck_subexpr(*e.left, constraints, ctx);
					Type const *right_type = typecheck_subexpr(*e.right, constraints, ctx);

					UnifyOutput out{
						.on_type_conversion_left = mk_expr_converter(e.left),
						.on_type_conversion_right = mk_expr_converter(e.right),
						.result = ctx.arena.alloc<Type>(),
					};
					auto uni_error_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Invalid operands for binary operator: " + reason, token_range_of(expr).first, &mod);
					};
					unify(
						*left_type, *right_type,
						TypeConversion::IMPLICIT_CTOR, TypeConversion::IMPLICIT_CTOR,
						&constraints,
						out, ctx,
						LazyErrorMsg(&expr, uni_error_msg)
					);

					KnownIntType const *left_known_int = std::get_if<KnownIntType>(left_type);
					KnownIntType const *right_known_int = std::get_if<KnownIntType>(right_type);
					if(left_known_int and right_known_int)
					{
						// TODO Implement interval arithmetic
						assert(left_known_int->low == left_known_int->high);
						assert(right_known_int->low == right_known_int->high);

						Int128 result;
						switch(e.op)
						{
							// TODO Handle overflow / division by zero
							case BinaryOp::ADD: result = left_known_int->low + right_known_int->low; break;
							case BinaryOp::SUB: result = left_known_int->low - right_known_int->low; break;
							case BinaryOp::MUL: result = left_known_int->low * right_known_int->low; break;
							case BinaryOp::DIV: result = left_known_int->low / right_known_int->low; break;
							default: UNREACHABLE;
						}

						e.type = mk_known_int_type(result, result, ctx.arena);
					}
					else
					{
						// Example where type_of(e.left) and type_of(e.right) are a TypeDeductionVars
						// that are not mapped to any concrete type:
						//
						//     proc foo'S() -> S {}
						//     proc bar'T(a: T, b: T) {}
						//
						//     bar(foo() + foo(), 1)
						//
						//
						// Example where the type of type_of(e.left) and type_of(e.right) change:
						//
						//     proc foo'S() -> S {}
						//     proc bar'T(a: T, b: T, c: T) {}
						//
						//     bar(1, foo() + foo(), 4294967295)

						auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
						{
							throw_sem_error("Invalid type for binary operator: " + reason, token_range_of(expr).first, &mod);
						};
						constraints.add_check(IntegerCheck(LazyErrorMsg(e.left, error_msg), left_type));
						constraints.add_check(IntegerCheck(LazyErrorMsg(e.right, error_msg), right_type));

						e.type = out.result;
					}
				} break;

				case BinaryOp::EQ:
				{
					Type const *left_type = typecheck_subexpr(*e.left, constraints, ctx);
					Type const *right_type = typecheck_subexpr(*e.right, constraints, ctx);

					auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Equality operator requires equal types: " + reason, token_range_of(expr).first, &mod);
					};
					unify(*left_type, *right_type, TypeConversion::NONE, TypeConversion::NONE, &constraints, {}, ctx, LazyErrorMsg(&expr, error_msg));

					e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
				} break;

				case BinaryOp::LT:
				case BinaryOp::LE:
				case BinaryOp::GT:
				case BinaryOp::GE:
				{
					Type const *left_type = typecheck_subexpr(*e.left, constraints, ctx);
					Type const *right_type = typecheck_subexpr(*e.right, constraints, ctx);

					auto uni_error_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Invalid operands for comparison operator: " + reason, token_range_of(expr).first, &mod);
					};
					unify(*left_type, *right_type, TypeConversion::NONE, TypeConversion::NONE, &constraints, {}, ctx, LazyErrorMsg(&expr, uni_error_msg));

					auto integer_check_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Invalid operand for comparison operator: " + reason, token_range_of(expr).first, &mod);
					};

					constraints.add_check(IntegerCheck(LazyErrorMsg(e.left, integer_check_msg), left_type));
					constraints.add_check(IntegerCheck(LazyErrorMsg(e.right, integer_check_msg), right_type));

					e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
				} break;
			}

			return e.type;
		},
		[&](AddressOfExpr &e) -> Type*
		{
			Type const *object_type = typecheck_subexpr(*e.object, constraints, ctx);
			constraints.add_check(LValueCheck(e.object, e.mutability));

			return e.type = mk_pointer_type(clone_ptr(object_type, ctx.arena), e.mutability, ctx.arena);
		},
		[&](DerefExpr &e) -> Type*
		{
			Type const *addr_type = typecheck_subexpr(*e.addr, constraints, ctx);

			if(optional<TypeDeductionVar> pointer_var = get_if_type_deduction_var(*addr_type))
			{
				TypeDeductionVar var = ctx.new_type_deduction_var();
				constraints.add_relational_constraint(var, UnificationConstraint(SinglePointeeConstraint(*pointer_var)));
				e.type = ctx.arena.alloc<Type>(VarType(var));
			}
			else
			{
				Type const *pointee = get_single_element_pointee(addr_type, e.range, ctx);
				e.type = clone_ptr(pointee, ctx.arena);
			}

			return e.type;
		},
		[&](IndexExpr &e) -> Type*
		{
			Type const *addr_type = typecheck_subexpr(*e.addr, constraints, ctx);
			Type const *index_type = typecheck_subexpr(*e.index, constraints, ctx);

			if(optional<TypeDeductionVar> array_var = get_if_type_deduction_var(*addr_type))
			{
				TypeDeductionVar var = ctx.new_type_deduction_var();
				constraints.add_relational_constraint(var, UnificationConstraint(ManyPointeeConstraint(*array_var)));
				e.type = ctx.arena.alloc<Type>(VarType(var));
			}
			else
			{
				Type const *pointee = get_multi_element_pointee(addr_type, e.range, ctx);
				e.type = clone_ptr(pointee, ctx.arena);
			}

			auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
			{
				throw_sem_error("Invalid operand for index operator: " + reason, token_range_of(expr).first, &mod);
			};

			constraints.add_check(IntegerCheck(LazyErrorMsg(e.index, error_msg), index_type));
			return e.type;
		},
		[&](MemberAccessExpr &e) -> Type*
		{
			// Example where type_of(e.object) is a TypeDeductionVar:
			//
			//     struct Foo { i: i32 }
			//     proc id'T(v: T) { return v; }
			//
			//     id(Foo(3)).x

			Type const *object_type = typecheck_subexpr(*e.object, constraints, ctx);

			if(optional<TypeDeductionVar> object_var = get_if_type_deduction_var(*object_type))
			{
				TypeDeductionVar var = ctx.new_type_deduction_var();
				constraints.add_relational_constraint(var, UnificationConstraint(MemberConstraint(*object_var, e.member)));

				e.type = ctx.arena.alloc<Type>(VarType(var));
			}
			else
			{
				Type const *member_type = get_member_type(object_type, e.member, e.range, ctx);
				e.type = clone_ptr(member_type, ctx.arena);
			}

			return e.type;
		},
		[&](AssignmentExpr &e) -> Type*
		{
			Type const *lhs_type = typecheck_subexpr(*e.lhs, constraints, ctx);
			Type const *rhs_type = typecheck_subexpr(*e.rhs, constraints, ctx);

			auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
			{
				throw_sem_error("Invalid operands for assignment operator: " + reason, token_range_of(expr).first, &mod);
			};
			unify(
				*lhs_type, *rhs_type,
				TypeConversion::NONE, TypeConversion::IMPLICIT_CTOR,
				&constraints,
				{.on_type_conversion_right = mk_expr_converter(e.rhs)},
				ctx,
				LazyErrorMsg(&expr, error_msg)
			);

			constraints.add_check(LValueCheck(e.lhs, IsMutable::YES));

			return e.type = clone_ptr(lhs_type, ctx.arena);
		},
		[&](AsExpr &e) -> Type*
		{
			typecheck_subexpr(*e.src_expr, constraints, ctx);

			constraints.add_check(CastCheck{e.src_expr, e.target_type});
			return e.type = clone_ptr(e.target_type, ctx.arena);
		},
		[&](ConstructorExpr &e)
		{
			StructType const *struct_type = std::get_if<StructType>(e.ctor);
			if(not struct_type)
				throw_sem_error("Expected struct type", e.range.first, ctx.mod);

			Type const *ctor_type = struct_type->inst->try_get_ctor_type();
			if(not ctor_type)
				throw_sem_error("Struct does not provide a constructor", e.range.first, ctx.mod);

			return e.type = clone_ptr(ctor_type, ctx.arena);
		},
		[&](ProcExpr &e)
		{
			return e.type = clone_ptr(e.inst->get_type(), ctx.arena);
		},
		[&](CallExpr &e)
		{
			Type const *callable_type = typecheck_subexpr(*e.callable, constraints, ctx);
			ProcType const *callable_proc_type = std::get_if<ProcType>(callable_type);
			if(not callable_proc_type)
				throw_sem_error("Expected callable expression", e.range.first, ctx.mod);

			FixedArray<Type> const *param_types = callable_proc_type->inst->params;
			if(e.args->count > param_types->count)
				throw_sem_error("Too many arguments", e.range.first, ctx.mod);

			auto arg_error_msg = [](Expr const &arg, string const &reason, Module const &mod)
			{
				throw_sem_error("Invalid argument: " + reason, token_range_of(arg).first, &mod);
			};

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
				Type *arg_type = typecheck_subexpr(arg.expr, constraints, ctx);
				unify(
					param_type, *arg_type,
					TypeConversion::NONE, TypeConversion::IMPLICIT_CTOR,
					&constraints,
					{.on_type_conversion_right = mk_expr_converter(&arg.expr)},
					ctx,
					LazyErrorMsg(&arg.expr, arg_error_msg)
				);
			}

			for(size_t param_idx = 0; param_idx < param_types->count; ++param_idx)
			{
				if(!assigned_params.contains(param_idx))
				{
					if(not callable_proc_type->param_default_value_at(param_idx))
						throw_sem_error("Missing argument for parameter "s + callable_proc_type->param_name_at(param_idx), e.range.first, ctx.mod);
				}
			}

			return e.type = clone_ptr(callable_proc_type->inst->ret, ctx.arena);
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
		[&](UnionInitExpr &e)
		{
			return e.type;
		},
		[&](VarExpr &e)
		{
			assert(e.var->type && "typecheck_subexpr: VarExpr: Var::type is null");
			return e.type = clone_ptr(e.var->type, ctx.arena);
		},
		[&](Path&) -> Type* { assert(!"typecheck_subexpr: Path"); },
	};

	return res;
}

static Type const* typecheck_expr(Expr &expr, SemaContext &ctx)
{
	LOGGER(ctx.mod->logger, on_expr_start, expr);

	ConstraintSystem constraints(*ctx.mod);
	Type const *type = typecheck_subexpr(expr, constraints, ctx);
	LOGGER(ctx.mod->logger, on_data, constraints);

	TypeEnv subst = create_subst_from_constraints(constraints, ctx);
	substitute_types_in_expr(expr, subst, ctx.mod->sema->insts, FullDeductionSubsitution(token_range_of(expr)));

	LOGGER(ctx.mod->logger, on_data, subst);
	LOGGER(ctx.mod->logger, on_expr_end);

	return type;
}


// Checks whether pattern_type <c rhs_type, where <c refers to the subtype relation induced by the
// provided TypeConversion.
//
// Attention: Does not take lhs_pattern.provided_type into account.
static Type const* unify_pattern(
	Pattern &lhs_pattern,
	Type const &rhs_type,
	TypeConversion lhs_conv,
	TypeConversion rhs_conv,
	ConstraintSystem &constraints,
	SemaContext &ctx,
	// Output
	optional<IsMutable> &requires_lvalue
)
{
	return lhs_pattern | match
	{
		[&](VarPattern &p)
		{
			p.var->type = clone_ptr(&rhs_type, ctx.arena);
			return p.type = clone_ptr(p.var->type, ctx.arena);
		},
		[&](DerefPattern &p)
		{
			PointerType const *pointer_type = std::get_if<PointerType>(&rhs_type);
			if(not pointer_type)
				throw_sem_error("Invalid deref pattern: target type not a pointer", p.range.first, ctx.mod);

			Type const *sub_type = unify_pattern(
				*p.sub,
				*pointer_type->pointee,
				lhs_conv,
				rhs_conv,
				constraints,
				ctx,
				requires_lvalue
			);
			return p.type = clone_ptr(sub_type, ctx.arena);
		},
		[&](AddressOfPattern &p)
		{
			if(not requires_lvalue or *requires_lvalue == IsMutable::NO)
				requires_lvalue = p.mutability;

			Type sub_rhs_type(PointerType{
				.range = UNKNOWN_TOKEN_RANGE,
				.kind = PointerType::SINGLE,
				.pointee = clone_ptr(&rhs_type, ctx.arena),
				.mutability = p.mutability,
			});

			Type const *sub_type = unify_pattern(
				*p.sub,
				sub_rhs_type,
				lhs_conv,
				rhs_conv,
				constraints,
				ctx,
				requires_lvalue
			);

			return p.type = clone_ptr(sub_type, ctx.arena);
		},
		[&](ConstructorPattern &p) -> Type*
		{
			LazyErrorMsg error_msg(&lhs_pattern, [](Pattern const &pattern, string const &reason, Module const &mod)
			{
				throw_sem_error("Invalid constructor pattern: " + reason, token_range_of(pattern).first, &mod);
			});
			unify(
				*p.ctor, rhs_type,
				lhs_conv, rhs_conv,
				&constraints,
				{},
				ctx,
				error_msg
			);

			// Make sure the constructor is not a type variable
			if(VarType const *var = std::get_if<VarType>(p.ctor))
			{
				*var | match
				{
					[&](TypeParameterVar v)
					{
						throw_sem_error("Type parameters cannot be used as constructor patterns", v.range.first, ctx.mod);
					},
					[&](TypeDeductionVar)
					{
						assert(!"Constructor pattern is TypeDeductionVar");
					},
				};
			}

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

						Type param_type = clone(ctor_proc_type.inst->params->items[i], ctx.arena);

						if(arg.param_name.size())
							throw_sem_error("TODO: Support named arguments in constructor pattern", p.range.first, ctx.mod);

						unify_pattern(
							arg.pattern,
							param_type,
							lhs_conv,
							rhs_conv,
							constraints,
							ctx,
							requires_lvalue
						);
						arg.param_idx = i;
					}
				}
			}

			return p.type = clone_ptr(p.ctor, ctx.arena);
		},
		[&](WildcardPattern &p) -> Type*
		{
			return p.type = clone_ptr(&rhs_type, ctx.arena);
		},
		[&](VarPatternUnresolved&) -> Type* { assert(!"unify_pattern: VarPatternUnresolved"); },
	};
}

static Type const* typecheck_pattern(
	Pattern &lhs_pattern,
	Type const &rhs_type,
	bool irrefutable_pattern_required,
	ConstraintSystem &constraints,
	SemaContext &ctx,
	Expr *root_rhs_expr
)
{
	optional<IsMutable> requires_lvalue;
	if(lhs_pattern.provided_type)
	{
		// If pattern must be irrefutable:
		//   1. rhs_type <: provided_type
		// Otherwise:
		//   1. provided_type <: rhs_type
		//
		// In both cases:
		//   2. pattern_type <: provided_type

		auto error_msg = [](Pattern const &pattern, string const &reason, Module const &mod)
		{
			throw_sem_error(
				"Inferred type does not match specified type in let statement: " + reason,
				token_range_of(pattern).first,
				&mod
			);
		};

		if(irrefutable_pattern_required)
		{
			unify(
				*lhs_pattern.provided_type, rhs_type,
				TypeConversion::NONE, TypeConversion::IMPLICIT_CTOR,
				&constraints,
				{.on_type_conversion_right = mk_expr_converter(root_rhs_expr)},
				ctx,
				LazyErrorMsg(&lhs_pattern, error_msg)
			);
		}
		else
		{
			unify(
				rhs_type, *lhs_pattern.provided_type,
				TypeConversion::NONE, TypeConversion::IMPLICIT_CTOR,
				&constraints,
				{},
				ctx,
				LazyErrorMsg(&lhs_pattern, error_msg)
			);
		}

		return unify_pattern(
			lhs_pattern,
			*lhs_pattern.provided_type,
			TypeConversion::TRIVIAL, TypeConversion::NONE,
			constraints,
			ctx,
			requires_lvalue
		);
	}
	else
	{
		// If pattern must be irrefutable:
		//   rhs_type <: pattern_type
		// Otherwise:
		//   pattern_type <: rhs_type

		if(irrefutable_pattern_required)
		{
			return unify_pattern(
				lhs_pattern,
				rhs_type,
				TypeConversion::NONE,
				TypeConversion::IMPLICIT_CTOR,
				constraints,
				ctx,
				requires_lvalue
			);
		}
		else
		{
			return unify_pattern(
				lhs_pattern,
				rhs_type,
				TypeConversion::IMPLICIT_CTOR,
				TypeConversion::NONE,
				constraints,
				ctx,
				requires_lvalue
			);
		}
	}

	if(requires_lvalue)
		constraints.add_check(LValueCheck(root_rhs_expr, *requires_lvalue));
}


static void typecheck_stmt(Stmt &stmt, SemaContext &ctx)
{
	LOGGER(ctx.mod->logger, on_stmt_start, stmt);
	stmt | match
	{
		[&](LetStmt const &s)
		{
			if(s.init_expr)
			{
				LOGGER(ctx.mod->logger, on_expr_start, *s.init_expr);

				ConstraintSystem constraints(*ctx.mod);
				Type const *init_type = typecheck_subexpr(*s.init_expr, constraints, ctx);
				typecheck_pattern(*s.lhs, *init_type, true, constraints, ctx, s.init_expr);
				LOGGER(ctx.mod->logger, on_data, constraints);

				TypeEnv subst = create_subst_from_constraints(constraints, ctx);
				LOGGER(ctx.mod->logger, on_data, subst);

				substitute_types_in_expr(*s.init_expr, subst, ctx.mod->sema->insts, FullDeductionSubsitution(s.range));
				substitute_types_in_pattern(*s.lhs, subst, ctx.mod->sema->insts, FullDeductionSubsitution(s.range));

				LOGGER(ctx.mod->logger, on_expr_end);
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
				LOGGER(ctx.mod->logger, on_expr_start, *s.ret_expr);

				auto error_msg = [](Expr const &ret_expr, string const &reason, Module const &mod)
				{
					throw_sem_error("Invalid return expression: " + reason, token_range_of(ret_expr).first, &mod);
				};

				ConstraintSystem constraints(*ctx.mod);
				Type const *ret_expr_type = typecheck_subexpr(*s.ret_expr, constraints, ctx);

				unify(
					*ctx.proc->ret_type,
					*ret_expr_type,
					TypeConversion::NONE, TypeConversion::IMPLICIT_CTOR,
					&constraints,
					{.on_type_conversion_right = mk_expr_converter(s.ret_expr)},
					ctx,
					LazyErrorMsg(s.ret_expr, error_msg)
				);
				LOGGER(ctx.mod->logger, on_data, constraints);

				TypeEnv subst = create_subst_from_constraints(constraints, ctx);
				LOGGER(ctx.mod->logger, on_data, subst);

				substitute_types_in_expr(*s.ret_expr, subst, ctx.mod->sema->insts, FullDeductionSubsitution(s.range));

				LOGGER(ctx.mod->logger, on_expr_end);
			}
			else
			{
				if(not is_builtin_type(*ctx.proc->ret_type, BuiltinTypeDef::UNIT))
					throw_sem_error("Return statement must return unit value", s.range.first, ctx.mod);
			}
		},
		[&](IfStmt const &s)
		{
			Type const *condition_type = typecheck_expr(*s.condition, ctx);
			if(not is_builtin_type(*condition_type, BuiltinTypeDef::BOOL))
				throw_sem_error("If-condition must be boolean", s.range.first, ctx.mod);

			typecheck_stmt(*s.then, ctx);
			if(s.else_)
				typecheck_stmt(*s.else_, ctx);
		},
		[&](WhileStmt const &s)
		{
			Type const *condition_type = typecheck_expr(*s.condition, ctx);
			if(not is_builtin_type(*condition_type, BuiltinTypeDef::BOOL))
				throw_sem_error("While-condition must be boolean", s.range.first, ctx.mod);

			typecheck_stmt(*s.body, ctx);
		},
		[&](MatchStmt const &s)
		{
			Type const *expr_type = typecheck_expr(*s.expr, ctx);
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

						ConstraintSystem arm_constraints(*ctx.mod);
						typecheck_pattern(arm.capture, *type_of(*s.expr), false, arm_constraints, ctx, s.expr);
						TypeEnv subst_arm = create_subst_from_constraints(arm_constraints, ctx);

						substitute_types_in_pattern(
							arm.capture,
							subst_arm,
							ctx.mod->sema->insts,
							FullDeductionSubsitution(token_range_of(arm.capture))
						);

						if(is<WildcardPattern>(arm.capture))
							has_wildcard = true;
						else
						{
							StructType *arm_struct_type = std::get_if<StructType>(&type_of(arm.capture));
							if(not arm_struct_type)
								throw_sem_error("Must match against a case member", token_range_of(arm.capture).first, ctx.mod);

							StructInstance *arm_inst = arm_struct_type->inst;
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
				[&](UnionType const &union_type)
				{
					assert(union_type.inst->is_deduction_complete());

					UnionInstance *subject = union_type.inst;
					if(subject->occurring_vars().size())
					{
						// TODO
						// The reason for this current restriction is that we cannot know if the
						// alternatives of the union are actually distinct if type variables are
						// involved.
						//
						// For example, consider the following function:
						//
						//     proc foo'(S,T)(c: S | T)
						//     {
						//         match c {
						//             case let v: S {
						//                 ...
						//             }
						//             case let v: T {
						//                 ...
						//             }
						//         }
						//     }
						//
						// If foo is instantiated such that S and T refer to the same type, then
						// it's unclear which of the two case-clauses to keep.
						//
						// I think a good solution would be to require that the alternatives
						// mentioned in the match statement must be distinct under all possible
						// substitutions. (Note that this requirement is only necessary for the
						// alternatives actually mentioned in the match statement, not for all
						// alternatives of the union.)
						throw_sem_error(
							"Matching on a union that depends on type variables is not supported yet",
							token_range_of(*s.expr).first,
							ctx.mod
						);
					}

					vector<bool> matched_alts(subject->alternatives().size(), false);
					size_t num_matched_alts = 0;
					bool has_wildcard = false;
					for(MatchArm &arm: *s.arms)
					{
						if(has_wildcard)
						{
							throw_sem_error(
								"Pattern is following wildcard pattern and is therefore unreachable",
								token_range_of(arm.capture).first,
								ctx.mod
							);
						}

						ConstraintSystem arm_constraints(*ctx.mod);
						typecheck_pattern(arm.capture, *type_of(*s.expr), false, arm_constraints, ctx, s.expr);
						TypeEnv subst_arm = create_subst_from_constraints(arm_constraints, ctx);

						substitute_types_in_pattern(
							arm.capture,
							subst_arm,
							ctx.mod->sema->insts,
							FullDeductionSubsitution(token_range_of(arm.capture))
						);

						if(is<WildcardPattern>(arm.capture))
							has_wildcard = true;
						else
						{
							optional<size_t> alt_idx = subject->try_get_alt_idx(type_of(arm.capture));
							if(not alt_idx)
								throw_sem_error("Must match against an alternative of the union", token_range_of(arm.capture).first, ctx.mod);

							if(matched_alts[*alt_idx])
								throw_sem_error("Duplicate case value", token_range_of(arm.capture).first, ctx.mod);

							matched_alts[*alt_idx] = true;
							num_matched_alts += 1;
							arm.discr = *alt_idx;
						}

						typecheck_stmt(arm.stmt, ctx);
					}

					if(num_matched_alts != subject->alternatives().size() and not has_wildcard)
						throw_sem_error("Match is not exhaustive", s.range.first, ctx.mod);
				},
				[&](auto const&)
				{
					throw_sem_error("The match subject must refer to a struct or union type", token_range_of(*s.expr).first, ctx.mod);
				},
			};
		},
	};
	LOGGER(ctx.mod->logger, on_stmt_end);
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
					LOGGER(ctx.mod->logger, on_expr_start, *default_value);

					ConstraintSystem constraints(*ctx.mod);
					Type const *default_value_type = typecheck_subexpr(*default_value, constraints, ctx);
					LOGGER(ctx.mod->logger, on_data, constraints);

					auto error_msg = [](Expr const &init_expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Invalid default value: " + reason, token_range_of(init_expr).first, &mod);
					};
					unify(
						*var_member.type, *default_value_type,
						TypeConversion::NONE, TypeConversion::IMPLICIT_CTOR,
						&constraints,
						{.on_type_conversion_right = mk_expr_converter(default_value)},
						ctx,
						LazyErrorMsg(default_value, error_msg)
					);

					TypeEnv subst = create_subst_from_constraints(constraints, ctx);
					LOGGER(ctx.mod->logger, on_data, subst);

					substitute_types_in_expr(*default_value, subst, ctx.mod->sema->insts, FullDeductionSubsitution(token_range_of(*default_value)));

					LOGGER(ctx.mod->logger, on_expr_end);
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
				LOGGER(ctx.mod->logger, on_proc_start, &proc);

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

				LOGGER(ctx.mod->logger, on_proc_end);
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

	if(struct_->struct_()->name == "Option" and is<PointerType>(struct_->type_args().args->items[0]))
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
		[&](PointerType const&)
		{
			return MemoryLayout{.size = 8, .alignment = 8};
		},
		[&](StructType const &t)
		{
			assert(t.inst->is_concrete());
			if(parent_type_deps)
				parent_type_deps->insert(t.inst);

			StructInstance *cur = t.inst;
			while(cur->parent())
				cur = cur->parent();

			if(is_optional_ptr(cur))
				return MemoryLayout{.size = 8, .alignment = 8};

			return cur->compute_own_layout();
		},
		[&](UnionType const &t) -> MemoryLayout
		{
			if(parent_type_deps)
				parent_type_deps->insert(t.inst);

			return t.inst->layout();
		},

		[&](VarType const&) -> MemoryLayout { assert(!"compute_layout: VarType"); },
		[&](ProcType const&) -> MemoryLayout { assert(!"compute_layout: ProcType"); },
		[&](KnownIntType const&) -> MemoryLayout { assert(!"compute_layout: KnownIntType"); },
		[&](ProcTypeUnresolved const&) -> MemoryLayout { assert(!"compute_layout: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> MemoryLayout { assert(!"compute_layout: UnionTypeUnresolved"); },
		[&](Path const&) -> MemoryLayout { assert(!"compute_layout: Path"); },
		[&](InlineStructType const&) -> MemoryLayout { assert(!"compute_layout: InlineStructType"); },
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

	LOGGER(mod.logger, on_layout_computation_start);
	{
		compute_type_layouts(mod);
	}
	LOGGER(mod.logger, on_layout_computation_end);
}


//==============================================================================
// Event logger
//==============================================================================
EventLogger::EventLogger(Module *mod, std::ostream &os) :
	m_mod(mod),
	m_os(os)
{
	m_os <<
R"###(<!doctype html>
<html>
	<head>
		<title>Myca Compiler Log</title>

		<style>

pre.data {
	margin-top: 5px;
	padding: 5px;
	background-color: #eee;
}

code.myca-inline {
	background-color: #eee;
	white-space: pre;
	padding: 0 3px;
}

span.side-info {
	font-size: small;
	color: gray;
}

.entry-struct-subst {
	display: none;
}

		</style>
	</head>
	<body>
)###";
}

EventLogger::~EventLogger()
{
	try
	{
		m_os <<
R"###(
	</body>
</html>
)###" << std::flush;
	}
	catch(...) {}
}

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

void EventLogger::on_stmt_start(Stmt const &stmt)
{
	auto output_html = [&](optional<string> const &custom = nullopt)
	{
		m_os << "<li>Statement:";
			if(custom)
				m_os << " <code class='myca-inline'>" << *custom << "</code>";

			m_os << "<ul>\n";
			if(not custom)
			{
				m_os << "<li>\n";
					m_os << "<pre class='data'><code>";
					print(stmt, *m_mod, m_os); m_os << "</code></pre>";
				m_os << "</li>\n";
			}
	};

	stmt | match
	{
		[&](LetStmt const&)
		{
			output_html();
		},
		[&](ExprStmt const&)
		{
			output_html();
		},
		[&](BlockStmt const&) {},
		[&](ReturnStmt const&)
		{
			output_html();
		},
		[&](IfStmt const&)
		{
			output_html("if");
		},
		[&](WhileStmt const&)
		{
			output_html("while");
		},
		[&](MatchStmt const&)
		{
			output_html("match");
		},
	};

	m_stmt_stack.push_back(&stmt);
}

void EventLogger::on_stmt_end()
{
	Stmt const *stmt = m_stmt_stack.back();
	m_stmt_stack.pop_back();

	auto output_html = [&]()
	{
			m_os << "</ul>\n";
		m_os << "</li>\n";
	};

	*stmt | match
	{
		[&](LetStmt const&)
		{
			output_html();
		},
		[&](ExprStmt const&)
		{
			output_html();
		},
		[&](BlockStmt const&) {},
		[&](ReturnStmt const&)
		{
			output_html();
		},
		[&](IfStmt const&)
		{
			output_html();
		},
		[&](WhileStmt const&)
		{
			output_html();
		},
		[&](MatchStmt const&)
		{
			output_html();
		},
	};
}

void EventLogger::on_expr_start(Expr const &expr)
{
	m_os << "<li>Expression:\n";
		m_os << "<code class='myca-inline'>"; print(expr, *m_mod, m_os); m_os << "</code>";
		m_os << "<ul>\n";
}

void EventLogger::on_expr_end()
{
		m_os << "</ul>\n";
	m_os << "</li>\n";
}

void EventLogger::on_struct_substitution_start(StructInstance *inst)
{
	m_os << "<li class='entry-struct-subst'>Struct substitution: \n";
		m_os << "<code class='myca-inline'>"; print(StructType(UNKNOWN_TOKEN_RANGE, inst), *m_mod, m_os); m_os << "</code>";
		m_os << " <span class='side-info'>(" << inst << ")</span>\n";
		m_os << "<ul>\n";
}

void EventLogger::on_struct_substitution_replaced(StructInstance *inst)
{
	(void)inst;
			m_os << "<li>Replaced: ";
			m_os << "<code class='myca-inline'>"; print(StructType(UNKNOWN_TOKEN_RANGE, inst), *m_mod, m_os); m_os << "</code>";
			m_os << " <span class='side-info'>(" << inst << ")</span>\n";
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


void EventLogger::on_struct_register(StructInstance *inst)
{
	string_view deduction_state = "partially deduced";
	if(inst->is_deduction_complete())
		deduction_state = "fully deduced";

	m_os << "<li>Register struct: \n";
		m_os << "<code class='myca-inline'>"; print(StructType(UNKNOWN_TOKEN_RANGE, inst), *m_mod, m_os); m_os << "</code>";
		m_os << " <span class='side-info'>[" << deduction_state << "]</span>\n";
		m_os << " <span class='side-info'>[" << inst << "]</span>\n";
	m_os << "</li>\n";
}

void EventLogger::on_proc_register(ProcInstance *inst)
{
	string_view deduction_state = "partially deduced";
	if(inst->is_deduction_complete())
		deduction_state = "fully deduced";

	m_os << "<li>Register proc: \n";
		m_os << "<code class='myca-inline'>" << inst->proc()->name << "</code>";
		m_os << " <span class='side-info'>[" << deduction_state << "]</span>\n";
		m_os << " <span class='side-info'>[" << inst << "]</span>\n";
	m_os << "</li>\n";
}

void EventLogger::on_proc_start(ProcItem *proc)
{
	m_os << "<li>Procedure: ";
		m_os << "<code class='myca-inline'>";
		m_os << proc->name;
		m_os << "</code>";
		m_os << "<ul>\n";
}

void EventLogger::on_proc_end()
{
		m_os << "</ul>\n";
	m_os << "</li>\n";
}


void EventLogger::on_layout_computation_start()
{
	m_os << "<h2>Pass 4: Layout computation</h2>\n";
	m_os << "<ul>\n";
}

void EventLogger::on_layout_computation_end()
{
	m_os << "</ul>\n";
}

void EventLogger::on_struct_layout_computation_start(StructInstance *inst)
{
	m_os << "<li>Compute layout: \n";
		m_os << "<code class='myca-inline'>"; print(StructType(UNKNOWN_TOKEN_RANGE, inst), *m_mod, m_os); m_os << "</code>";
		m_os << " <span class='side-info'>(" << inst << ")</span>\n";

		m_os << "<ul>\n";
			m_os << "<li>Members:\n";
			m_os << "<ul>\n";
				for(Parameter const &m: inst->own_var_members())
				{
					m_os << "<li><code class='myca-inline'>";
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

void EventLogger::on_struct_layout_computation_end()
{
				m_os << "</ul>\n";
			m_os << "</li>\n";
		m_os << "</ul>\n";
	m_os << "</li>\n";
}

void EventLogger::on_data(ConstraintSystem const &sys)
{
	if(sys.checks.size() or sys.constraints.size())
	{
		m_os << "<li>Constraints:\n";
			m_os << "<pre class='data'><code>";
			sys.print(m_os);
			m_os << "</code></pre>\n";
		m_os << "</li>\n";
	}
}

void EventLogger::on_data(TypeEnv const &subst)
{
	if(not subst.empty())
	{
		m_os << "<li>Substitution:\n";
			m_os << "<pre class='data'><code>";
			subst.print(m_os, *m_mod);
			m_os << "</code></pre>\n";
		m_os << "</li>\n";
	}
}
