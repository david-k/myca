#include "semantics.hpp"
#include "syntax.hpp"
#include "utils.hpp"
#include <algorithm>
#include <expected>
#include <functional>
#include <limits>
#include <memory>
#include <ostream>
#include <ranges>
#include <unordered_map>
#include <variant>
#include <iostream>

using std::pair;


//==============================================================================
Type clone(Type const &type, Arena &arena);
TypeArgList clone(TypeArgList const &args, Arena &arena);
Expr clone(Expr const &expr, Arena &arena);

FixedArray<MatchArm>* clone(FixedArray<MatchArm> const *arms, Arena &arena);
FixedArray<Stmt>* clone(FixedArray<Stmt> const *stmts, Arena &arena);

template<typename T>
T* clone_ptr(T const *ptr, Arena &arena)
{
	if(not ptr)
		return nullptr;

	return arena.alloc<T>(clone(*ptr, arena));
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

FixedArray<Type>* clone(FixedArray<Type> const *types, Arena &arena)
{
	FixedArray<Type> *result = alloc_fixed_array<Type>(types->count, arena);
	for(size_t i = 0; i < types->count; ++i)
		result->items[i] = clone(types->items[i], arena);

	return result;
}

GenericArg clone(GenericArg const &arg, Arena &arena)
{
	return arg | match
	{
		[&](Type const &t) -> GenericArg { return clone(t, arena); },
		[&](Expr const &e) -> GenericArg { return clone(e, arena); },
	};
}

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
				.pointee = clone_ptr(t.pointee, arena),
				.kind = t.kind,
				.mutability = t.mutability,
			};
		},
		[&](ArrayType const &t) -> Type
		{
			return ArrayType{
				.range = t.range,
				.element = clone_ptr(t.element, arena),
				.count_arg = clone_ptr(t.count_arg, arena),
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
		[&](GenericVarExpr const &e) -> Expr
		{
			return GenericVarExpr{
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
		[&](DeclStmt const &s) -> Stmt
		{
			return s;
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

string mk_error_msg(string const &msg, TokenIdx tok_idx, Module const *mod)
{
	if(tok_idx.value == INVALID_TOKEN_IDX.value)
		return "error: " + msg;

	Token const &tok = mod->parser.token_at(tok_idx);
	return "|" + str(tok.span.begin) + "| error: " + msg;
}

[[noreturn]] static void throw_sem_error(string const &msg, TokenIdx tok_idx, Module const *mod)
{
	throw ParseError(mk_error_msg(msg, tok_idx, mod));
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

void Scope::declare_type_var(GenericParameter *def)
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
	if(ScopeItem *item = try_lookup(name, sloc, traverse_upwards))
		return *item;

	throw_sem_error("Name has not been declared: "s + name, sloc, mod);
}

ScopeItem* Scope::try_lookup(string_view const &name, TokenIdx sloc, bool traverse_upwards)
{
	auto it = items_by_name.find(name);
	if(it != items_by_name.end())
		return &it->second;

	if(parent && traverse_upwards)
		return &parent->lookup(name, sloc);

	return nullptr;
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

static void substitute(
	GenericArg &arg,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
);

static void substitute(
	TypeArgList &args,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
);


TypeEnv DeclContainerInst::create_type_env() const
{
	return *this | match
	{
		[](auto *inst) { return inst->create_type_env(); }
	};
}

bool DeclContainerInst::is_concrete() const
{
	return *this | match
	{
		[](auto *inst) { return inst->is_concrete(); }
	};
}

bool DeclContainerInst::is_deduction_complete() const
{
	return *this | match
	{
		[](auto *inst) { return inst->is_deduction_complete(); }
	};
}

void DeclContainerInst::finalize_typechecking()
{
	*this | match
	{
		[](StructInstance *inst) { inst->finalize_typechecking(); },
		[](ProcInstance*) {},
	};
}

TypeArgList const& DeclContainerInst::type_args() const
{
	return *this | match
	{
		[](StructInstance *inst) -> TypeArgList const& { return inst->type_args(); },
		[](ProcInstance *inst) -> TypeArgList const& { return inst->type_args(); },
	};
}

optional<DeclContainerInst> DeclContainerInst::decl_parent() const
{
	return *this | match
	{
		[](StructInstance *inst) { return inst->decl_parent(); },
		[](ProcInstance *inst) { return inst->decl_parent(); },
	};
}

Scope* DeclContainerInst::scope() const
{
	return *this | match
	{
		[](StructInstance *inst) { return inst->struct_()->sema->type_scope; },
		[](ProcInstance *inst) { return inst->proc()->sema->scope; },
	};
}


void TypeEnv::materialize(class InstanceRegistry &registry)
{
	for(auto &[_, gen_arg]: m_env)
	{
		if(Type *t = std::get_if<Type>(&gen_arg))
			substitute(*t, *this, registry, FullDeductionSubstitution());
	}
}

void TypeEnv::print(std::ostream &os, Module const &mod) const
{
	vector<std::pair<GenericVar, GenericArg const*>> sorted;
	for(auto const &[var, type]: m_env)
		sorted.push_back({var, &type});

	std::ranges::sort(sorted, [](auto const &a, auto const &b)
	{
		GenericVar va = a.first;
		GenericVar vb = b.first;

		if(va.index() < vb.index())
			return true;

		return va | match
		{
			[&](GenericParameterVar p)
			{
				return p.def->name < std::get<GenericParameterVar>(vb).def->name;
			},
			[&](GenericDeductionVar d)
			{
				return d.def->id < std::get<GenericDeductionVar>(vb).def->id;
			}
		};
	});

	for(auto const &[var, type]: sorted)
	{
		::print(var, os);
		os << " ==> ";
		::print(*type, mod, os);
		os << std::endl;
	}
}

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

struct TypeStatInfo
{
	void merge(TypeStatInfo other)
	{
		has_type_deduction_vars |= other.has_type_deduction_vars;
		has_known_ints |= other.has_known_ints;
	}

	bool has_type_deduction_vars = false;
	bool has_known_ints = false;
};

static TypeStatInfo gather_type_vars(Type const &type, unordered_set<GenericVar> &type_vars, bool deduction_vars_only = false);

static TypeStatInfo gather_type_vars(DeclContainerInst cont, unordered_set<GenericVar> &type_vars, bool deduction_vars_only = false)
{
	TypeStatInfo info;
	if(cont.decl_parent())
		info.merge(gather_type_vars(*cont.decl_parent(), type_vars, deduction_vars_only));

	TypeArgList const &type_args = cont | match
	{
		[&](StructInstance *decl_parent_inst) -> TypeArgList const&
		{
			return decl_parent_inst->type_args();
		},
		[&](ProcInstance *decl_parent_inst) -> TypeArgList const&
		{
			return decl_parent_inst->type_args();
		},
	};

	for(GenericVar v: type_args.occurring_vars)
		gather_type_vars(VarType(UNKNOWN_TOKEN_RANGE, v), type_vars, deduction_vars_only);

	info.has_type_deduction_vars |= type_args.has_type_deduction_vars;
	info.has_known_ints |= type_args.has_known_ints;

	return info;
}

static TypeStatInfo gather_type_vars(Expr const &expr, unordered_set<GenericVar> &type_vars, bool deduction_vars_only)
{
	TypeStatInfo info;
	traverse(expr, match
	{
		[&](GenericVarExpr const &e)
		{
			info.merge(gather_type_vars(VarType(e.range, e.var), type_vars, deduction_vars_only));
		},
		[](auto const&) {}
	});

	return info;
}

static TypeStatInfo gather_type_vars(GenericArg const &arg, unordered_set<GenericVar> &type_vars, bool deduction_vars_only)
{
	return arg | match
	{
		[&](Type const &t) { return gather_type_vars(t, type_vars, deduction_vars_only); },
		[&](Expr const &e) { return gather_type_vars(e, type_vars, deduction_vars_only); },
	};
}

static TypeStatInfo gather_type_vars(Type const &type, unordered_set<GenericVar> &type_vars, bool deduction_vars_only)
{
	return type | match
	{
		[&](BuiltinType const&) { return TypeStatInfo{}; },
		[&](KnownIntType const&) { return TypeStatInfo{.has_known_ints = true}; },
		[&](PointerType const &t)
		{
			return gather_type_vars(*t.pointee, type_vars, deduction_vars_only);
		},
		[&](ArrayType const &t)
		{
			assert(t.count_arg);

			TypeStatInfo info;
			info.merge(gather_type_vars(*t.count_arg, type_vars, deduction_vars_only));
			info.merge(gather_type_vars(*t.element, type_vars, deduction_vars_only));
			return info;
		},
		[&](ProcType const&) -> TypeStatInfo
		{
			assert(!"gather_type_vars: ProcType");
		},
		[&](StructType const &t)
		{
			return gather_type_vars(t.inst, type_vars, deduction_vars_only);
		},
		[&](UnionType const &t)
		{
			if(not deduction_vars_only or t.inst->has_type_deduction_vars())
			{
				for(GenericVar v: t.inst->occurring_vars())
					gather_type_vars(Type(VarType(UNKNOWN_TOKEN_RANGE, v)), type_vars, deduction_vars_only);
			}

			return TypeStatInfo{
				.has_type_deduction_vars = t.inst->has_type_deduction_vars(),
				.has_known_ints = t.inst->has_known_ints(),
			};
		},
		[&](VarType const &t)
		{
			if(std::holds_alternative<GenericDeductionVar>(t.var) or not deduction_vars_only)
				type_vars.insert(t.var);

			return TypeStatInfo{.has_type_deduction_vars = std::holds_alternative<GenericDeductionVar>(t.var)};
		},
		[&](ProcTypeUnresolved const&) -> TypeStatInfo { assert(!"gather_type_vars: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> TypeStatInfo { assert(!"gather_type_vars: UnionTypeUnresolved"); },
		[&](Path const&) -> TypeStatInfo { assert(!"gather_type_vars: Path"); },
		[&](InlineStructType const&) -> TypeStatInfo { assert(!"gather_type_vars: InlineStructType"); },
	};
}

static TypeStatInfo get_type_stats(GenericArg const &arg)
{
	// TODO Change gather_type_vars() so that we don't need to pass type_vars when we don't need it
	unordered_set<GenericVar> type_vars;
	return gather_type_vars(arg, type_vars, false);
}

static bool is_deduction_complete(GenericArg const &arg)
{
	TypeStatInfo info = get_type_stats(arg);
	return not info.has_type_deduction_vars and not info.has_known_ints;
}

static TypeArgList create_type_arg_list(FixedArray<GenericArg> *NULLABLE type_args, Arena &arena)
{
	if(not type_args)
		type_args = alloc_fixed_array<GenericArg>(0, arena);

	TypeArgList arg_list{.args = type_args};
	for(GenericArg const &arg: *type_args)
	{
		TypeStatInfo info = gather_type_vars(arg, arg_list.occurring_vars, false);
		arg_list.has_type_deduction_vars |= info.has_type_deduction_vars;
		arg_list.has_known_ints |= info.has_known_ints;
	}

	return arg_list;
}

static void check_struct_parents(StructItem const *struct_, optional<DeclContainerInst> decl_parent)
{
	if(struct_->sema->decl_parent)
	{
		*struct_->sema->decl_parent | match
		{
			[&](StructItem *parent_item)
			{
				assert(decl_parent and decl_parent->try_as_struct()->struct_() == parent_item);
			},
			[&](ProcItem *parent_item)
			{
				assert(decl_parent and decl_parent->as_proc()->proc() == parent_item);
			},
		};
	}
	else {
		assert(not decl_parent.has_value());
	}
}

// `type_args` must already be resolved
StructInstance* InstanceRegistry::get_struct_instance(
	StructItem const *struct_,
	FixedArray<GenericArg> *NULLABLE type_args,
	optional<DeclContainerInst> decl_parent
)
{
	check_struct_parents(struct_, decl_parent);

	auto it = m_struct_instances.find(StructInstanceKey(struct_, type_args, decl_parent));
	if(it != m_struct_instances.end())
		return &it->second;

	TypeArgList type_arg_list = create_type_arg_list(type_args, m_arena);
	return add_struct_instance(StructInstance(struct_, type_arg_list, decl_parent, next_struct_id(), this));
}

StructInstance* InstanceRegistry::get_struct_instance(
	StructItem const *struct_,
	TypeArgList const &type_args,
	optional<DeclContainerInst> decl_parent
)
{
	check_struct_parents(struct_, decl_parent);

	auto it = m_struct_instances.find(StructInstanceKey(struct_, type_args.args, decl_parent));
	if(it != m_struct_instances.end())
		return &it->second;

	return add_struct_instance(StructInstance(struct_, type_args, decl_parent, next_struct_id(), this));
}

StructInstance* InstanceRegistry::get_struct_instance(
	StructItem const *struct_,
	TypeArgList const &type_args,
	TypeEnv const &subst,
	optional<DeclContainerInst> decl_parent,
	SubstitutionMode mode
)
{
	check_struct_parents(struct_, decl_parent);

	void *original_mem_pos = m_arena.current_ptr();
	TypeArgList new_args = clone(type_args, m_arena);
	substitute(new_args, subst, *this, mode);

	auto it = m_struct_instances.find(StructInstanceKey(struct_, new_args.args, decl_parent));
	if(it != m_struct_instances.end())
	{
		// Free `new_args` since we only needed it for lookup
		m_arena.set_current_ptr(original_mem_pos);
		return &it->second;
	}

	return add_struct_instance(StructInstance(struct_, new_args, decl_parent, next_struct_id(), this));
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

static GenericArg mk_generic_arg(GenericParameter const *param, TokenRange range)
{
	return param->kind | match
	{
		[&](GenericTypeParameter const&) -> GenericArg
		{
			return VarType(range, GenericParameterVar(param));
		},
		[&](GenericValueParameter const&) -> GenericArg
		{
			return GenericVarExpr(range, GenericParameterVar(param));
		},
	};
}

static GenericArg mk_generic_arg(GenericDeductionVar var)
{
	return var.def->kind | match
	{
		[&](TypeDeductionVar const&) -> GenericArg
		{
			return VarType(UNKNOWN_TOKEN_RANGE, var);
		},
		[&](ValueDeductionVar const&) -> GenericArg
		{
			return GenericVarExpr(UNKNOWN_TOKEN_RANGE, var);
		},
	};
}

StructInstance* InstanceRegistry::get_struct_self_instance(StructItem const *struct_)
{
	auto it = m_struct_self_instances.find(struct_);
	if(it != m_struct_self_instances.end())
		return it->second;

	FixedArray<GenericArg> *type_args = nullptr;
	if(struct_->type_params->count)
	{
		type_args = alloc_fixed_array<GenericArg>(struct_->type_params->count, m_arena);
		for(auto const &[idx, type_param]: *struct_->type_params | std::views::enumerate)
			new (type_args->items+idx) GenericArg(mk_generic_arg(&type_param, type_param.range));
	}

	optional<DeclContainerInst> decl_parent_inst = nullopt;
	if(struct_->sema->decl_parent)
		decl_parent_inst = get_self_instance(*struct_->sema->decl_parent);

	StructInstance *self_inst = get_struct_instance(struct_, type_args, decl_parent_inst);
	m_struct_self_instances.insert({struct_, self_inst});

	return self_inst;
}

ProcInstance* InstanceRegistry::get_proc_self_instance(ProcItem const *proc)
{
	auto it = m_proc_self_instances.find(proc);
	if(it != m_proc_self_instances.end())
		return it->second;

	FixedArray<GenericArg> *type_args = nullptr;
	if(proc->type_params->count)
	{
		type_args = alloc_fixed_array<GenericArg>(proc->type_params->count, m_arena);
		for(auto const &[idx, type_param]: *proc->type_params | std::views::enumerate)
			new (type_args->items+idx) GenericArg(mk_generic_arg(&type_param, type_param.range));
	}

	ProcInstance *self_inst = get_proc_instance(proc, type_args);
	m_proc_self_instances.insert({proc, self_inst});

	return self_inst;
}

DeclContainerInst InstanceRegistry::get_self_instance(DeclContainer decl)
{
	return decl | match
	{
		[&](StructItem *parent_item) -> DeclContainerInst
		{
			return get_struct_self_instance(parent_item);
		},
		[&](ProcItem *parent_item) -> DeclContainerInst
		{
			return get_proc_self_instance(parent_item);
		}
	};
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

std::generator<ProcTypeInstance&> InstanceRegistry::proc_type_instances()
{
	for(auto &[_, proc_type]: m_proc_type_instances)
		co_yield proc_type;
}


// `type_args` must already be resolved and must not contain KnownIntTypes
ProcInstance* InstanceRegistry::get_proc_instance(ProcItem const *proc, FixedArray<GenericArg> *NULLABLE type_args)
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
	unordered_set<GenericVar> occurring_vars;
	TypeStatInfo info = gather_type_vars(*ret, occurring_vars);
	for(Type const &type: *params)
		info.merge(gather_type_vars(type, occurring_vars));

	ProcTypeInstance *inst = &m_proc_type_instances.emplace(
		ProcTypeInstanceKey(params, ret), 
		ProcTypeInstance{
			.params = params,
			.ret = ret,
			.occurring_vars = std::move(occurring_vars),
			.has_type_deduction_vars = info.has_type_deduction_vars,
			.has_known_ints = info.has_known_ints,
		}
	).first->second;

	for(InstanceRegistryListener *l: m_listeners)
		l->on_new_proc_type_instance(inst);

	return inst;
}

UnionInstance* InstanceRegistry::get_union_instance(vector<Type*> &&alternatives)
{
	UnionInstanceKey key{alternatives};
	auto it = m_union_instances.find(key);
	if(it != m_union_instances.end())
		return &it->second;

	return add_union_instance(std::move(alternatives));
}

bool operator < (Type const &a, Type const &b);

UnionInstance* InstanceRegistry::add_union_instance(vector<Type*> &&alternatives)
{
	unordered_set<GenericVar> occurring_vars;
	TypeStatInfo info;
	for(Type const *alt: alternatives)
		info.merge(gather_type_vars(*alt, occurring_vars));

	UnionInstanceKey key(alternatives);
	UnionInstance *inst = &m_union_instances.emplace(
		key,
		UnionInstance(std::move(alternatives), std::move(occurring_vars), info.has_type_deduction_vars, info.has_known_ints)
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
	FixedArray<GenericParameter> const *type_params,
	FixedArray<GenericArg> const *type_args,
	TypeEnv &result,
	InstanceRegistry &registry)
{
	for(size_t i = 0; i < type_params->count; ++i)
	{
		GenericParameterVar type_param(&type_params->items[i]);
		GenericArg type_arg = type_args->items[i];

		// See tests struct_recursive_type_param_<N> for why we call substitute() and perform the
		// type_var_occurs_in() check
		substitute(type_arg, result, registry, BestEffortSubstitution());
		if(not equiv(mk_generic_arg(type_param.def, UNKNOWN_TOKEN_RANGE), type_arg))
		{
			if(type_var_occurs_in(type_param, type_arg))
				throw_sem_error(
					"The type argument " + str(type_args->items[i], registry.mod()) + " causes the type term to grow indefinitely",
					token_range_of(type_args->items[i]).first,
					&registry.mod()
				);

			result.add(type_param, type_arg);
		}
	}
}

static TypeEnv create_type_env(
	FixedArray<GenericParameter> const *type_params,
	FixedArray<GenericArg> const *type_args,
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
	if(m_decl_parent)
		env = m_decl_parent->create_type_env();

	::create_type_env(m_struct->type_params, m_type_args.args, env, *m_registry);

	return env;
}

bool StructInstance::is_concrete() const
{
	bool parent_concrete = m_decl_parent ? m_decl_parent->is_concrete() : true;
	return parent_concrete && m_type_args.occurring_vars.empty() and not m_type_args.has_known_ints;
}

bool StructInstance::is_deduction_complete() const
{
	bool parent_is_deduction_complete = m_decl_parent ?
		m_decl_parent->is_deduction_complete() : true;

	return parent_is_deduction_complete and not m_type_args.has_type_deduction_vars and not m_type_args.has_known_ints;
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
	assert(is_case_member());

	if(m_decl_parent)
	{
		if(StructInstance *parent_inst = m_decl_parent->try_as_struct())
		{
			if(not parent_inst->m_dependent_properties_computed)
				parent_inst->compute_dependent_properties();
		}
	}

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
	if(is_case_member())
		co_yield std::ranges::elements_of(m_decl_parent->as_struct()->initial_var_members());

	co_yield std::ranges::elements_of(own_var_members());

	if(is_case_member())
		co_yield std::ranges::elements_of(m_decl_parent->as_struct()->trailing_var_members());
}


void StructInstance::compute_dependent_properties()
{
	TypeEnv env = create_type_env();

	// Initialize the struct's own members (m_members)
	size_t num_instance_members = m_struct->num_var_members + m_struct->num_case_members;
	m_members = alloc_fixed_array<InstanceMember>(num_instance_members, m_registry->arena());
	int member_idx = 0;
	int case_idx = 0;
	for(Member const &m: *m_struct->members)
	{
		m | match
		{
			[&](VarMember const &var_member)
			{
				Parameter inst_var_member{
					.range = var_member.var.range,
					.type = clone_ptr(var_member.var.type, m_registry->arena()),
					// Why ExprPending?
					// - First, when typechecking the *usage* of a struct, we only need to know
					//   whether a member *has* a default value, but it doesn't matter what that
					//   default value is. This is war ExprPending() accomplishes.
					// - Second, if we directly assigned the default value expression here, we would
					//   also need to substitute the type parameters in it. However,
					//   var_member.default_value has not been type-checked at this point, and in
					//   order to typecheck var_member.default_value we may have to typecheck the
					//   default values of other structs, which may well end up in a cycle
					.default_value = var_member.var.default_value ?
						DefaultValueExpr(ExprPending()) : DefaultValueExpr(NoDefaultValue()),
				};
				substitute(*inst_var_member.type, env, *m_registry, BestEffortSubstitution());

				new (m_members->items+member_idx) InstanceMember(inst_var_member);
				member_idx += 1;
			},
			[&](CaseMember case_member)
			{
				assert(case_member.struct_->type_params->count == 0);
				StructInstance *case_member_inst = m_registry->get_struct_instance(case_member.struct_, nullptr, this);
				case_member_inst->m_case_idx = case_idx;
				new (m_members->items+member_idx) InstanceMember(case_member_inst);
				if(case_member.struct_->is_implicit)
					m_implicit_case = case_member_inst;

				case_idx += 1;
				member_idx += 1;
			},
			[&](StructMember) {}
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

	if(m_decl_parent)
		m_decl_parent->finalize_typechecking();

	// TODO Store and re-use the `env` created in compute_dependent_properties()
	TypeEnv env = create_type_env();

	// Substitute type vars in default values
	for(size_t i = 0; i < m_struct->members->count; ++i)
	{
		if(VarMember const *var_member = std::get_if<VarMember>(&m_struct->members->items[i]))
		{
			Parameter &inst_var_member = std::get<Parameter>(m_members->items[i]);
			if(Expr *default_value = var_member->var.default_value.try_get_expr())
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
	if(is_case_member())
		return m_decl_parent->as_struct()->layout();

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

	if(a.decl_parent != b.decl_parent)
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
void flatten_union_alternatives(vector<Type*> &alternatives, Arena &arena)
{
	auto it = alternatives.begin();
	while(it != alternatives.end())
	{
		if(UnionType const *sub_union = std::get_if<UnionType>(*it))
		{
			it = alternatives.erase(it);
			for(Type const *sub_alt: sub_union->inst->alternatives())
				it = alternatives.insert(it, clone_ptr(sub_alt, arena));
		}
		else
			++it;
	}
}

void canonicalize_union_alternatives(vector<Type*> &alternatives, Arena &arena)
{
	// 1. Flatten
	// 2. Sort
	// 3. Remove duplicates

	flatten_union_alternatives(alternatives, arena);

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

vector<Type*> canonicalize_union_alternatives(FixedArray<Type> *alternatives, Arena &arena)
{
	vector<Type*> result;
	result.reserve(alternatives->count);
	for(Type &alt: *alternatives)
		result.push_back(&alt);

	canonicalize_union_alternatives(result, arena);

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

static Type* typecheck_subexpr(Expr &expr, ConstraintGatheringSubst &subst, SemaContext &ctx);

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
// Pass 1: Declare procs, structs and aliases
//==============================================================================
static void declare_struct_item(StructItem *struct_, optional<DeclContainer> decl_parent, Scope *scope, SemaContext &ctx);

static void declare_types(Type const &type, Scope *scope, optional<DeclContainer> decl_parent, SemaContext &ctx)
{
	type | match
	{
		[&](BuiltinType const&) {},
		[&](KnownIntType const&) {},
		[&](PointerType const &t)
		{
			declare_types(*t.pointee, scope, decl_parent, ctx);
		},
		[&](ArrayType const &t)
		{
			declare_types(*t.element, scope, decl_parent, ctx);
		},
		[&](ProcTypeUnresolved const &t)
		{
			for(Type const &param: *t.params)
				declare_types(param, scope, decl_parent, ctx);

			declare_types(*t.ret, scope, decl_parent, ctx);
		},
		[&](UnionTypeUnresolved const &t)
		{
			for(Type &alt: *t.alternatives)
				declare_types(alt, scope, decl_parent, ctx);
		},
		[&](Path const&) {},
		[&](InlineStructType const &t)
		{
			declare_struct_item(t.struct_, decl_parent, scope, ctx);
		},
		[&](StructType const&) {},
		[&](ProcType const&) {},
		[&](UnionType const&) {},
		[&](VarType const&) {},
	};
}

static void declare_types_in_pattern(Pattern &pattern, Scope *scope, optional<DeclContainer> decl_parent, SemaContext &ctx)
{
	if(pattern.provided_type)
		declare_types(*pattern.provided_type, scope, decl_parent, ctx);
}

static void declare_types_in_stmt(Stmt &stmt, Scope *scope, optional<DeclContainer> decl_parent, SemaContext &ctx)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			declare_types_in_pattern(*s.lhs, scope, decl_parent, ctx);
		},
		[&](ExprStmt const&) {},
		[&](BlockStmt &s)
		{
			// For example, in declare_item(), the scope of a proc's BlockStmt is set to the scope
			// of the proc
			if(not s.scope)
				s.scope = scope->new_child(true);

			for(Stmt &child_stmt: *s.stmts)
				declare_types_in_stmt(child_stmt, s.scope, decl_parent, ctx);
		},
		[&](ReturnStmt const&) {},
		[&](IfStmt const &s)
		{
			declare_types_in_stmt(*s.then, scope, decl_parent, ctx);
			if(s.else_)
				declare_types_in_stmt(*s.else_, scope, decl_parent, ctx);
		},
		[&](WhileStmt const &s)
		{
			declare_types_in_stmt(*s.body, scope, decl_parent, ctx);
		},
		[&](DeclStmt const &s)
		{
			declare_struct_item(s.item, decl_parent, scope, ctx);
		},
		[&](MatchStmt &s)
		{
			for(MatchArm &arm: *s.arms)
			{
				arm.scope = scope->new_child(true);
				declare_types_in_stmt(arm.stmt, arm.scope, decl_parent, ctx);
			}
		},
	};
}

static std::generator<Parameter&> initial_var_members(StructItem *struct_)
{
	optional<int> next_idx = struct_->first_initial_var_member_idx;
	while(next_idx)
	{
		VarMember &var_member = std::get<VarMember>(struct_->members->items[*next_idx]);
		co_yield var_member.var;
		next_idx = var_member.next_var_member_idx;
	}
}

static std::generator<Parameter&> trailing_var_members(StructItem *struct_)
{
	optional<int> next_idx = struct_->first_trailing_var_member_idx;
	while(next_idx)
	{
		VarMember &var_member = std::get<VarMember>(struct_->members->items[*next_idx]);
		co_yield var_member.var;
		next_idx = var_member.next_var_member_idx;
	}
}

static std::generator<Parameter&> all_var_members(StructItem *struct_)
{
	if(struct_->is_case_member)
		co_yield std::ranges::elements_of(initial_var_members(struct_->sema->decl_parent->as_struct()));

	co_yield std::ranges::elements_of(initial_var_members(struct_));
	co_yield std::ranges::elements_of(trailing_var_members(struct_));

	if(struct_->is_case_member)
		co_yield std::ranges::elements_of(trailing_var_members(struct_->sema->decl_parent->as_struct()));
}

std::generator<StructItem*> own_case_members(StructItem *struct_)
{
	optional<int> next_idx = struct_->first_case_member_idx;
	while(next_idx)
	{
		CaseMember &case_member = std::get<CaseMember>(struct_->members->items[*next_idx]);
		co_yield case_member.struct_;
		next_idx = case_member.next_case_member_idx;
	}
}

static void declare_struct_item(StructItem *struct_, optional<DeclContainer> decl_parent, Scope *scope, SemaContext &ctx)
{
	assert(not struct_->ctor_without_parens or struct_->members->count == 0);

	scope->declare_struct(struct_);
	struct_->sema = ctx.arena.alloc<Struct>(decl_parent, scope->new_child(true));
	if(struct_->is_case_member)
		struct_->sema->variant_depth = decl_parent->as_struct()->sema->variant_depth + 1;

	// Declare type parameters
	size_t num_type_params = struct_->type_params->count;
	for(size_t i = 0; i < num_type_params; ++i)
	{
		GenericParameter &type_param = struct_->type_params->items[i];
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
			[&](VarMember const &var_member)
			{
				if(state == INITIAL_VAR_MEMBERS)
					struct_->sema->num_initial_var_members += 1;

				if(state == CASE_MEMBERS)
					state = TRAILING_VAR_MEMBERS;

				declare_types(*var_member.var.type, struct_->sema->type_scope, struct_, ctx);
				bool inserted = member_names.insert(name_of(var_member.var, ctx.mod)).second;
				if(not inserted)
					throw_sem_error("A member with this name has already been declared", var_member.var.range.first, ctx.mod);
			},
			[&](CaseMember case_member)
			{
				if(state == INITIAL_VAR_MEMBERS)
					state = CASE_MEMBERS;
				else if(state != CASE_MEMBERS)
					throw_sem_error("Variable members must come before or after all case members", case_member.struct_->range.first, ctx.mod);

				bool inserted = member_names.insert(case_member.struct_->name).second;
				if(not inserted)
					throw_sem_error("A member with this name has already been declared", case_member.struct_->range.first, ctx.mod);

				if(case_member.struct_->type_params->count)
					throw_sem_error("Case members cannot be generic", case_member.struct_->range.first, ctx.mod);

				declare_struct_item(case_member.struct_, struct_, struct_->sema->type_scope, ctx);

				if(case_member.struct_->ctor_without_parens and struct_->num_var_members)
					throw_sem_error("Struct must have braces because it inherits members from parent", case_member.struct_->range.first, ctx.mod);
			},
			[&](StructMember struct_member)
			{
				declare_struct_item(struct_member.struct_, struct_, struct_->sema->type_scope, ctx);
			},
		};
	}

	// If the struct has a constructor, create an array with all the constructor parameters, i.e.,
	// all the variable members.
	if(struct_->has_constructor())
	{
		size_t param_count = 0;
		for(StructItem *cur_item = struct_; cur_item;)
		{
			param_count += cur_item->num_var_members;
			cur_item = cur_item->is_case_member ? cur_item->sema->decl_parent->as_struct() : nullptr;
		}

		struct_->sema->ctor_params = alloc_fixed_array<Parameter const*>(param_count, ctx.arena);
		for(auto const &[idx, var_member]: all_var_members(struct_) | std::views::enumerate)
			struct_->sema->ctor_params->items[idx] = &var_member;
	}
}

void declare_item(TopLevelItem &item, SemaContext &ctx)
{
	item | match
	{
		[&](ProcItem &proc)
		{
			proc.sema = ctx.arena.alloc<Proc>(ctx.mod->sema->globals->new_child(true));
			proc.sema->param_vars = alloc_fixed_array<Var*>(proc.params->count, ctx.arena);
			for(auto const& [idx, param]: *proc.params | std::views::enumerate)
			{
				declare_types(*param.type, proc.sema->scope, &proc, ctx);
				Var *param_var = proc.sema->scope->declare_var(name_of(param, ctx.mod), IsMutable::NO, param.range);
				proc.sema->param_vars->items[idx] = param_var;
			}

			declare_types(*proc.ret_type, proc.sema->scope, &proc, ctx);

			size_t num_type_params = proc.type_params->count;
			for(size_t i = 0; i < num_type_params; ++i)
			{
				GenericParameter &type_param = proc.type_params->items[i];
				proc.sema->scope->declare_type_var(&type_param);
			}

			if(proc.body)
			{
				std::get<BlockStmt>(*proc.body).scope = proc.sema->scope;
				declare_types_in_stmt(*proc.body, proc.sema->scope, &proc, ctx);
			}

			ctx.mod->sema->globals->declare_proc(&proc);
		},
		[&](StructItem &struct_)
		{
			declare_struct_item(&struct_, nullopt, ctx.mod->sema->globals.get(), ctx);
		},
		[&](AliasItem &alias)
		{
			alias.sema = ctx.arena.alloc<Alias>(ctx.mod->sema->globals->new_child(false));
			declare_types(*alias.aliased_type, alias.sema->scope, nullopt, ctx);

			size_t num_type_params = alias.type_params->count;
			for(size_t i = 0; i < num_type_params; ++i)
			{
				GenericParameter &type_param = alias.type_params->items[i];
				alias.sema->scope->declare_type_var(&type_param);
			}

			ctx.mod->sema->globals->declare_alias(&alias);
		},
	};
}

static void declare_items(SemaContext &ctx)
{
	ctx.mod->sema = std::make_unique<SemaModule>(std::make_unique<Scope>(ctx.mod, true), ctx.arena);
	for(TopLevelItem &item: to_range(ctx.mod->items.list()))
		declare_item(item, ctx);
}


//==============================================================================
// Pass 2: Path resolution
//==============================================================================
static void resolve_expr(Expr &expr, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx);
static void resolve_generic_arg(GenericArg &arg, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx);
static void resolve_alias(AliasItem &alias, SemaContext &ctx);

static Type resolve_path_to_type(Path const &path, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx);
static Expr resolve_path_to_expr(Path const &path, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx);

static FixedArray<GenericArg>* resolve_type_args(
	FixedArray<GenericArg> *NULLABLE args,
	FixedArray<GenericParameter> *NULLABLE params,
	Scope *scope,
	ResolutionContext res_ctx,
	SemaContext &ctx
)
{
	size_t num_args = args ? args->count : 0;
	assert(num_args <= params->count);
	FixedArray<GenericArg> *resolved_type_args = alloc_fixed_array<GenericArg>(params->count, ctx.arena);

	size_t cur_idx = 0;
	if(num_args)
	{
		for(GenericArg const &arg: *args)
		{
			resolved_type_args->items[cur_idx] = arg;
			resolve_generic_arg(resolved_type_args->items[cur_idx], scope, res_ctx, ctx);
			++cur_idx;
		}
	}

	for(size_t param_idx = num_args; param_idx < params->count; ++param_idx)
	{
		GenericParameter const &param = params->items[param_idx];
		param.kind | match
		{
			[&](GenericTypeParameter const&)
			{
				GenericDeductionVar var = ctx.new_deduction_var(TypeDeductionVar());
				new (&resolved_type_args->items[cur_idx++]) GenericArg(VarType(UNKNOWN_TOKEN_RANGE, var));
			},
			[&](GenericValueParameter const &p)
			{
				GenericDeductionVar var = ctx.new_deduction_var(ValueDeductionVar(p.type));
				new (&resolved_type_args->items[cur_idx++]) GenericArg(GenericVarExpr(UNKNOWN_TOKEN_RANGE, var));
			},
		};
	}

	return resolved_type_args;
}


enum class PathSegment
{
	// This is the first segment of the complete path
	HEAD,
	// This is one of the remaining segments of the complete path
	TAIL,
};

// The lookup of the head of `path` starts at `decl_parent` (or `ambient_scope` is
// there is no `decl_parent`). The type args of `path` are always looked up in `ambient_scope`. The
// resolved type/procedure of the head of `path` becomes the `decl_parent` when resolving
// `path.child`.
//
// Example 1:
// ---------
//
//   proc foo'T() -> A.B'(T) {…}
//
// Assume we want to resolve the path `A.B'(T)`:
//
// 1. path=A.B'(T), decl_parent=foo, ambient_scope=scope_of(foo)
//    - `A` is looked up in the decl_parent `foo`
//    - `resolve_path()` is then called recursively with the remaining path and the decl_parent set to `A`
//
// 2. path=B'(T), decl_parent=A, ambient_scope=scope_of(foo)
//    - `B` is looked up in the decl_parent `A`
//    - The type argument `T` is looked up in the ambient scope `scope_of(foo)`
//
// Example 2:
// ---------
//
//   struct S {
//       struct X {}
//       x: X
//   };
//
// Assume we want to resolve the path `X` occuring in the member declaration of `x`. This consists
// of a single step:
//
// 1. path=X, decl_parent=S, ambient_scope=scope_of(S)
//    - `X` is looked up in the decl_parent `S`
static variant<Expr, Type> resolve_path(
	Path const &path,
	optional<DeclContainerInst> path_parent,
	Scope *ambient_scope,
	ResolutionContext res_ctx,
	SemaContext &ctx
)
{
	ScopeItem *resolved_item = nullptr;
	if(path_parent)
		resolved_item = &path_parent->scope()->lookup(name_of(path, ctx.mod), path.range.first, false);
	else
	{
		string_view name = name_of(path, ctx.mod);
		if(name == "?")
			name = "Option";
		else if(name == "!")
			name = "Result";

		resolved_item = &ambient_scope->lookup(name, path.range.first);
	}

	return *resolved_item | match
	{
		[&](StructItem *struct_) -> variant<Expr, Type>
		{
			if(path.type_args->count > struct_->type_params->count)
				throw_sem_error("Too many type arguments", path.range.first, ctx.mod);

			FixedArray<GenericArg> *resolved_type_args = resolve_type_args(path.type_args, struct_->type_params, ambient_scope, res_ctx, ctx);

			optional<DeclContainerInst> parent_inst = path_parent;
			if(not parent_inst and struct_->sema->decl_parent)
				parent_inst = ctx.mod->sema->insts.get_self_instance(*struct_->sema->decl_parent);

			StructInstance *inst = ctx.mod->sema->insts.get_struct_instance(struct_, resolved_type_args, parent_inst);
			return path.child ?
				resolve_path(*path.child, inst, ambient_scope, res_ctx, ctx) :
				StructType(path.range, inst);
		},
		[&](ProcItem *proc) -> variant<Expr, Type>
		{
			if(path.type_args->count > proc->type_params->count)
				throw_sem_error("Too many type arguments", path.range.first, ctx.mod);

			FixedArray<GenericArg> *resolved_type_args = resolve_type_args(path.type_args, proc->type_params, ambient_scope, res_ctx, ctx);
			ProcInstance *inst = ctx.mod->sema->insts.get_proc_instance(proc, resolved_type_args);;
			return path.child ?
				resolve_path(*path.child, inst, ambient_scope, res_ctx, ctx) :
				ProcExpr(path.range, inst);
		},
		[&](AliasItem *alias) -> variant<Expr, Type>
		{
			assert(not path_parent.has_value());

			resolve_alias(*alias, ctx);

			if(path.type_args->count > alias->type_params->count)
				throw_sem_error("Too many type arguments", path.range.first, ctx.mod);

			FixedArray<GenericArg> *resolved_type_args = resolve_type_args(path.type_args, alias->type_params, ambient_scope, res_ctx, ctx);
			TypeEnv env = create_type_env(alias->type_params, resolved_type_args, ctx.mod->sema->insts);

			Type type = clone(*alias->aliased_type, ctx.arena);
			substitute(type, env, ctx.mod->sema->insts, BestEffortSubstitution());

			if(path.child)
			{
				StructType const *struct_type = std::get_if<StructType>(&type);
				if(not struct_type)
					throw_sem_error("Type has no members", path.range.first, ctx.mod);

				return resolve_path(*path.child, struct_type->inst, ambient_scope, res_ctx, ctx);
			}

			return type;
		},
		[&](GenericParameter *type_param) -> variant<Expr, Type>
		{
			if(path.type_args->count)
				throw_sem_error("Cannot apply type arguments to type variable", path.range.first, ctx.mod);

			if(path.child)
				throw_sem_error("Member selection of type parameters not supported", path.child->range.first, ctx.mod);

			return type_param->kind | match
			{
				[&](GenericTypeParameter) -> variant<Expr, Type>
				{
					return VarType(path.range, GenericParameterVar(type_param));
				},
				[&](GenericValueParameter const&) -> variant<Expr, Type>
				{
					return GenericVarExpr(path.range, GenericParameterVar(type_param));
				},
			};
		},
		[&](Var const &var) -> variant<Expr, Type>
		{
			if(path_parent.has_value())
				throw_sem_error("Cannot access local variables from the outside", path.range.first, ctx.mod);

			if(path.type_args->count)
				throw_sem_error("Cannot apply type arguments to variable", path.range.first, ctx.mod);

			// Accessing local variables in a default value expression is forbidden:
			//
			//   proc foo()
			//   {
			//       let a = 3;
			//       struct S
			//       {
			//           value: i32 = a, // Causes the error below
			//       };
			//   }
			//
			// The reason is that the struct can be accessed from the outside using `foo.S` where
			// the value of the local variable is not available.
			if(res_ctx == ResolutionContext::DEFAULT_VALUE)
				throw_sem_error("Cannot access local variables in default value expression", path.range.first, ctx.mod);

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

static Type resolve_path_to_type(Path const &path, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	variant<Expr, Type> resolved_path = resolve_path(path, nullopt, scope, res_ctx, ctx);
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

static Expr resolve_path_to_expr(Path const &path, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	variant<Expr, Type> resolved_path = resolve_path(path, nullopt, scope, res_ctx, ctx);
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

static void resolve_generic_arg(GenericArg &arg, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	arg | match
	{
		[&](Type &t)
		{
			resolve_type(t, scope, res_ctx, ctx);
		},
		[&](Expr &e) { resolve_expr(e, scope, res_ctx, ctx); },
	};
}

static void resolve_struct(StructItem &struct_, SemaContext &ctx);

void resolve_type(Type &type, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	type | match
	{
		[&](BuiltinType&) {},
		[&](KnownIntType&) {},
		[&](PointerType &t)
		{
			resolve_type(*t.pointee, scope, res_ctx, ctx);
		},
		[&](ArrayType &t)
		{
			resolve_type(*t.element, scope, res_ctx, ctx);
			if(t.count_arg)
				resolve_expr(*t.count_arg, scope, res_ctx, ctx);
			else
			{
				assert(!"[TODO] resolve_type: ArrayType: UnknownCount");
				// TODO Create a GenericDeductionVar for the count
			}
		},
		[&](ProcTypeUnresolved &t)
		{
			for(Type &param: *t.params)
				resolve_type(param, scope, res_ctx, ctx);

			resolve_type(*t.ret, scope, res_ctx, ctx);
			assert(!"[TODO] resolve_type: ProcTypeUnresolved");
		},
		[&](UnionTypeUnresolved &t)
		{
			for(Type &alt: *t.alternatives)
				resolve_type(alt, scope, res_ctx, ctx);

			vector<Type*> canonical_alts = canonicalize_union_alternatives(t.alternatives, ctx.arena);
			type = UnionType{
				.range = t.range,
				.inst = ctx.mod->sema->insts.get_union_instance(std::move(canonical_alts)),
			};
		},
		[&](Path &path)
		{
			type = resolve_path_to_type(path, scope, res_ctx, ctx);
		},
		[&](InlineStructType &t)
		{
			resolve_struct(*t.struct_, ctx);

			// What should happen if you delcare a generic inline struct?
			//
			//   let x: struct X'S { v: S } = X(3);
			//
			// (Ignoring the fact that it doesn't look like a useful feature. But who the hell
			// knows.)
			//
			// What I decided to do is to rewrite the declaration of `x` as follows:
			//
			//   let x: X'(?_0) = X'(i32)(3);
			//
			// Since `struct X'S { v: S }` is a type constructor and not an actual type I
			// automatically generate GenericDeductionVars for its type arguments. After type checking,
			// the type of `x` will be deduced as `X'(i32)`.

			optional<DeclContainerInst> decl_parent_inst;
			if(t.struct_->sema->decl_parent)
				decl_parent_inst = ctx.mod->sema->insts.get_self_instance(*t.struct_->sema->decl_parent);

			FixedArray<GenericArg> *type_args = resolve_type_args(nullptr, t.struct_->type_params, scope, res_ctx, ctx);
			StructInstance *inst = ctx.mod->sema->insts.get_struct_instance(t.struct_, type_args, decl_parent_inst);

			type = StructType(t.range, inst);
		},
		[&](StructType const&) {},
		[&](ProcType const&) {},
		[&](UnionType const&) {},
		[&](VarType const&) {},
	};
}

static void resolve_expr(Expr &expr, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	expr | match
	{
		[&](IntLiteralExpr const&) {},
		[&](BoolLiteralExpr const&) {},
		[&](StringLiteralExpr const&) {},
		[&](UnaryExpr const &e)
		{
			resolve_expr(*e.sub, scope, res_ctx, ctx);
		},
		[&](BinaryExpr const &e)
		{
			resolve_expr(*e.left, scope, res_ctx, ctx);
			resolve_expr(*e.right, scope, res_ctx, ctx);
		},
		[&](AddressOfExpr const &e)
		{
			resolve_expr(*e.object, scope, res_ctx, ctx);
		},
		[&](DerefExpr const &e)
		{
			resolve_expr(*e.addr, scope, res_ctx, ctx);
		},
		[&](IndexExpr const &e)
		{
			resolve_expr(*e.addr, scope, res_ctx, ctx);
			resolve_expr(*e.index, scope, res_ctx, ctx);
		},
		[&](MemberAccessExpr const &e)
		{
			resolve_expr(*e.object, scope, res_ctx, ctx);
		},
		[&](AssignmentExpr const &e)
		{
			resolve_expr(*e.lhs, scope, res_ctx, ctx);
			resolve_expr(*e.rhs, scope, res_ctx, ctx);
		},
		[&](AsExpr const &e)
		{
			resolve_expr(*e.src_expr, scope, res_ctx, ctx);
			resolve_type(*e.target_type, scope, res_ctx, ctx);
		},
		[&](ConstructorExpr const &e)
		{
			resolve_type(*e.ctor, scope, res_ctx, ctx);
		},
		[&](ProcExpr const&) {},
		[&](CallExpr const &e)
		{
			resolve_expr(*e.callable, scope, res_ctx, ctx);

			for(Argument &arg: *e.args)
				resolve_expr(arg.expr, scope, res_ctx, ctx);
		},
		[&](SizeOfExpr const &e)
		{
			resolve_type(*e.subject, scope, res_ctx, ctx);
		},
		[&](MakeExpr const &e)
		{
			resolve_expr(*e.addr, scope, res_ctx, ctx);
			resolve_expr(*e.init, scope, res_ctx, ctx);
		},
		[&](Path const &p)
		{
			expr = resolve_path_to_expr(p, scope, res_ctx, ctx);
		},
		[&](VarExpr const&) {},
		[&](GenericVarExpr const&) {},
		[&](UnionInitExpr const &e)
		{
			resolve_expr(*e.alt_expr, scope, res_ctx, ctx);
		},
	};
}

static void resolve_pattern(Pattern &pattern, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
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
			resolve_pattern(*p.sub, scope, res_ctx, ctx);
		},
		[&](AddressOfPattern const &p)
		{
			resolve_pattern(*p.sub, scope, res_ctx, ctx);
		},
		[&](ConstructorPattern const &p)
		{
			resolve_type(*p.ctor, scope, res_ctx, ctx);
			for(PatternArgument &arg: *p.args)
				resolve_pattern(arg.pattern, scope, res_ctx, ctx);
		},
		[&](WildcardPattern const&) {}
	};

	if(pattern.provided_type)
		resolve_type(*pattern.provided_type, scope, res_ctx, ctx);
}

static void resolve_stmt(Stmt &stmt, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			// Resolve init expr before the lhs pattern so the init expr cannot access the declared
			// variable
			if(s.init_expr)
				resolve_expr(*s.init_expr, scope, res_ctx, ctx);

			resolve_pattern(*s.lhs, scope, res_ctx, ctx);
		},
		[&](ExprStmt const &s)
		{
			resolve_expr(*s.expr, scope, res_ctx, ctx);
		},
		[&](BlockStmt const &s)
		{
			for(Stmt &child_stmt: *s.stmts)
				resolve_stmt(child_stmt, s.scope, res_ctx, ctx);
		},
		[&](ReturnStmt const &s)
		{
			resolve_expr(*s.ret_expr, scope, res_ctx, ctx);
		},
		[&](IfStmt const &s)
		{
			resolve_expr(*s.condition, scope, res_ctx, ctx);
			resolve_stmt(*s.then, scope, res_ctx, ctx);
			if(s.else_)
				resolve_stmt(*s.else_, scope, res_ctx, ctx);
		},
		[&](WhileStmt const &s)
		{
			resolve_expr(*s.condition, scope, res_ctx, ctx);
			resolve_stmt(*s.body, scope, res_ctx, ctx);
		},
		[&](DeclStmt const &s)
		{
			resolve_struct(*s.item, ctx);
		},
		[&](MatchStmt const &s)
		{
			resolve_expr(*s.expr, scope, res_ctx, ctx);
			for(MatchArm &arm: *s.arms)
			{
				resolve_pattern(arm.capture, arm.scope, res_ctx, ctx);
				resolve_stmt(arm.stmt, arm.scope, res_ctx, ctx);
			}
		},
	};
}

static void resolve_param(Parameter &param, Scope *scope, SemaContext &ctx)
{
	if(param.type)
		resolve_type(*param.type, scope, ResolutionContext::DEFAULT_VALUE, ctx);

	if(Expr *default_value = param.default_value.try_get_expr())
		resolve_expr(*default_value, scope, ResolutionContext::DEFAULT_VALUE, ctx);
}

static void resolve_struct(StructItem &struct_, SemaContext &ctx)
{
	for(Member &member: *struct_.members)
	{
		member | match
		{
			[&](VarMember &var_member)
			{
				assert(var_member.var.type);
				resolve_param(var_member.var, struct_.sema->type_scope, ctx);
				if(not is_deduction_complete(*var_member.var.type))
				{
					// I was playing with the idea of allowing type deduction basically everywhere,
					// including struct members:
					//
					//   struct Foo'T
					//   {
					//       x: Option = Some(3),
					//   }
					//
					// Here, the type of `x` would be deduced as `Option'i32`. However, the problem
					// is that type deduction happens type checking, and type checking is done one
					// struct/proc at a time. If, for example, some other struct `Bar` is using
					// `Foo` and is type-checked before it, then the type for `x` would not be
					// deduced yet. This could be solved by lazily type-checking on demand, but then
					// I would also need to take care of cycles (though I believe
					// `check_default_value_deps()` already takes care of this?).
					//
					// This is too much trouble for a feature that was the result of asking "Can I?"
					// instead of "Should I?". Nobody is going to miss this feature.
					throw_sem_error(
						"Member type of \""s + name_of(var_member.var, ctx.mod) + "\" is incomplete",
						var_member.var.range.first,
						ctx.mod
					);
				}
			},
			[&](CaseMember case_member)
			{
				resolve_struct(*case_member.struct_, ctx);
			},
			[&](StructMember struct_member)
			{
				resolve_struct(*struct_member.struct_, ctx);
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
	resolve_type(*alias.aliased_type, alias.sema->scope, ResolutionContext::DEFAULT_VALUE, ctx);
	if(not is_deduction_complete(*alias.aliased_type))
	{
		// Having an alias alternative like
		//
		//   typealias U = struct A'T { v: T } | Foo;
		//
		// doesn't make sense because type constructors are not first-class. What you can do instead
		// is to lift the type parameter to the typealias:
		//
		//   struct A'T { v: T }
		//   typealias U'T = A'T | Foo;
		//
		throw_sem_error("Aliased type is incomplete", alias.range.first, ctx.mod);
	}

	alias.sema->resolution_state = ResolutionState::DONE;
}

void resolve_item(TopLevelItem &item, SemaContext &ctx)
{
	item | match
	{
		[&](ProcItem &proc)
		{
			for(Parameter &param: *proc.params)
			{
				resolve_param(param, proc.sema->scope, ctx);
				if(not is_deduction_complete(*param.type))
					throw_sem_error("Parameter type is incomplete", token_range_of(*param.type).first, ctx.mod);
			}

			resolve_type(*proc.ret_type, proc.sema->scope, ResolutionContext::GENERAL, ctx);
			if(not is_deduction_complete(*proc.ret_type))
				throw_sem_error("Return type is incomplete", token_range_of(*proc.ret_type).first, ctx.mod);

			if(proc.body)
				resolve_stmt(*proc.body, proc.sema->scope, ResolutionContext::GENERAL, ctx);
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

static void resolve_names(SemaContext &ctx)
{
	for(TopLevelItem &item: to_range(ctx.mod->items.list()))
		resolve_item(item, ctx);
}


//--------------------------------------------------------------------
// Type substitution
//--------------------------------------------------------------------
bool have_common_vars(unordered_set<GenericVar> const &occurring_vars, TypeEnv const &env)
{
	for(GenericVar const &var: occurring_vars)
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
	for(GenericArg const &arg: *args.args)
	{
		TypeStatInfo info = gather_type_vars(arg, args.occurring_vars, false);
		args.has_type_deduction_vars |= info.has_type_deduction_vars;
		args.has_known_ints |= info.has_known_ints;
	}
}

void validate_unsubstituted_var(GenericVar var, SubstitutionMode mode, Module const &mod)
{
	if(is<FullSubstitution>(mode)) {
		assert(!"VarType has not been substituted");
	}

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

static ProcInstance* substitute_types_in_proc(
	ProcInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode,
	bool *modified = nullptr
);

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

	if(parent_modified or inst->type_args().needs_subsitution(env))
	{
		inst = registry.get_struct_instance(inst->struct_(), inst->type_args(), env, new_decl_parent, mode);

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
	SubstitutionMode mode,
	bool *modified
)
{
	if(inst->type_args().needs_subsitution(env))
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
	if(inst->needs_subsitution(env))
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
	if(inst->needs_subsitution(env))
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

static void substitute(
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
			if(optional<GenericArg> result = get_substituted(t.var, env, registry, mode))
				type = std::get<Type>(*result);
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
		[&](GenericVarExpr &e)
		{
			if(optional<GenericArg> result = get_substituted(e.var, env, registry, mode))
				expr = std::get<Expr>(*result);
		},
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
			return ta.var == tb.var;
		},
		[&](PointerType const &ta)
		{
			PointerType const &tb = std::get<PointerType>(b);
			return ta.kind == tb.kind and ta.mutability == tb.mutability and equiv(*ta.pointee, *tb.pointee);
		},
		[&](ArrayType const &ta)
		{
			ArrayType const &tb = std::get<ArrayType>(b);
			assert(ta.count_arg and tb.count_arg);
			return equiv(*ta.count_arg, *tb.count_arg) and equiv(*ta.element, *tb.element);
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

static bool equiv(Expr const &a, Expr const &b)
{
	if(a.index() != b.index())
		return false;

	return a | match
	{
		[&](IntLiteralExpr const &ea)
		{
			IntLiteralExpr const &eb = std::get<IntLiteralExpr>(b);
			return ea.value == eb.value;
		},
		[&](BoolLiteralExpr const &ea)
		{
			BoolLiteralExpr const &eb = std::get<BoolLiteralExpr>(b);
			return ea.value == eb.value;
		},
		[&](StringLiteralExpr const &ea)
		{
			StringLiteralExpr const &eb = std::get<StringLiteralExpr>(b);
			return ea.kind == eb.kind and ea.value == eb.value;
		},
		[&](UnaryExpr const &ea)
		{
			UnaryExpr const &eb = std::get<UnaryExpr>(b);
			return ea.op == eb.op and equiv(*ea.sub, *eb.sub);
		},
		[&](BinaryExpr const &ea)
		{
			BinaryExpr const &eb = std::get<BinaryExpr>(b);
			return ea.op == eb.op and equiv(*ea.left, *eb.left) and equiv(*ea.right, *eb.right);
		},
		[&](AddressOfExpr const &ea)
		{
			AddressOfExpr const &eb = std::get<AddressOfExpr>(b);
			return ea.mutability == eb.mutability and equiv(*ea.object, *eb.object);
		},
		[&](DerefExpr const &ea)
		{
			DerefExpr const &eb = std::get<DerefExpr>(b);
			return equiv(*ea.addr, *eb.addr);
		},
		[&](IndexExpr const &ea)
		{
			IndexExpr const &eb = std::get<IndexExpr>(b);
			return equiv(*ea.addr, *eb.addr) and equiv(*ea.index, *eb.index);
		},
		[&](MemberAccessExpr const &ea)
		{
			MemberAccessExpr const &eb = std::get<MemberAccessExpr>(b);
			return ea.member == eb.member and equiv(*ea.object, *eb.object);
		},
		[&](AssignmentExpr const &ea)
		{
			AssignmentExpr const &eb = std::get<AssignmentExpr>(b);
			return equiv(*ea.lhs, *eb.lhs) and equiv(*ea.rhs, *eb.rhs);
		},
		[&](AsExpr const &ea)
		{
			AsExpr const &eb = std::get<AsExpr>(b);
			return equiv(*ea.src_expr, *eb.src_expr) and equiv(*ea.target_type, *eb.target_type);
		},
		[&](ConstructorExpr const &ea)
		{
			ConstructorExpr const &eb = std::get<ConstructorExpr>(b);
			return equiv(*ea.ctor, *eb.ctor);
		},
		[&](ProcExpr const &ea)
		{
			ProcExpr const &eb = std::get<ProcExpr>(b);
			return ea.inst == eb.inst;
		},
		[&](CallExpr const &ea)
		{
			CallExpr const &eb = std::get<CallExpr>(b);
			if(not equiv(*ea.callable, *eb.callable))
				return false;

			if(ea.args->count != eb.args->count)
				return false;

			for(size_t i = 0; i < ea.args->count; ++i)
			{
				Argument const &arg_a = ea.args->items[i];
				Argument const &arg_b = eb.args->items[i];
				if(arg_a.param_idx != arg_b.param_idx) return false;
				if(arg_a.name != arg_b.name) return false;
				if(not equiv(arg_a.expr, arg_b.expr)) return false;
			}

			return true;
		},
		[&](SizeOfExpr const &ea)
		{
			SizeOfExpr const &eb = std::get<SizeOfExpr>(b);
			if(not ea.subject or not eb.subject)
				return ea.subject == eb.subject;

			return equiv(*ea.subject, *eb.subject);
		},
		[&](MakeExpr const&) -> bool
		{
			assert(!"[TODO] substitute_types_in_expr: MakeExpr");
		},
		[&](UnionInitExpr const &ea)
		{
			UnionInitExpr const &eb = std::get<UnionInitExpr>(b);
			return equiv(*ea.alt_expr, *eb.alt_expr) and equiv(*ea.alt_type, *eb.alt_type);
		},
		[&](VarExpr const &ea)
		{
			VarExpr const &eb = std::get<VarExpr>(b);
			return ea.var == eb.var;
		},
		[&](GenericVarExpr const &ea)
		{
			GenericVarExpr const &eb = std::get<GenericVarExpr>(b);
			return ea.var == eb.var;
		},
		[&](Path const&) ->bool { assert(!"substitute_types_in_expr: Path"); },
	};
}

bool equiv(GenericArg const &a, GenericArg const &b)
{
	if(a.index() != b.index())
		return false;

	return a | match
	{
		[&](Type const &t) { return equiv(t, std::get<Type>(b)); },
		[&](Expr const &e) { return equiv(e, std::get<Expr>(b)); },
	};
}

static bool operator < (GenericVar const &a, GenericVar const &b)
{
	if(a.index() != b.index())
		return a.index() < b.index();

	return a | match
	{
		[&](GenericParameterVar va)
		{
			// TODO Use something other than pointers to make comparison deterministic
			return va.def < std::get<GenericParameterVar>(b).def;
		},
		[&](GenericDeductionVar va)
		{
			return va.def->id < std::get<GenericDeductionVar>(b).def->id;
		},
	};
}

static bool operator < (Expr const &a, Expr const &b)
{
	if(a.index() != b.index())
		return a.index() < b.index();

	return a | match
	{
		[&](IntLiteralExpr const &ea)
		{
			return ea.value < std::get<IntLiteralExpr>(b).value;
		},
		[&](GenericVarExpr const &ea)
		{
			return ea.var < std::get<GenericVarExpr>(b).var;
		},
		[&](auto const&) -> bool
		{
			assert(!"[TODO] operator <: Expr");
		}
	};
}

static bool operator < (GenericArg const &a, GenericArg const &b)
{
	if(a.index() != b.index())
		return a.index() < b.index();

	return a | match
	{
		[&](Type const &t) { return t < std::get<Type>(b); },
		[&](Expr const &e) { return e < std::get<Expr>(b); },
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
			return ta.var < tb.var;
		},
		[&](PointerType const &ta)
		{
			PointerType const &tb = std::get<PointerType>(b);
			return std::tie(ta.kind, ta.mutability, *ta.pointee) < std::tie(tb.kind, tb.mutability, *tb.pointee);
		},
		[&](ArrayType const &ta)
		{
			ArrayType const &tb = std::get<ArrayType>(b);
			assert(ta.count_arg and tb.count_arg);
			return std::tie(*ta.count_arg, *ta.element) < std::tie(*tb.count_arg, *tb.element);
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
		.pointee = pointee,
		.kind = PointerType::SINGLE,
		.mutability = mutability,
	});
}


//--------------------------------------------------------------------
// Unification
//--------------------------------------------------------------------
[[noreturn]] static void throw_unification_error(
	GenericArg const &left,
	GenericArg const &right,
	optional<LazyErrorMsg> error_msg,
	Module const *mod
)
{
	// TO make error messages more predictable (for testing)
	if(right < left)
		throw_unification_error(right, left, error_msg, mod);

	string reason = "Incompatible types " + str(left, *mod) + " and " + str(right, *mod);
	if(error_msg)
		error_msg->throw_error(reason, *mod);
	else
		throw ParseError(reason);

	UNREACHABLE;
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

void apply_conversion(TypeConversionEvent const &conv, Expr *expr, Arena &arena)
{
	if(not conv.info)
		return;

	*conv.info | match
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
				.alt_type = clone_ptr(&c.alt, arena),
				.type = arena.alloc<Type>(UnionType(UNKNOWN_TOKEN_RANGE, c.union_)),
			});
		},
	};
}


// Unification of integer types
//--------------------------------------------------------------------
static optional<TypeConversion> try_convert_integer_type(
	Type const &src_type,
	Type const &target_type,
	TypeConversion conv
)
{
	assert(is_integer_type(src_type));
	assert(is_integer_type(target_type));

	if(is<KnownIntType>(target_type))
		return TypeConversion::NONE;

	if(KnownIntType const *src = std::get_if<KnownIntType>(&src_type))
	{
		BuiltinTypeDef target = std::get<BuiltinType>(target_type).builtin;
		if(not integer_assignable_to(target, src->low) or not integer_assignable_to(target, src->high))
			return nullopt;

		return TypeConversion::NONE;
	}

	switch(conv)
	{
		case TypeConversion::NONE:
		case TypeConversion::TRIVIAL:
			return equiv(src_type, target_type) ? optional(TypeConversion::NONE) : nullopt;

		case TypeConversion::IMPLICIT_CTOR:
		{
			BuiltinTypeDef src = std::get<BuiltinType>(src_type).builtin;
			BuiltinTypeDef target = std::get<BuiltinType>(target_type).builtin;
			return builtin_losslessly_convertible(target, src) ? optional(TypeConversion::IMPLICIT_CTOR) : nullopt;
		}
	}

	UNREACHABLE;
}

bool Unifier::try_unify_integer_types()
{
	Type const &left = std::get<Type>(*m_left);
	Type const &right = std::get<Type>(*m_right);

	if(not is_integer_type(left) or not is_integer_type(right))
		return false;

	optional<Type> common_type = common_int_type(left, right);
	if(not common_type)
		throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

	optional<TypeConversion> act_conv_left = try_convert_integer_type(left, *common_type, m_conv_left);
	if(not act_conv_left)
		throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

	optional<TypeConversion> act_conv_right = try_convert_integer_type(right, *common_type, m_conv_right);
	if(not act_conv_right)
		throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

	if(m_result)
		*m_result = *common_type;

	if(*act_conv_left > TypeConversion::NONE)
		emit_conversion(*act_conv_left, m_expr_left);

	if(*act_conv_right > TypeConversion::NONE)
		emit_conversion(*act_conv_right, m_expr_right);

	return true;
}


// Unification of type deduction variables
//--------------------------------------------------------------------
static bool type_var_occurs_in(GenericVar var, Expr const &expr)
{
	bool result = false;
	traverse(expr, match
	{
		[&](GenericVarExpr const &e)
		{
			if(e.var == var)
				result = true;
		},
		[&](auto const&) {}
	});

	return result;
}

bool type_var_occurs_in(GenericVar var, Type const &type)
{
	return type | match
	{
		[&](BuiltinType const&) { return false; },
		[&](KnownIntType const&) { return false; },
		[&](VarType const &t)
		{
			return t.var == var;
		},
		[&](PointerType const &t)
		{
			return type_var_occurs_in(var, *t.pointee);
		},
		[&](ArrayType const &t)
		{
			assert(t.count_arg);
			return type_var_occurs_in(var, *t.count_arg) or type_var_occurs_in(var, *t.element);
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

bool type_var_occurs_in(GenericVar var, GenericArg const &arg)
{
	return arg | match
	{
		[&](Type const &type) { return type_var_occurs_in(var, type); },
		[&](Expr const &expr) { return type_var_occurs_in(var, expr); }
	};
}

optional<GenericDeductionVar> get_if_type_deduction_var(Type const &type)
{
	VarType const *var_type = std::get_if<VarType>(&type);
	if(not var_type)
		return nullopt;

	GenericDeductionVar const* type_deduction_var = std::get_if<GenericDeductionVar>(&var_type->var);
	if(not type_deduction_var)
		return nullopt;

	return *type_deduction_var;
}

optional<GenericDeductionVar> get_if_type_deduction_var(Expr const &expr)
{
	GenericVarExpr const *var = std::get_if<GenericVarExpr>(&expr);
	if(not var)
		return nullopt;

	GenericDeductionVar const* type_deduction_var = std::get_if<GenericDeductionVar>(&var->var);
	if(not type_deduction_var)
		return nullopt;

	return *type_deduction_var;
}

optional<GenericDeductionVar> get_if_deduction_var(GenericArg const &arg)
{
	return arg | match
	{
		[](Type const &t) { return get_if_type_deduction_var(t); },
		[](Expr const &e) { return get_if_type_deduction_var(e); },
	};
}

bool Unifier::try_unify_type_deduction_vars()
{
	optional<GenericDeductionVar> left_var = get_if_deduction_var(*m_left);
	optional<GenericDeductionVar> right_var = get_if_deduction_var(*m_right);

	if(GenericArg const* arg = left_var ? try_lookup(*left_var) : nullptr)
	{
		Unifier(*this)
			.left(*arg, m_conv_left, m_expr_left)
			.right(*m_right, m_conv_right, m_expr_right)
			.set(m_subst)
			.result(m_result)
			.go();

		return true;
	}

	if(GenericArg const *arg = right_var ? try_lookup(*right_var) : nullptr)
	{
		Unifier(*this)
			.left(*m_left, m_conv_left, m_expr_left)
			.right(*arg, m_conv_right, m_expr_right)
			.set(m_subst)
			.result(m_result)
			.go();

		return true;
	}

	if(left_var)
	{
		if(left_var != right_var)
		{
			// Classical first-order unification would check that the variable does not occur in the
			// right-hand side:
			//
			//   assert(not type_var_occurs_in(*left_var, *m_right));
			//
			// However, in our case constraints like `_a ->> Option'(_a)` are actuall valid and so
			// we do not perform that check.

			m_subst->set(
				*left_var, m_conv_left, m_expr_left,
				*m_right, m_conv_right, m_expr_right,
				m_err
			);
		}

		if(m_result)
		{
			// It's important here to chose the right side as the result so the constraint solver
			// can make progress (the left side is pretty boring since it is just a
			// GenericDeductionVar, while the right side might be an actual type, giving the constraint
			// solver additional information)
			*m_result = *m_right;
		}

		return true;
	}

	if(right_var)
		return sides_swapped().try_unify_type_deduction_vars();

	return false;
}


// Unification of struct types
//--------------------------------------------------------------------
static Type const* try_find_matching_alt_for(
	UnionInstance *union_,
	Type const &type,
	Expr *NULLABLE expr,
	optional<LazyErrorMsg> error_msg,
	Subst *subst,
	SemaContext const &ctx
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
	optional<LazyErrorMsg> error_msg,
	Subst *subst,
	SemaContext const &ctx
)
{
	assert(left.args->count == right.args->count);
	for(size_t i = 0; i < left.args->count; ++i)
	{
		Unifier(ctx)
			.left(left.args->items[i], TypeConversion::NONE)
			.right(right.args->items[i], TypeConversion::NONE)
			.set(subst)
			.set(error_msg)
			.go();
	}
}

static void unify_structs_eq(
	StructInstance *left,
	StructInstance *right,
	optional<LazyErrorMsg> error_msg,
	Subst *subst,
	SemaContext const &ctx
)
{
	if(left != right)
	{
		if(left->is_deduction_complete() && right->is_deduction_complete())
		{
			throw_unification_error(
				StructType(UNKNOWN_TOKEN_RANGE, left),
				StructType(UNKNOWN_TOKEN_RANGE, right),
				error_msg,
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
					error_msg,
					ctx.mod
				);
			}

			optional<DeclContainerInst> left_cont{left};
			optional<DeclContainerInst> right_cont{right};
			do
			{
				unify_type_args(left_cont->type_args(), right_cont->type_args(), error_msg, subst, ctx);
				left_cont = left_cont->decl_parent();
				right_cont = right_cont->decl_parent();
			}
			while(left_cont);
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
	int min_depth = std::min(left->variant_depth(), right->variant_depth());
	while(left->variant_depth() > min_depth)
		left = left->variant_parent();

	while(right->variant_depth() > min_depth)
		right = right->variant_parent();

	do
	{
		if(left->struct_() == right->struct_())
			return {left, right};

		left = left->variant_parent();
		right = right->variant_parent();
	} while(left);

	return {nullptr, nullptr};
}

StructInstance* try_get_implicit_ctor_for(
	StructInstance *struct_,
	Type const &arg_type,
	Expr *NULLABLE arg_expr,
	optional<LazyErrorMsg> error_msg,
	Subst *subst,
	SemaContext const &ctx
)
{
	StructInstance *implicit_case = struct_->implicit_case();
	if(not implicit_case)
		return nullptr;

	ProcType const &implicit_ctor = std::get<ProcType>(*implicit_case->try_get_ctor_type());
	Type const &implicit_param_type = implicit_ctor.inst->params->items[0];

	try
	{
		Unifier(ctx)
			.left(implicit_param_type, TypeConversion::NONE)
			.right(arg_type, TypeConversion::IMPLICIT_CTOR, arg_expr)
			.set(subst)
			.set(error_msg)
			.go();

		return implicit_case;
	}
	catch(ParseError const&) {
		return nullptr;
	}
}

bool Unifier::try_unify_structs()
{
	Type const &left = std::get<Type>(*m_left);
	Type const &right = std::get<Type>(*m_right);

	StructInstance *left_struct = get_if_struct(&left);
	StructInstance *right_struct = get_if_struct(&right);
	if(not left_struct)
	{
		if(not right_struct)
			return false;

		return sides_swapped().try_unify_structs();
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
			not try_convert_struct_to_parent(left_struct, common_parent_left, m_conv_left)
			or not try_convert_struct_to_parent(right_struct, common_parent_right, m_conv_right)
		) {
			throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);
		}

		unify_structs_eq(common_parent_left, common_parent_right, m_err, m_subst, m_ctx);

		if(m_result)
			*m_result = StructType(UNKNOWN_TOKEN_RANGE, common_parent_left);

		if(left_struct != common_parent_left)
			emit_conversion(TypeConversion::TRIVIAL, m_expr_left);

		if(right_struct != common_parent_right)
			emit_conversion(TypeConversion::TRIVIAL, m_expr_right);
	}
	else if(UnionInstance *right_union = get_if_union(&right))
	{
		StructInstance *implicit_case_left = nullptr;
		Type const *matching_alt_right = nullptr;

		if(m_conv_right == TypeConversion::IMPLICIT_CTOR)
			implicit_case_left = try_get_implicit_ctor_for(left_struct, right, m_expr_right, m_err, m_subst, m_ctx);

		if(m_conv_left == TypeConversion::IMPLICIT_CTOR)
			matching_alt_right = try_find_matching_alt_for(right_union, left, m_expr_left, m_err, m_subst, m_ctx);

		if(not implicit_case_left and not matching_alt_right)
			throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

		// This shouldn't be possible at the moment because it would imply left_struct and
		// right_union are defined recursively. However, this could change once user-defined
		// constructors are supported.
		if(implicit_case_left and matching_alt_right)
		{
			string reason = "Ambiguous common type for " + str(*m_left, *m_ctx.mod) + " and " + str(*m_right, *m_ctx.mod);
			if(m_err)
				m_err->throw_error(reason, *m_ctx.mod);
			else
				throw ParseError(reason);
		}

		if(implicit_case_left)
		{
			if(m_result)
				*m_result = *m_left;

			emit_conversion(ConstructorConversion(implicit_case_left), m_expr_right);
		}

		if(matching_alt_right)
		{
			if(m_result)
				*m_result = *m_right;

			emit_conversion(UnionConversion(right_union, *matching_alt_right), m_expr_left);
		}
	}
	else
	{
		// No common parent, so the only way forward is using an implicit ctor

		StructInstance *implicit_case_right = nullptr;
		StructInstance *implicit_case_left = nullptr;

		if(m_conv_right == TypeConversion::IMPLICIT_CTOR)
			implicit_case_left = try_get_implicit_ctor_for(left_struct, right, m_expr_right, m_err, m_subst, m_ctx);

		if(m_conv_left == TypeConversion::IMPLICIT_CTOR and right_struct)
			implicit_case_right = try_get_implicit_ctor_for(right_struct, left, m_expr_left, m_err, m_subst, m_ctx);

		if(implicit_case_left and implicit_case_right)
		{
			string reason = "Ambiguous common type for " + str(*m_left, *m_ctx.mod) + " and " + str(*m_right, *m_ctx.mod);
			if(m_err)
				m_err->throw_error(reason, *m_ctx.mod);
			else
				throw ParseError(reason);
		}

		if(not implicit_case_left and not implicit_case_right)
			throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

		if(implicit_case_left)
		{
			if(m_result)
				*m_result = *m_left;

			emit_conversion(ConstructorConversion(implicit_case_left), m_expr_right);
		}

		if(implicit_case_right)
		{
			if(m_result)
				*m_result = *m_right;

			emit_conversion(ConstructorConversion(implicit_case_right), m_expr_left);
		}
	}

	return true;
}


// Unification of union types
//--------------------------------------------------------------------
static Type const* try_find_matching_alt_for(
	UnionInstance *union_,
	Type const &type,
	Expr *NULLABLE expr,
	optional<LazyErrorMsg> error_msg,
	Subst *subst,
	SemaContext const &ctx
)
{
	Type const *matched_alt = nullptr;
	for(Type const *alt: union_->alternatives())
	{
		bool success = false;
		try
		{
			Unifier(ctx)
				.left(*alt, TypeConversion::NONE)
				.right(type, TypeConversion::TRIVIAL, expr)
				.set(subst)
				.set(error_msg)
				.go();

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

bool Unifier::try_unify_unions()
{
	Type const &left = std::get<Type>(*m_left);
	Type const &right = std::get<Type>(*m_right);

	UnionInstance *left_union = get_if_union(&left);
	UnionInstance *right_union = get_if_union(&right);
	if(not left_union)
	{
		if(not right_union)
			return false;

		sides_swapped().try_unify_unions();
	}
	assert(left_union);

	if(left_union and right_union)
	{
		// Unions cannot be nested, so if we have two unions then they can only be unified if they
		// are equal

		if(not equiv(*m_left, *m_right))
			throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

		if(m_result)
			*m_result = *m_left;
	}
	else
	{
		// If we get here we know `left` is a union type and `right` can be anything except a
		// union. The only way unification can succeed here is if `right` can be unified with one of
		// the alternatives of `left`. (Note that the case where `right` is a struct with an
		// implicit constructor for `left` has already been handled in `try_unify_structs()`).

		if(m_conv_right != TypeConversion::IMPLICIT_CTOR)
			throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

		Type const *matched_alt = try_find_matching_alt_for(left_union, right, m_expr_right, m_err, m_subst, m_ctx);
		if(not matched_alt)
			throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

		if(m_result)
			*m_result = *m_left;

		emit_conversion(UnionConversion(left_union, *matched_alt), m_expr_right);
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

optional<TypeConversion> can_convert_mutability(IsMutable from, IsMutable to, TypeConversion conv)
{
	switch(conv)
	{
		case TypeConversion::NONE:
			return from == to ? optional(TypeConversion::NONE) : nullopt;

		case TypeConversion::TRIVIAL:
		case TypeConversion::IMPLICIT_CTOR:
			return to == IsMutable::NO or from == IsMutable::YES ?
				optional(TypeConversion::TRIVIAL) : nullopt;
	}

	UNREACHABLE;
}

bool Unifier::try_unify_pointers()
{
	Type const &left = std::get<Type>(*m_left);
	Type const &right = std::get<Type>(*m_right);

	PointerType const *left_pointer = std::get_if<PointerType>(&left);
	PointerType const *right_pointer = std::get_if<PointerType>(&right);
	if(not left_pointer or not right_pointer)
		return false;

	if(left_pointer->kind != right_pointer->kind)
		throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

	IsMutable common_mutability = mutability_lub(left_pointer->mutability, right_pointer->mutability);
	optional<TypeConversion> act_conv_left = can_convert_mutability(left_pointer->mutability, common_mutability, m_conv_left);
	optional<TypeConversion> act_conv_right = can_convert_mutability(right_pointer->mutability, common_mutability, m_conv_right);
	if(not act_conv_left or not act_conv_right)
		throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

	GenericArg common_pointee;
	Unifier(*this)
		.left(
			*left_pointer->pointee,
			m_conv_left == TypeConversion::IMPLICIT_CTOR ? TypeConversion::TRIVIAL : m_conv_left,
			m_expr_left
		)
		.right(
			*right_pointer->pointee,
			m_conv_right == TypeConversion::IMPLICIT_CTOR ? TypeConversion::TRIVIAL : m_conv_right,
			m_expr_right
		)
		.result(&common_pointee)
		.go();

	if(m_result)
	{
		*m_result = PointerType{
			.range = UNKNOWN_TOKEN_RANGE,
			.pointee = clone_ptr(&common_pointee.as_type(), m_ctx.arena),
			.kind = left_pointer->kind,
			.mutability = common_mutability,
		};
	}

	if(*act_conv_left > TypeConversion::NONE)
		emit_conversion(*act_conv_left, m_expr_left);

	if(*act_conv_right > TypeConversion::NONE)
		emit_conversion(*act_conv_right, m_expr_right);

	return true;
}


// Unification of array types
//--------------------------------------------------------------------
bool Unifier::try_unify_arrays()
{
	Type const &left = std::get<Type>(*m_left);
	Type const &right = std::get<Type>(*m_right);

	ArrayType const *left_array = std::get_if<ArrayType>(&left);
	ArrayType const *right_array = std::get_if<ArrayType>(&right);
	if(not left_array or not right_array)
		return false;

	GenericArg count_result;
	Unifier(*this)
		.left(*left_array->count_arg, TypeConversion::NONE)
		.right(*right_array->count_arg, TypeConversion::NONE)
		.result(&count_result)
		.go();

	GenericArg element_type_result;
	Unifier(*this)
		.left(*left_array->element, TypeConversion::NONE)
		.right(*right_array->element, TypeConversion::NONE)
		.result(&element_type_result)
		.go();

	if(m_result)
	{
		*m_result = ArrayType{
			.range = UNKNOWN_TOKEN_RANGE,
			.element = clone_ptr(&element_type_result.as_type(), m_ctx.arena),
			.count_arg = clone_ptr(&count_result.as_expr(), m_ctx.arena),
		};
	}

	return true;
}


// Unification of array types
//--------------------------------------------------------------------
void Unifier::unify_generic_values()
{
	Expr const &left = std::get<Expr>(*m_left);
	Expr const &right = std::get<Expr>(*m_right);

	IntLiteralExpr const *left_int = std::get_if<IntLiteralExpr>(&left);
	IntLiteralExpr const *right_int = std::get_if<IntLiteralExpr>(&right);
	if(left_int and right_int)
	{
		if(left_int->value != right_int->value)
			throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

		if(m_result)
			*m_result = *m_left;
	}
	else
		throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);
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
void Unifier::go()
{
	// The order in which (1), (3), and (4) are executed is important because their conditions
	// overlap.

	// (1) Cases covered: is<GenericDeductionVar>(left) OR is<GenericDeductionVar>(right)
	if(try_unify_type_deduction_vars())
		return;

	if(is<Type>(*m_left) and is<Type>(*m_right))
	{
		// (2) Cases covered: is_integer_type(left) AND is_integer_type(right)
		if(try_unify_integer_types())
			return;

		// (3) Cases covered: is<StructType>(left) OR is<StructType>(right)
		if(try_unify_structs())
			return;

		// (4) Cases covered: is<UnionType>(left) OR is<UnionType>(right)
		if(try_unify_unions())
			return;

		// (5) Cases covered: is<PointerType>(left) AND is<PointerType>(right)
		if(try_unify_pointers())
			return;

		// (6) Cases covered: is<ArrayType>(left) AND is<ArrayType>(right)
		if(try_unify_arrays())
			return;

		Type const &left = std::get<Type>(*m_left);
		Type const &right = std::get<Type>(*m_right);
		left | match
		{
			[&](BuiltinType const &left_t)
			{
				BuiltinType const *right_t = std::get_if<BuiltinType>(&right);
				if(not right_t)
					throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

				// The case where both left and right are integer types has already been handled above.
				// Non-integer builtin types (Never, Unit, Bool) can only be unified if they are equal.
				if(left_t.builtin != right_t->builtin)
					throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

				if(m_result) *m_result = *m_left;
			},
			[&](VarType const&)
			{
				// If we get here, we know left and right are GenericParameterVars (because we already
				// handled GenericDeductionVars above).
				//
				// Unification of GenericParameterVars only succeeds if both sides refer to the same
				// GenericParameterVar as they may be instantiated with any type.

				if(not equiv(*m_left, *m_right))
					throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);

				if(m_result) *m_result = *m_left;
			},
			[&](ProcType const&)
			{
				assert(!"[TODO] unify: ProcType");
			},

			// Handled above
			[&](KnownIntType const&) { throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod); },
			[&](PointerType const&) { throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod); },
			[&](ArrayType const&) { throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod); },
			[&](StructType const&) { throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod); },
			[&](UnionType const&) { throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod); },

			[&](ProcTypeUnresolved const&) { assert(!"unify: ProcTypeUnresolved"); },
			[&](UnionTypeUnresolved const&) { assert(!"unify: UnionTypeUnresolved"); },
			[&](Path const&) { assert(!"unify: Path"); },
			[&](InlineStructType const&) { assert(!"unify: InlineStructType"); },
		};
	}
	else if(is<Expr>(*m_left) and is<Expr>(*m_right))
		unify_generic_values();
	else
		throw_unification_error(*m_left, *m_right, m_err, m_ctx.mod);
}


//--------------------------------------------------------------------
// Constraints
//--------------------------------------------------------------------
Type const* follow_type_var(Type const *type, TypeEnv const &env)
{
	if(optional<GenericDeductionVar> var = get_if_type_deduction_var(*type))
		return follow_type_var(&env.lookup(*var).as_type(), env);

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
			Type const *indexed_type = follow_type_var(type_of(*e.addr), env);
			return *indexed_type | match
			{
				[&](ArrayType const&) -> optional<IsMutable>
				{
					return is_lvalue_expr(*e.addr, env);
				},
				[&](PointerType const &ptr) -> optional<IsMutable>
				{
					assert(ptr.kind == PointerType::MANY);
					return ptr.mutability;
				},
				[](auto const&) -> optional<IsMutable> { assert(!"is_lvalue_expr: IndexExpr"); },
			};
		},
		[&](MemberAccessExpr const &e) -> optional<IsMutable>
		{
			return is_lvalue_expr(*e.object, env);
		},

		[](auto const&) -> optional<IsMutable> { return nullopt; },
	};
}

class TypeEnvReadonlySubst : public Subst
{
public:
	TypeEnvReadonlySubst(TypeEnv const &env, Arena &arena) :
		env(&env),
		arena(&arena) {}

	virtual GenericArg const* try_get(GenericDeductionVar var) override
	{
		return env->try_lookup(var);
	}

	virtual void apply_conversion(TypeConversionEvent const &event, Expr *expr) override
	{
		::apply_conversion(event, expr, *arena);
	}

	virtual void set(
		GenericDeductionVar var,
		TypeConversion var_conv,
		Expr *NULLABLE var_expr,
		GenericArg const &arg,
		TypeConversion arg_conv,
		Expr *NULLABLE arg_expr,
		optional<LazyErrorMsg> error_msg
	) override
	{
		(void)var;
		(void)var_conv;
		(void)var_expr;
		(void)arg;
		(void)arg_conv;
		(void)arg_expr;
		(void)error_msg;

		assert(!"TypeEnvReadonlySubst::set()");
	}

	TypeEnv const *env;
	Arena *arena;
};

static bool is_cast_ok(Type const &target_type, Expr &src_expr, TypeEnv const &env, SemaContext &ctx)
{
	Type *src_type = type_of(src_expr);
	try
	{
		TypeEnvReadonlySubst subst(env, ctx.arena);
		Unifier(ctx)
			.left(target_type, TypeConversion::NONE)
			.right(*src_type, TypeConversion::IMPLICIT_CTOR, &src_expr)
			.set(&subst)
			.go();

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
			if(GenericArg const *arg = env.try_lookup(src_t.var))
				return is_cast_ok(arg->as_type(), src_expr, env, ctx);

			return false;
		},
		[&](PointerType const &src_t) -> bool
		{
			if(PointerType const *target_t = std::get_if<PointerType>(&target_type))
				return src_t.mutability == IsMutable::YES || target_t->mutability == IsMutable::NO;

			return false;
		},
		[&](ArrayType const&) -> bool
		{
			assert(!"[TODO] is_cast_ok: ArrayType");
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

void do_check(TypeCheck const &check, TypeEnv const &env, SemaContext &ctx)
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

void ConstraintSystem::add_check(TypeCheck const &check)
{
	checks.push_back(check);
}

bool ConstraintSystem::add_relational_constraint(
	GenericDeductionVar var,
	ConstraintEdge const &edge
)
{
	bool changed = false;
	if(get_node(var).add_edge(edge))
		changed = true;

	if(
		optional<GenericDeductionVar> other_var = get_if_deduction_var(edge.arg);
		other_var and is<NoModifier>(edge.arg_modifier)
	)
	{
		changed |= get_node(*other_var).add_edge(ConstraintEdge{
			.var_conv = edge.arg_conv,
			.var_expr = edge.arg_expr,
			.arg = mk_generic_arg(var),
			.arg_conv = edge.var_conv,
			.arg_expr = edge.var_expr,
			.arg_modifier = NoModifier(),
			.error_msg = edge.error_msg,
		});
	}

	return changed;
}

std::expected<GenericArg, ErrorMsg> follow_arg(GenericArg const &arg, ConstraintModifier const &modifier, TypeEnv const *env, SemaContext *ctx);

string edge_to_str(ConstraintEdge const &edge, Module const &mod)
{
	std::stringstream ss;
	ss << " " << type_conv_symbol(edge.var_conv) << ":" << type_conv_symbol(edge.arg_conv) << " ";
	::print(edge.arg, mod, ss);
	edge.arg_modifier | match
	{
		[&](NoModifier) {},
		[&](MemberTypeModifier m)
		{
			ss << " /member: " << m.member;
		},
		[&](PointeeTypeModifier m)
		{
			switch(m.pointer_kind)
			{
				case PointerType::SINGLE: ss << " /single_deref"; break;
				case PointerType::MANY: ss << " /many_deref"; break;
			}
		},
	};
	return std::move(ss).str();
}

void ConstraintSystem::print(std::ostream &os) const
{
	for(auto const &[var, node]: nodes)
	{
		::print(mk_generic_arg(var), mod, os);
		os << std::endl;

		TTable table;
		for(ConstraintEdge const &edge: node.push_edges)
			table.add_row({edge_to_str(edge, mod)});

		for(ConstraintEdge const &edge: node.pull_edges)
			table.add_row({edge_to_str(edge, mod)});

		table.print(os);
	}

	if(checks.size())
	{
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

class NewConstraintGatheringSubst : public Subst
{
public:
	virtual GenericArg const* try_get(GenericDeductionVar var) override
	{
		// Q: Why not look up the current type from TypeEnv?
		// A: Because TypeEnv contains in-progress types which may differ from the final type.
		//
		//      a <<- i8
		//      b <<- Option'(a)
		//      b <<- Option'(i32)
		//
		//    In the above example, we may at some point have `env[a] == i8`. If we then try to
		//    unify `Option'(a)` and `Option'(i32)` we get an error because `i8 != i32`, even though
		//    `env[a] == i32` would be a valid solution.

		// A possible solution to the above problem would be to store for each variable the allowed
		// TypeConversion. In the above example we would have `env[a] = (i8, IMPLICIT_CTOR)`. Then
		// the unification of `Option'(a)` and `Option'(i32)` would succeed because by using a
		// `IMPLICIT_CTOR` conversion we can make `a` equal to `i32`.
		//
		// The problem with this solution is that the unifier doesn't tell us about the conversion.
		// (The conversion happens when unifying the type args of `Option'(a)` and `Option'(i32)` so
		// no result type is produced.)
		//
		// This in turn can be solved by splitting the constraint solving algorithm into two steps:
		// 1. Constraint generation: Run the existing algorithm, but do not update the TypeEnv,
		//    making sure that the unifier never sees intermediate results. This ensures that all
		//    implicit constraints are made explicit. (For the example above, we would get an
		//    additional constraint `a -- i32` when unifying `Option'(a)` and `Option'(i32)`.)
		// 2. Constraint solving: Run the algorithm again, but this time update the TypeEnv and pass
		//    it to the unifier.
		//
		// Though now I'm back at the problem I had at the beginning: During this process,
		// additional pull constraints may be generated for nodes that do no have any push
		// constraints. Currently, when there are no push constriants, all pull constraints must be
		// equal. This is a stronger requirement than push constraints have, which leads to
		// situations where these additionally generated constraints lead to a constraint violation.

		(void)var;
		return nullptr;
	}

	virtual void apply_conversion(TypeConversionEvent const &event, Expr *expr) override
	{
		if(not conversions)
			return;

		if(expr == left_expr)
		{
			conversions->insert({expr, {}});
			for(auto &[_, expr_events]: *conversions)
				expr_events.push_back(event);
		}

		if(expr == right_expr)
			(*conversions)[expr].push_back(event);
	}

	virtual void set(
		GenericDeductionVar var,
		TypeConversion var_conv,
		Expr *NULLABLE var_expr,
		GenericArg const &arg,
		TypeConversion arg_conv,
		Expr *NULLABLE arg_expr,
		optional<LazyErrorMsg> error_msg
	) override
	{
		new_nodes.push_back({
			var,
			ConstraintEdge{
				.var_conv = var_conv,
				.var_expr = var_expr,
				.arg = arg,
				.arg_conv = arg_conv,
				.arg_expr = arg_expr,
				.arg_modifier = NoModifier(),
				.error_msg = error_msg,
			}
		});
	}

	vector<pair<GenericDeductionVar, ConstraintEdge>> new_nodes;
	unordered_map<Expr*, vector<TypeConversionEvent>> *conversions = nullptr;
	Expr *NULLABLE left_expr;
	Expr *NULLABLE right_expr;
};

struct ReductionResult
{
	GenericArg arg;
	TypeConversion conv;
};

struct ReductionState
{
	void set_error(ErrorMsg const &err)
	{
		error = err;
		all_edges_merged = false;
	}

	optional<ReductionResult> result = nullopt;
	unordered_map<Expr*, vector<TypeConversionEvent>> conversions;
	optional<ErrorMsg> error;
	bool all_edges_merged = true;
};

enum class ReductionBehavior
{
	MERGE,
	MERGE_EQUAL,
	NO_MERGE,
};

// "Reduction" refers to the process of calculating a concrete type for a GenericDeductionVar by,
// essentially, unifying all the types that are connected to the GenericDeductionVar.
//
// As an example, assume we have constraints C1, C2 and C3 that constrain `var` to the types A, B
// and C, respectively. In order to compute `result` we essentially do the following:
//
//     (1) result = unify(C1, C2)      // result contains the common type of A and B
//     (2) result = unify(result, C3)  // result contains the common type of A, B and C
//
// where (1) and (2) represent individual reduction steps.
//
// Care must be taken to ensure that type conversions are always correctly inserted. As an example,
// assume the following relationships: A <c B <c C, where A <c B means that A can be converted to B,
// but we do need to insert the corresponding conversion function.
//
// Now, when we do step (1) above, the unification needs to insert a conversion for the expression
// associated with C1 from A to B. In step (2), we need to insert conversions for both C1 and C2
// from B to A. In other words, we need to apply conversions to all previously visited constraints.
//
// See for example tests/common_type_struct_2
void reduction_step(
	ReductionState *state,
	ReductionBehavior behavior,
	ConstraintEdge const &edge,
	NewConstraintGatheringSubst *subst,
	TypeEnv const *env,
	SemaContext *ctx
)
{
	std::expected<GenericArg, ErrorMsg> edge_type = follow_arg(edge.arg, edge.arg_modifier, env, ctx);
	if(not edge_type)
		return state->set_error(edge_type.error());

	// If the edge type is a GenericDeductionVar do not merge (i.e. unify) it into `state` as it does
	// not contain any useful information.
	// This may sound like it's merely an optimization but it's also needed for correctness:
	//
	//   b ->> a ->> i8
	//
	// If we did allow GenericDeductionVar to be merged into `state`, then `b ->> a` would constitute a
	// valid push constraint for `a`. This in turn would mean that the pull constraint `a ->> i8`
	// would not update `a` (push takes precedence over pull), hence `env[a]` would never be set.
	if(get_if_deduction_var(*edge_type))
	{
		// If edge_type becomes constrained later on then this error message will be cleared
		return state->set_error(ErrorMsg(mk_error_msg(
			"Insufficiently constrained type "s + str(*edge_type, *ctx->mod), token_range_of(*edge_type).first, ctx->mod
		)));
	}

	TypeConversion edge_conv = edge.arg_conv;
	if(behavior == ReductionBehavior::MERGE_EQUAL)
		edge_conv = TypeConversion::NONE;

	Expr *left_expr = edge.var_expr;
	if(left_expr)
		state->conversions.insert({left_expr, {}});
	else if(state->conversions.size())
		left_expr = state->conversions.begin()->first;

	if(state->result)
	{
		if(behavior == ReductionBehavior::MERGE_EQUAL)
			state->result->conv = TypeConversion::NONE;

		subst->conversions = &state->conversions;
		subst->left_expr = left_expr;
		subst->right_expr = edge.arg_expr;

		GenericArg result;
		Unifier u(*ctx);
		u.left(state->result->arg, state->result->conv, left_expr);
		u.right(*edge_type, edge_conv, edge.arg_expr);
		u.set(subst);
		u.set(edge.error_msg);
		u.result(&result);

		try {
			u.go();

			if(behavior != ReductionBehavior::NO_MERGE)
			{
				state->result->arg = result;
				state->result->conv = glb(state->result->conv, edge.arg_conv);

				if(edge.arg_expr)
					state->conversions.insert({edge.arg_expr, {}});
			}
		}
		catch(ParseError const &exc) {
			state->set_error(ErrorMsg(exc.what()));
		}

		subst->conversions = nullptr;
	}
	else
	{
		if(behavior != ReductionBehavior::NO_MERGE)
		{
			state->result = ReductionResult(*edge_type, edge_conv);
			if(edge.arg_expr)
				state->conversions.insert({edge.arg_expr, {}});
		}
	}
}

static bool is_known_int(GenericArg const &arg)
{
	if(Type const *t = std::get_if<Type>(&arg))
		return is<KnownIntType>(*t);

	return false;
}

void reduce(
	ConstraintSystem *constraints,
	TypeEnv *NULLABLE env,
	unordered_map<Expr*, vector<TypeConversionEvent>> *NULLABLE conversions,
	SemaContext *ctx
)
{
	// int iteration_counter = 0;
	bool changed = true;
	while(changed)
	{
		changed = false;
		if(conversions) conversions->clear();
		for(auto &[var, node]: constraints->nodes)
		{
			NewConstraintGatheringSubst subst;
			ReductionState red_state;

			// Reduce all push edges to a single type (stored in red_state). This will be the type
			// of the node.
			for(ConstraintEdge const &edge: node.push_edges)
				reduction_step(&red_state, ReductionBehavior::MERGE, edge, &subst, env, ctx);

			// How pull edges are handled depends on whether there are any push edges.
			// - If the node has at least one push edge, then it fully determines the node's type.
			//   In this case, we only check whether the node can be unified with the targets of
			//   each pull edge, but do not actually update the node's type while doing so
			//   (ReductionBehavior::NO_MERGE).
			// - If the node does not have any push edges, then we use the pull edges to determine
			//   the node's type. Because the node needs to be assignable to all pull edges, we
			//   require the pull edges to all be unifiable (ReductionBehavior::NO_MERGE). An
			//   alternative would be to calculate their intersection, but I prefer to keep things
			//   simple for now and I don't even know if it would be desirable.
			//
			// If the node *does* have push edges but `red_state` is still empty we use
			// TRY_MERGE_EQUAL. Consider the following:
			//
			//   b ->> a ->> i8
			//
			// `a` has only one push edge, but because `b` is otherwise unconstrained, `red_state`
			// will still be empty when we get here. So we use TRY_MERGE_EQUAL to merge i8 into `a`
			// which is then backpropagated to `b`. TRY_MERGE_EQUAL means that unification failures
			// are ignored. This is because MERGE_EQUAL is stricter than what is required once we
			// get a type via one of the pull edges.
			//
			//   a ->> i32
			//   a ->> i8
			//   a <<- b
			//   b <<- i8
			//
			// Here, MERGE_EQUAL fails for `a ->> i32` and `a ->> i8`, but once `a` receives `i8`
			// via its push edge from `b` it all works out.
			ReductionBehavior pull_reduction_behavior = ReductionBehavior::NO_MERGE;
			if(
				node.push_edges.empty() or
				not red_state.result or
				is_known_int(red_state.result->arg)
			) {
				pull_reduction_behavior = ReductionBehavior::MERGE_EQUAL;
			}

			for(ConstraintEdge const &edge: node.pull_edges)
				reduction_step(&red_state, pull_reduction_behavior, edge, &subst, env, ctx);

			// Add all constraints that where generated during the previous reduction steps to the
			// cosntraint system.
			for(auto const &new_node: subst.new_nodes)
				changed |= constraints->add_relational_constraint(new_node.first, new_node.second);

			bool result_is_concrete = false;
			if(red_state.result)
			{
				result_is_concrete = not get_type_stats(red_state.result->arg).has_type_deduction_vars;
				if(env and not type_var_occurs_in(var, red_state.result->arg))
					changed |= env->update(var, red_state.result->arg);
			}

			if(red_state.error)
				node.error = red_state.error;
			else if(not result_is_concrete)
			{
				// If edge_type becomes constrained later on then this error message will be cleared
				if(red_state.result)
				{
					GenericArg const *arg = &red_state.result->arg;
					node.error = ErrorMsg(mk_error_msg(
						"Insufficiently constrained type "s + str(*arg, *ctx->mod), token_range_of(*arg).first, ctx->mod
					));
				}
				else
					node.error = ErrorMsg("Insufficiently constrained type");
			}
			else
				node.error = nullopt;

			if(conversions)
				conversions->insert_range(red_state.conversions);
		}

		// std::cout << "Iteration " << (iteration_counter+1) << std::endl;
		// constraints->print(std::cout);
		// if(env) env->print(std::cout, *ctx->mod);
		// iteration_counter += 1;
	}
}

TypeEnv create_subst_from_constraints(ConstraintSystem &constraints, SemaContext &ctx)
{
	// std::cout << std::endl;
	// constraints.print(std::cout);

	// Constraint solving is separated into two steps:
	//
	// 1. Constraint generation: Unify the edges of a node without updating the TypeEnv.
	// 2. Constraint solving: Unify the edges of a node *and* update the TypeEnv.
	//
	// During constraint generation we want to discover implicit constraints:
	//
	//      a <<- i8
	//      b <<- Option'(a)
	//      b <<- Option'(i32)
	//
	// Here, the implicit constraint `a -- i32` will be made explicit during the first step.
	// If we were also updating the TypeEnv during this step, then after visiting `a <<- i8` we
	// would have `env[a] == i8`. However, this would mean that the unification of `Option'(a)` and
	// `Option'(i32)` fails. This is solved by simply not updating the TypeEnv.

	TypeEnv env;
	unordered_map<Expr*, vector<TypeConversionEvent>> conversions;
	reduce(&constraints, nullptr, nullptr, &ctx);
	reduce(&constraints, &env, &conversions, &ctx);

	for(auto &[var, node]: constraints.nodes)
	{
		if(node.error)
			throw ParseError(node.error->msg);
	}

	env.materialize(ctx.mod->sema->insts);

	for(auto &[expr, events]: conversions)
	{
		for(TypeConversionEvent const &event: events)
			apply_conversion(event, expr, ctx.arena);
	}

	for(TypeCheck const &check: constraints.checks)
		do_check(check, env, ctx);

	return env;
}


static PointerType const* get_if_pointer_type(Type const *type, PointerType::Kind kind)
{
	PointerType const *pointer_type = std::get_if<PointerType>(type);
	if(pointer_type and pointer_type->kind == kind)
		return pointer_type;

	return nullptr;
}

static std::expected<Type, ErrorMsg> get_pointee_type(
	Type const *type,
	PointerType::Kind pointer_kind,
	TokenRange range,
	Module const &mod
)
{
	if(PointerType const *pointer_type = get_if_pointer_type(type, pointer_kind))
		return *pointer_type->pointee;
	else
	{
		char const *pointer_kind_str = nullptr;
		switch(pointer_kind)
		{
			case PointerType::SINGLE: pointer_kind_str = "single-element"; break;
			case PointerType::MANY: pointer_kind_str = "multi-element"; break;
		}

		return std::unexpected(
			ErrorMsg(mk_error_msg("Expected "s + pointer_kind_str + " pointer type, got " + str(*type, mod), range.first, &mod))
		);
	}
}

static std::expected<Type, ErrorMsg> get_indexed_type(
	Type const *type,
	TokenRange range,
	Module const &mod
)
{
	if(PointerType const *pointer_type = get_if_pointer_type(type, PointerType::MANY))
		return *pointer_type->pointee;
	else if(ArrayType const *array_type = std::get_if<ArrayType>(type))
		return *array_type->element;
	else
	{
		return std::unexpected(
			ErrorMsg(mk_error_msg("Expected indexable type, got " + str(*type, mod), range.first, &mod))
		);
	}
}


Parameter const* find_var_member(StructInstance *inst, string_view field)
{
	Module *mod = inst->struct_()->sema->type_scope->mod; // This looks disgusting

	for(Parameter const &var_member: inst->own_var_members())
	{
		if(name_of(var_member, mod) == field)
			return &var_member;
	}

	if(inst->variant_parent())
		return find_var_member(inst->variant_parent(), field);

	return nullptr;
}

static std::expected<Type, ErrorMsg> get_member_type(
	Type const *type,
	string_view member,
	TokenRange range,
	Module const &mod
)
{
	(void)range;
	(void)mod;

	StructType const *struct_type = std::get_if<StructType>(type);
	if(not struct_type)
	{
		return std::unexpected(
			ErrorMsg(mk_error_msg("Expected object of struct type for member access, got " + str(*type, mod), range.first, &mod))
		);
	}

	Parameter const *var_member = find_var_member(struct_type->inst, member);
	if(not var_member)
	{
		return std::unexpected(
			ErrorMsg(mk_error_msg("`"s + struct_type->inst->struct_()->name + "` has no field named `"s + member + "`", range.first, &mod))
		);
	}

	return *var_member->type;
}


std::expected<GenericArg, ErrorMsg> follow_arg(
	GenericArg const &arg,
	ConstraintModifier const &modifier,
	TypeEnv const *env,
	SemaContext *ctx
)
{
	std::expected<GenericArg, ErrorMsg> result = modifier | match
	{
		[&](NoModifier) -> std::expected<GenericArg, ErrorMsg>
		{
			return arg;
		},
		[&](PointeeTypeModifier m) -> std::expected<GenericArg, ErrorMsg>
		{
			GenericArg const *pointer_type = &arg;
			if(optional<GenericDeductionVar> var = get_if_deduction_var(arg))
				pointer_type = env ? env->try_lookup(*var) : nullptr;

			if(not pointer_type)
			{
				return std::unexpected(ErrorMsg(mk_error_msg(
					"Insufficiently constrained type "s + str(arg, *ctx->mod), token_range_of(arg).first, ctx->mod
				)));
			}

			return get_pointee_type(
				&pointer_type->as_type(),
				m.pointer_kind,
				token_range_of(*pointer_type),
				*ctx->mod
			);
		},
		[&](MemberTypeModifier m) -> std::expected<GenericArg, ErrorMsg>
		{
			GenericArg const *struct_type = &arg;
			if(optional<GenericDeductionVar> var = get_if_deduction_var(arg))
				struct_type = env ? env->try_lookup(*var) : nullptr;

			if(not struct_type)
			{
				return std::unexpected(ErrorMsg(mk_error_msg(
					"Insufficiently constrained type "s + str(arg, *ctx->mod), token_range_of(arg).first, ctx->mod
				)));
			}

			return get_member_type(
				&struct_type->as_type(),
				m.member,
				token_range_of(*struct_type),
				*ctx->mod
			);
		},
	};

	if(result and env)
		substitute(*result, *env, ctx->mod->sema->insts, BestEffortSubstitution());

	return result;
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
		[&](GenericVarExpr const&) {},
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
class ConstraintGatheringSubst : public Subst
{
public:
	ConstraintGatheringSubst(ConstraintSystem *constraints, SemaContext *ctx) :
		constraints(constraints),
		ctx(ctx) {}

	virtual GenericArg const * try_get(GenericDeductionVar var) override
	{
		(void)var;
		return nullptr;
	}

	virtual void apply_conversion(TypeConversionEvent const &event, Expr *expr) override
	{
		::apply_conversion(event, expr, ctx->arena);
	}

	virtual void set(
		GenericDeductionVar var,
		TypeConversion var_conv,
		Expr *NULLABLE var_expr,
		GenericArg const &arg,
		TypeConversion arg_conv,
		Expr *NULLABLE arg_expr,
		optional<LazyErrorMsg> error_msg
	) override
	{
		constraints->add_relational_constraint(
			var,
			ConstraintEdge{
				.var_conv = var_conv,
				.var_expr = var_expr,
				.arg = arg,
				.arg_conv = arg_conv,
				.arg_expr = arg_expr,
				.arg_modifier = NoModifier(),
				.error_msg = error_msg,
			}
		);
	}

	ConstraintSystem *constraints;
	SemaContext *ctx;
};

static void typecheck_type(Type &type, ConstraintGatheringSubst &subst, SemaContext &ctx)
{
	type | match
	{
		[&](this auto &self, ArrayType &t)
		{
			Type *count_type = typecheck_subexpr(*t.count_arg, subst, ctx);
			auto error_msg = [](Expr const &ret_expr, string const &reason, Module const &mod)
			{
				throw_sem_error("Invalid array count: " + reason, token_range_of(ret_expr).first, &mod);
			};
			Unifier(ctx)
				.left(*count_type, TypeConversion::NONE)
				.right(BuiltinType(UNKNOWN_TOKEN_RANGE, BuiltinTypeDef::USIZE), TypeConversion::NONE)
				.set(&subst)
				.set(LazyErrorMsg(t.count_arg, error_msg))
				.go();

			visit_child_types(t, self);
		},
		[&](StructType &t)
		{
			t.inst->typecheck_generic_args(subst, ctx);
		},
		[&](ProcType &t)
		{
			t.inst->typecheck(subst, ctx);
		},
		[&](UnionType &t)
		{
			t.inst->typecheck(subst, ctx);
		},
		[&](this auto &self, auto &t) { visit_child_types(t, self); },
	};
}

void typecheck_generic_args(
	FixedArray<GenericArg> *args,
	FixedArray<GenericParameter> const *params,
	ConstraintGatheringSubst &subst,
	SemaContext &ctx
)
{
	for(auto const &[idx, arg]: *args | std::views::enumerate)
	{
		GenericParameter const &generic_param = params->items[idx];
		arg | match
		{
			[&](Type &type)
			{
				typecheck_type(type, subst, ctx);
			},
			[&](Expr &expr)
			{
				GenericValueParameter const *gen_val = std::get_if<GenericValueParameter>(&generic_param.kind);
				if(not gen_val)
					throw_sem_error("Expected value, got type", token_range_of(expr).first, ctx.mod);

				Type *type = typecheck_subexpr(expr, subst, ctx);

				auto error_msg = [](Expr const &ret_expr, string const &reason, Module const &mod)
				{
					throw_sem_error("Invalid generic value: " + reason, token_range_of(ret_expr).first, &mod);
				};
				Unifier(ctx)
					.left(*type, TypeConversion::NONE)
					.right(*gen_val->type, TypeConversion::NONE)
					.set(&subst)
					.set(LazyErrorMsg(&expr, error_msg))
					.go();
			},
		};
	}
}

void StructInstance::typecheck_generic_args(ConstraintGatheringSubst &subst, SemaContext &ctx)
{
	if(m_generic_args_typechecked)
		return;

	::typecheck_generic_args(m_type_args.args, m_struct->type_params, subst, ctx);
	m_generic_args_typechecked = true;
}

void ProcInstance::typecheck_generic_args(ConstraintGatheringSubst &subst, SemaContext &ctx)
{
	if(m_generic_args_typechecked)
		return;

	::typecheck_generic_args(m_type_args.args, m_proc->type_params, subst, ctx);
	m_generic_args_typechecked = true;
}


void ProcTypeInstance::typecheck(ConstraintGatheringSubst &subst, SemaContext &ctx)
{
	if(has_been_typechecked)
		return;

	for(Type &param: *params)
		typecheck_type(param, subst, ctx);

	typecheck_type(*ret, subst, ctx);

	has_been_typechecked = true;
}

void UnionInstance::typecheck(ConstraintGatheringSubst &subst, SemaContext &ctx)
{
	if(m_has_been_typechecked)
		return;

	for(Type *alt: m_alternatives)
		typecheck_type(*alt, subst, ctx);

	m_has_been_typechecked = true;
}

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

static Type* typecheck_subexpr(Expr &expr, ConstraintGatheringSubst &subst, SemaContext &ctx)
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
			StructInstance *c_char = ctx.mod->sema->insts.get_struct_instance(std::get<StructItem*>(c_char_item), nullptr, nullopt);

			return e.type = ctx.arena.alloc<Type>(PointerType{
				.range = UNKNOWN_TOKEN_RANGE,
				.pointee = ctx.arena.alloc<Type>(StructType(UNKNOWN_TOKEN_RANGE, c_char)),
				.kind = PointerType::MANY,
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
					Type const *sub_type = typecheck_subexpr(*e.sub, subst, ctx);

					auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Invalid operand for not operator: " + reason, token_range_of(expr).first, &mod);
					};
					Unifier(ctx)
						.left(*e.type, TypeConversion::NONE)
						.right(*sub_type, TypeConversion::NONE)
						.set(&subst)
						.set(LazyErrorMsg(e.sub, error_msg))
						.go();
				} break;

				case UnaryOp::NEG:
				{
					// Example where type_of(e.sub) is a GenericDeductionVar that is not mapped to any
					// concrete type:
					//
					//     proc foo'S() -> S {}
					//     proc bar'T(a: T, b: T) {}
					//
					//     bar(-foo(), 1)

					Type const *sub_type = typecheck_subexpr(*e.sub, subst, ctx);
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
						subst.constraints->add_check(IntegerCheck(LazyErrorMsg(e.sub, error_msg), e.type));
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
					Type const *left_type = typecheck_subexpr(*e.left, subst, ctx);
					Type const *right_type = typecheck_subexpr(*e.right, subst, ctx);

					auto uni_error_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Invalid operands for binary operator: " + reason, token_range_of(expr).first, &mod);
					};
					GenericArg result;
					Unifier(ctx)
						.left(*left_type, TypeConversion::IMPLICIT_CTOR, e.left)
						.right(*right_type, TypeConversion::IMPLICIT_CTOR, e.right)
						.set(&subst)
						.set(LazyErrorMsg(&expr, uni_error_msg))
						.result(&result)
						.go();

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
						// Example where type_of(e.left) and type_of(e.right) are GenericDeductionVars
						// that are not mapped to any concrete type:
						//
						//     proc foo'S() -> S {}
						//     proc bar'T(a: T, b: T) {}
						//
						//     bar(foo() + foo(), 1)
						//
						//
						// Example where type_of(e.left) and type_of(e.right) change:
						//
						//     proc foo'S() -> S {}
						//     proc bar'T(a: T, b: T, c: T) {}
						//
						//     bar(1, foo() + foo(), 4294967295)

						auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
						{
							throw_sem_error("Invalid type for binary operator: " + reason, token_range_of(expr).first, &mod);
						};
						subst.constraints->add_check(IntegerCheck(LazyErrorMsg(e.left, error_msg), left_type));
						subst.constraints->add_check(IntegerCheck(LazyErrorMsg(e.right, error_msg), right_type));

						e.type = clone_ptr(&result.as_type(), ctx.arena);
					}
				} break;

				case BinaryOp::EQ:
				{
					Type const *left_type = typecheck_subexpr(*e.left, subst, ctx);
					Type const *right_type = typecheck_subexpr(*e.right, subst, ctx);

					auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Equality operator requires equal types: " + reason, token_range_of(expr).first, &mod);
					};
					Unifier(ctx)
						.left(*left_type, TypeConversion::NONE)
						.right(*right_type, TypeConversion::NONE)
						.set(&subst)
						.set(LazyErrorMsg(&expr, error_msg))
						.go();

					e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
				} break;

				case BinaryOp::LT:
				case BinaryOp::LE:
				case BinaryOp::GT:
				case BinaryOp::GE:
				{
					Type const *left_type = typecheck_subexpr(*e.left, subst, ctx);
					Type const *right_type = typecheck_subexpr(*e.right, subst, ctx);

					auto uni_error_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Invalid operands for comparison operator: " + reason, token_range_of(expr).first, &mod);
					};
					Unifier(ctx)
						.left(*left_type, TypeConversion::NONE)
						.right(*right_type, TypeConversion::NONE)
						.set(&subst)
						.set(LazyErrorMsg(&expr, uni_error_msg))
						.go();


					auto integer_check_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Invalid operand for comparison operator: " + reason, token_range_of(expr).first, &mod);
					};

					subst.constraints->add_check(IntegerCheck(LazyErrorMsg(e.left, integer_check_msg), left_type));
					subst.constraints->add_check(IntegerCheck(LazyErrorMsg(e.right, integer_check_msg), right_type));

					e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
				} break;
			}

			return e.type;
		},
		[&](AddressOfExpr &e) -> Type*
		{
			Type const *object_type = typecheck_subexpr(*e.object, subst, ctx);
			subst.constraints->add_check(LValueCheck(e.object, e.mutability));

			return e.type = mk_pointer_type(clone_ptr(object_type, ctx.arena), e.mutability, ctx.arena);
		},
		[&](DerefExpr &e) -> Type*
		{
			Type const *addr_type = typecheck_subexpr(*e.addr, subst, ctx);

			if(optional<GenericDeductionVar> pointer_var = get_if_type_deduction_var(*addr_type))
			{
				GenericDeductionVar var = ctx.new_deduction_var(TypeDeductionVar());
				subst.constraints->add_relational_constraint(var, ConstraintEdge{
					.arg = mk_generic_arg(*pointer_var),
					.arg_modifier = PointeeTypeModifier(PointerType::SINGLE),
				});
				e.type = ctx.arena.alloc<Type>(VarType(UNKNOWN_TOKEN_RANGE, var));
			}
			else
			{
				std::expected<Type, ErrorMsg> pointee = get_pointee_type(addr_type, PointerType::SINGLE, e.range, *ctx.mod);
				if(not pointee)
					throw ParseError(pointee.error().msg);

				e.type = clone_ptr(&pointee.value(), ctx.arena);
			}

			return e.type;
		},
		[&](IndexExpr &e) -> Type*
		{
			Type const *addr_type = typecheck_subexpr(*e.addr, subst, ctx);
			Type const *index_type = typecheck_subexpr(*e.index, subst, ctx);

			if(optional<GenericDeductionVar> array_var = get_if_type_deduction_var(*addr_type))
			{
				GenericDeductionVar var = ctx.new_deduction_var(TypeDeductionVar());
				subst.constraints->add_relational_constraint(var, ConstraintEdge{
					.arg = mk_generic_arg(*array_var),
					.arg_modifier = PointeeTypeModifier(PointerType::MANY),
				});
				e.type = ctx.arena.alloc<Type>(VarType(UNKNOWN_TOKEN_RANGE, var));
			}
			else
			{
				std::expected<Type, ErrorMsg> pointee = get_indexed_type(addr_type, e.range, *ctx.mod);
				if(not pointee)
					throw ParseError(pointee.error().msg);

				e.type = clone_ptr(&pointee.value(), ctx.arena);
			}

			auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
			{
				throw_sem_error("Invalid operand for index operator: " + reason, token_range_of(expr).first, &mod);
			};

			subst.constraints->add_check(IntegerCheck(LazyErrorMsg(e.index, error_msg), index_type));
			return e.type;
		},
		[&](MemberAccessExpr &e) -> Type*
		{
			// Example where type_of(e.object) is a GenericDeductionVar:
			//
			//     struct Foo { i: i32 }
			//     proc id'T(v: T) { return v; }
			//
			//     id(Foo(3)).x

			Type const *object_type = typecheck_subexpr(*e.object, subst, ctx);

			if(optional<GenericDeductionVar> object_var = get_if_type_deduction_var(*object_type))
			{
				GenericDeductionVar var = ctx.new_deduction_var(TypeDeductionVar());
				subst.constraints->add_relational_constraint(var, ConstraintEdge{
					.arg = mk_generic_arg(*object_var),
					.arg_modifier = MemberTypeModifier(e.member),
				});

				e.type = ctx.arena.alloc<Type>(VarType(UNKNOWN_TOKEN_RANGE, var));
			}
			else
			{
				std::expected<Type, ErrorMsg> member_type = get_member_type(object_type, e.member, e.range, *ctx.mod);
				if(not member_type)
					throw ParseError(member_type.error().msg);

				e.type = clone_ptr(&member_type.value(), ctx.arena);
			}

			return e.type;
		},
		[&](AssignmentExpr &e) -> Type*
		{
			Type const *lhs_type = typecheck_subexpr(*e.lhs, subst, ctx);
			Type const *rhs_type = typecheck_subexpr(*e.rhs, subst, ctx);

			auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
			{
				throw_sem_error("Invalid operands for assignment operator: " + reason, token_range_of(expr).first, &mod);
			};
			Unifier(ctx)
				.left(*lhs_type, TypeConversion::NONE)
				.right(*rhs_type, TypeConversion::IMPLICIT_CTOR, e.rhs)
				.set(&subst)
				.set(LazyErrorMsg(&expr, error_msg))
				.go();

			subst.constraints->add_check(LValueCheck(e.lhs, IsMutable::YES));

			return e.type = clone_ptr(lhs_type, ctx.arena);
		},
		[&](AsExpr &e) -> Type*
		{
			typecheck_type(*e.target_type, subst, ctx);
			typecheck_subexpr(*e.src_expr, subst, ctx);

			subst.constraints->add_check(CastCheck{e.src_expr, e.target_type});
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
			Type *callable_type = typecheck_subexpr(*e.callable, subst, ctx);
			ProcType *callable_proc_type = std::get_if<ProcType>(callable_type);
			if(not callable_proc_type)
				throw_sem_error("Expected callable expression", e.range.first, ctx.mod);

			callable_proc_type->inst->typecheck(subst, ctx);

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
				Type *arg_type = typecheck_subexpr(arg.expr, subst, ctx);
				Unifier(ctx)
					.left(param_type, TypeConversion::NONE)
					.right(*arg_type, TypeConversion::IMPLICIT_CTOR, &arg.expr)
					.set(&subst)
					.set(LazyErrorMsg(&arg.expr, arg_error_msg))
					.go();
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
		[&](GenericVarExpr &e) -> Type*
		{
			e.type = e.var | match
			{
				[&](GenericParameterVar const &v)
				{
					return v.def->kind | match
					{
						[&](GenericTypeParameter const&) -> Type*
						{
							assert(!"typecheck_subexpr: GenericVarExpr: GenericTypeParameter");
						},
						[&](GenericValueParameter const &p) -> Type*
						{
							return clone_ptr(p.type, ctx.arena);
						},
					};
				},
				[&](GenericDeductionVar const &v)
				{
					return v.def->kind | match
					{
						[&](TypeDeductionVar const&) -> Type*
						{
							assert(!"typecheck_subexpr: GenericVarExpr: TypeDeductionVar");
						},
						[&](ValueDeductionVar const &v) -> Type*
						{
							return clone_ptr(v.type, ctx.arena);
						},
					};
				},
			};

			return e.type;
		},
		[&](Path&) -> Type* { assert(!"typecheck_subexpr: Path"); },
	};

	return res;
	return nullptr;
}

static Type const* typecheck_expr(Expr &expr, SemaContext &ctx)
{
	LOGGER(ctx.mod->logger, on_expr_start, expr);

	ConstraintSystem constraints(*ctx.mod);
	ConstraintGatheringSubst constraint_gatherer(&constraints, &ctx);
	Type const *type = typecheck_subexpr(expr, constraint_gatherer, ctx);
	LOGGER(ctx.mod->logger, on_data, constraints);

	TypeEnv subst = create_subst_from_constraints(constraints, ctx);
	substitute_types_in_expr(expr, subst, ctx.mod->sema->insts, FullDeductionSubstitution(token_range_of(expr)));

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
				.pointee = clone_ptr(&rhs_type, ctx.arena),
				.kind = PointerType::SINGLE,
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
			ConstraintGatheringSubst subst(&constraints, &ctx);
			Unifier(ctx)
				.left(*p.ctor, lhs_conv)
				.right(rhs_type, rhs_conv)
				.set(&subst)
				.set(error_msg)
				.go();

			// Make sure the constructor is not a type variable
			if(VarType const *var = std::get_if<VarType>(p.ctor))
			{
				var->var | match
				{
					[&](GenericParameterVar)
					{
						throw_sem_error("Type parameters cannot be used as constructor patterns", var->range.first, ctx.mod);
					},
					[&](GenericDeductionVar)
					{
						assert(!"Constructor pattern is GenericDeductionVar");
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
	ConstraintGatheringSubst subst(&constraints, &ctx);

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

		typecheck_type(*lhs_pattern.provided_type, subst, ctx);

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
			Unifier(ctx)
				.left(*lhs_pattern.provided_type, TypeConversion::NONE)
				.right(rhs_type, TypeConversion::IMPLICIT_CTOR, root_rhs_expr)
				.set(&subst)
				.set(LazyErrorMsg(&lhs_pattern, error_msg))
				.go();
		}
		else
		{
			Unifier(ctx)
				.left(rhs_type, TypeConversion::NONE)
				.right(*lhs_pattern.provided_type, TypeConversion::IMPLICIT_CTOR)
				.set(&subst)
				.set(LazyErrorMsg(&lhs_pattern, error_msg))
				.go();
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

void typecheck_struct(StructItem *struct_, SemaContext &ctx);

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
				ConstraintGatheringSubst constraint_gatherer(&constraints, &ctx);
				Type const *init_type = typecheck_subexpr(*s.init_expr, constraint_gatherer, ctx);
				typecheck_pattern(*s.lhs, *init_type, true, constraints, ctx, s.init_expr);
				LOGGER(ctx.mod->logger, on_data, constraints);

				TypeEnv subst = create_subst_from_constraints(constraints, ctx);
				LOGGER(ctx.mod->logger, on_data, subst);

				substitute_types_in_expr(*s.init_expr, subst, ctx.mod->sema->insts, FullDeductionSubstitution(s.range));
				substitute_types_in_pattern(*s.lhs, subst, ctx.mod->sema->insts, FullDeductionSubstitution(s.range));

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
				ConstraintGatheringSubst constraint_gatherer(&constraints, &ctx);
				Type const *ret_expr_type = typecheck_subexpr(*s.ret_expr, constraint_gatherer, ctx);

				ConstraintGatheringSubst constraint_subst(&constraints, &ctx);
				Unifier(ctx)
					.left(*ctx.proc->ret_type, TypeConversion::NONE)
					.right(*ret_expr_type, TypeConversion::IMPLICIT_CTOR, s.ret_expr)
					.set(&constraint_subst)
					.set(LazyErrorMsg(s.ret_expr, error_msg))
					.go();

				LOGGER(ctx.mod->logger, on_data, constraints);

				TypeEnv subst = create_subst_from_constraints(constraints, ctx);
				LOGGER(ctx.mod->logger, on_data, subst);

				substitute_types_in_expr(*s.ret_expr, subst, ctx.mod->sema->insts, FullDeductionSubstitution(s.range));

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
		[&](DeclStmt const &s)
		{
			typecheck_struct(s.item, ctx);
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
							FullDeductionSubstitution(token_range_of(arm.capture))
						);

						if(is<WildcardPattern>(arm.capture))
							has_wildcard = true;
						else
						{
							StructType *arm_struct_type = std::get_if<StructType>(&type_of(arm.capture));
							if(not arm_struct_type)
								throw_sem_error("Must match against a case member", token_range_of(arm.capture).first, ctx.mod);

							StructInstance *arm_inst = arm_struct_type->inst;
							if(arm_inst->variant_parent() != subject)
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
							FullDeductionSubstitution(token_range_of(arm.capture))
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

void typecheck_param(Parameter &param, SemaContext &ctx)
{
	ConstraintSystem constraints(*ctx.mod);
	ConstraintGatheringSubst subst(&constraints, &ctx);
	typecheck_type(*param.type, subst, ctx);

	if(Expr *default_value = param.default_value.try_get_expr())
	{
		LOGGER(ctx.mod->logger, on_expr_start, *default_value);

		Type const *default_value_type = typecheck_subexpr(*default_value, subst, ctx);
		LOGGER(ctx.mod->logger, on_data, constraints);

		auto error_msg = [](Expr const &init_expr, string const &reason, Module const &mod)
		{
			throw_sem_error("Invalid default value: " + reason, token_range_of(init_expr).first, &mod);
		};

		Unifier(ctx)
			.left(*param.type, TypeConversion::NONE)
			.right(*default_value_type, TypeConversion::IMPLICIT_CTOR, default_value)
			.set(&subst)
			.set(LazyErrorMsg(default_value, error_msg))
			.go();

		TypeEnv subst = create_subst_from_constraints(constraints, ctx);
		LOGGER(ctx.mod->logger, on_data, subst);

		substitute_types_in_expr(*default_value, subst, ctx.mod->sema->insts, FullDeductionSubstitution(token_range_of(*default_value)));
		LOGGER(ctx.mod->logger, on_expr_end);
	}
}

void typecheck_struct(StructItem *struct_, SemaContext &ctx)
{
	for(Member &m: *struct_->members)
	{
		m | match
		{
			[&](VarMember &var_member) { typecheck_param(var_member.var, ctx); },
			[&](CaseMember case_member) { typecheck_struct(case_member.struct_, ctx); },
			[&](StructMember struct_member) { typecheck_struct(struct_member.struct_, ctx); },
		};
	}
}

static void typecheck(SemaContext &ctx)
{
	for(TopLevelItem &item: to_range(ctx.mod->items.list()))
	{
		item | match
		{
			[&](ProcItem &proc)
			{
				LOGGER(ctx.mod->logger, on_proc_start, &proc);

				for(auto const &[idx, param]: *proc.params | std::views::enumerate)
				{
					typecheck_param(param, ctx);
					proc.sema->param_vars->items[idx]->type = param.type;
				}

				ConstraintSystem constraints(*ctx.mod);
				ConstraintGatheringSubst subst(&constraints, &ctx);
				typecheck_type(*proc.ret_type, subst, ctx);

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
			[&](AliasItem &alias)
			{
				ConstraintSystem constraints(*ctx.mod);
				ConstraintGatheringSubst subst(&constraints, &ctx);
				typecheck_type(*alias.aliased_type, subst, ctx);
			},
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
			[&](VarMember const &var_member)
			{
				if(Expr *default_value = var_member.var.default_value.try_get_expr())
					check_default_value_deps(struct_, name_of(var_member.var, ctx.mod), default_value, default_value_deps, *ctx.mod);
			},
			[&](CaseMember case_member)
			{
				check_default_values_in_struct(case_member.struct_, default_value_deps, ctx);
			},
			[&](StructMember struct_member)
			{
				check_default_values_in_struct(struct_member.struct_, default_value_deps, ctx);
			},
		};
	}
}

void check_default_values(SemaContext const &ctx)
{
	std::unordered_map<DefaultValueDep, VisitState> default_value_deps;
	for(TopLevelItem const &item: to_range(ctx.mod->items.list()))
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
	while(struct_->is_case_member())
		struct_ = struct_->variant_parent();

	if(struct_->struct_()->name == "Option")
	{
		Type const &first_arg = std::get<Type>(struct_->type_args().args->items[0]);
		if(is<PointerType>(first_arg))
			return &first_arg;
	}

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
		[&](ArrayType const &t)
		{
			MemoryLayout element_layout = compute_layout(*t.element, parent_type_deps);
			return MemoryLayout{
				.size = element_layout.size * t.count(),
				.alignment = element_layout.alignment,
			};
		},
		[&](StructType const &t)
		{
			assert(t.inst->is_concrete());
			if(parent_type_deps)
				parent_type_deps->insert(t.inst);

			StructInstance *cur = t.inst;
			while(cur->is_case_member())
				cur = cur->variant_parent();

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
		[&](DeclStmt const&)
		{
			output_html("struct");
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
		[&](DeclStmt const&)
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
		m_os << "<code class='myca-inline'>"; print(Type(StructType(UNKNOWN_TOKEN_RANGE, inst)), *m_mod, m_os); m_os << "</code>";
		m_os << " <span class='side-info'>(" << inst << ")</span>\n";
		m_os << "<ul>\n";
}

void EventLogger::on_struct_substitution_replaced(StructInstance *inst)
{
	(void)inst;
			m_os << "<li>Replaced: ";
			m_os << "<code class='myca-inline'>"; print(Type(StructType(UNKNOWN_TOKEN_RANGE, inst)), *m_mod, m_os); m_os << "</code>";
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
		m_os << "<code class='myca-inline'>"; print(Type(StructType(UNKNOWN_TOKEN_RANGE, inst)), *m_mod, m_os); m_os << "</code>";
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
		m_os << "<code class='myca-inline'>"; print(Type(StructType(UNKNOWN_TOKEN_RANGE, inst)), *m_mod, m_os); m_os << "</code>";
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
	if(sys.checks.size() or sys.nodes.size())
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
