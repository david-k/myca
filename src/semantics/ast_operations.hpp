#pragma once

#include "utils.hpp"
#include "syntax/ast.hpp"

#include <unordered_set>

using std::unordered_set;

bool operator == (GenericVar const &a, GenericVar const &b);
bool equiv(Type const &a, Type const &b);
bool equiv(Expr const &a, Expr const &b);
bool equiv(GenericArg const &a, GenericArg const &b);

struct TypeEquiv
{
	bool operator () (Type const &a, Type const &b) const
	{
		return equiv(a, b);
	}
};

struct ExprEquiv
{
	bool operator () (Expr const &a, Expr const &b) const
	{
		return equiv(a, b);
	}
};

bool operator < (GenericVar const &a, GenericVar const &b);
bool operator < (Expr const &a, Expr const &b);
bool operator < (GenericArg const &a, GenericArg const &b);
bool operator < (Type const &a, Type const &b);

template<>
struct std::hash<GenericDeductionVar>
{
	size_t operator () (GenericDeductionVar var) const { return compute_hash(var.def); }
};

template<>
struct std::hash<GenericVar>
{
	size_t operator () (GenericVar var) const;
};

template<>
struct std::hash<Type>
{
	size_t operator () (Type const &type) const;
};

template<>
struct std::hash<Expr>
{
	size_t operator () (Expr const &expr) const;
};

template<>
struct std::hash<GenericArg>
{
	size_t operator () (GenericArg const &arg) const;
};

template<>
struct std::hash<FixedArray<GenericArg>>
{
	size_t operator () (FixedArray<GenericArg> const &arg) const;
};

struct TypeArgList;
TypeArgList clone(TypeArgList const &args, Arena &arena);
ProcTypeParameter clone(ProcTypeParameter const &param, Arena &arena);
GenericArg clone(GenericArg const &arg, Arena &arena);
Argument clone(Argument const &arg, Arena &arena);
Type clone(Type const &type, Arena &arena);
Expr clone(Expr const &expr, Arena &arena);
Stmt clone(Stmt const &stmt, Arena &arena);

template<typename T>
T* clone_ptr(T const *ptr, Arena &arena)
{
	if(not ptr)
		return nullptr;

	return arena.alloc<T>(clone(*ptr, arena));
}

void print_type_args(FixedArray<GenericArg> const *type_args, Module const &mod, std::ostream &os);
void print(GenericArg const &g, Module const &mod, std::ostream &os);
void print(Path const &path, Module const &mod, std::ostream &os);
void print(GenericVar const &var, std::ostream &os);
void print(Type const &type, Module const &mod, std::ostream &os);
void print(Expr const &expr, Module const &mod, std::ostream &os);
void print(Pattern const &pattern, Module const &mod, std::ostream &os);
void print(Stmt const &stmt, Module const &mod, std::ostream &os);
void print(TopLevelItem const &item, Module const &mod, std::ostream &os);
void print(Module const &mod, std::ostream &os);

string_view str(BuiltinTypeDef t);
string str(Type const &type, Module const &mod);
string str(Expr const &expr, Module const &mod);
string str(Stmt const &stmt, Module const &mod);
string str(GenericArg const &a, Module const &mod);

Type* mk_builtin_type(BuiltinTypeDef builtin, Arena &arena);
Type* mk_known_int_type(Int128 low, Int128 high, Arena &arena);
Type* mk_pointer_type(Type *pointee, IsMutable mutability, Arena &arena);
GenericArg mk_generic_arg(GenericParameter const *param, TokenRange range);
GenericArg mk_generic_arg(GenericDeductionVar var);

using TypeInstance = variant<StructInstance*, UnionInstance*>;
template<>
struct std::hash<TypeInstance>
{
	size_t operator () (TypeInstance const &v) const;
};
void compute_direct_type_dependencies(Type const &type, unordered_set<TypeInstance> &deps);

bool type_var_occurs_in(GenericVar var, Expr const &expr);
bool type_var_occurs_in(GenericVar var, Type const &type);
bool type_var_occurs_in(GenericVar var, GenericArg const &arg);

void const_eval(Type &type, Module const &mod);
void const_eval(Expr &expr, Module const &mod);

struct TopLevelTarget
{
	friend bool operator == (TopLevelTarget, TopLevelTarget) = default;
};
template<>
struct std::hash<TopLevelTarget>
{
	size_t operator () (TopLevelTarget) const { return 273654124; }
};

template<>
struct std::hash<pair<StructItem const*, VarMember const*>>
{
	size_t operator () (pair<StructItem const*, VarMember const*> p) const
	{
		size_t h = 0;
		combine_hashes(h, compute_hash(p.first));
		combine_hashes(h, compute_hash(p.second));
		return h;
	}
};

using OptionTarget = variant<
	TopLevelTarget,
	StructItem const*,
	pair<StructItem const*, VarMember const*>,
	ProcItem const*,
	AliasItem const*,
	Stmt const*
>;

struct ModuleOptions
{
	bool empty() const { return opts.empty(); }

	OptionSet const* try_get(OptionTarget target) const
	{
		auto target_it = opts.find(target);
		if(target_it == opts.end())
			return nullptr;

		return &target_it->second;
	}

	optional<string_view> try_get(OptionTarget target, string_view option_name) const
	{
		if(OptionSet const *opts = try_get(target))
			return opts->try_get(option_name);

		return nullopt;
	}

	unordered_map<OptionTarget, OptionSet> opts;
};

// Extract options from special comments
ModuleOptions gather_options(Module const &mod);
void print_options(OptionSet const &options, std::ostream &os);
string str(OptionTarget target, Module const &mod);
