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

struct TypeArgList;
FixedArray<MatchArm>* clone(FixedArray<MatchArm> const *arms, Arena &arena);
FixedArray<Stmt>* clone(FixedArray<Stmt> const *stmts, Arena &arena);
FixedArray<Type>* clone(FixedArray<Type> const *types, Arena &arena);
TypeArgList clone(TypeArgList const &args, Arena &arena);
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
