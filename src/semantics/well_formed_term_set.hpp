#pragma once

#include "semantics/ast_operations.hpp"

// Since Myca is using definition-checked generics (like Rust, unlike C++) we need to make sure that
// instantiating a generic proc/struct does not introduce any invalid types or expressions
// ("invalid" here means any type/expr that would have been rejected by the typechecker if all type
// parameters were known).
//
// This is quite difficult in general. For example, at the moment we forbid zero-sized static arrays
// (makes it easier to transpile to C). Now imagine you have a proc like this:
//
//   proc foo'(let count: usize)()
//   {
//       let arr: [count - 1]i32;
//   }
//
// We need to statically ensure that `count - 1 > 0`. Since we can't do that we need to reject the
// above code.
//
// However, we do want to allow the following:
//
//   proc concat'
//     (T, let count_a: usize, let count_b: usize)
//     (a: ^[count_a]T, b: ^[count_b]T)
//     -> [count_a + count_b]T
//  {
//      ...
//  }
//
// So simply rejecting any complex expression with generic parameters is not an option.
//
// The way Rust intends to tackle the problem is described here: https://hackmd.io/OZG_XiLFRs2Xmw5s39jRzA?view
// The insight is that complex expressions involving generic parameters are fine as long as they
// appear in the signature of the procedure. This is the case for `concat()` above but not for
// `foo()`. This ensures that no implementation detail leaks out to the caller. At the call site, we
// either have concrete value for e.g. `count_a` and `count_b`, or we require the caller itself to
// use `count_a + count_b` in the signature. This mechanism ensures that we always reach a point
// where all generic parameters are known and thus the expressions they appear in can be fully
// checked.
//
// `WellFormedTermSet` gathers all terms (types and expressions) that appear in the signature of a
// proc/struct, and the implementation is only allowed to use generic parameters in expressions that
// are part of this set.
class WellFormedTermSet
{
public:
	WellFormedTermSet(Module const &mod, Arena &arena) :
		m_mod(&mod),
		m_arena(&arena) {}

	void add(Type const &type);
	void add(Expr const &expr);

	bool contains(Type const &type) const
	{
		return m_well_formed_types.contains(type);
	}

	bool contains(Expr const &expr) const
	{
		return m_well_formed_exprs.contains(expr);
	}

	void clear()
	{
		m_well_formed_types.clear();
		m_well_formed_exprs.clear();
	}

private:
	unordered_set<Type, std::hash<Type>, TypeEquiv> m_well_formed_types;
	unordered_set<Expr, std::hash<Expr>, ExprEquiv> m_well_formed_exprs;
	Module const *m_mod;
	Arena *m_arena;
};

void gather_well_formed_terms(ProcItem const &proc, WellFormedTermSet &well_formed_terms);
void gather_well_formed_terms(StructItem const &struct_, WellFormedTermSet &well_formed_terms);
