#pragma once

#include "semantics/passes.hpp"

// General type properties
optional<IsMutable> lvalue_mutability(Expr const &expr);
bool is_cast_ok(Type const &target_type, Expr &src_expr, SemaContext &ctx);
inline bool is_builtin_type(Type const &type, BuiltinTypeDef builtin)
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

// Integer type properties
bool is_builtin_int_type(BuiltinTypeDef type);
bool is_signed(BuiltinTypeDef type);

inline bool is_integer_type(Type const &type)
{
	return type | match
	{
		[&](BuiltinType const &t) { return is_builtin_int_type(t.builtin); },
		[&](KnownIntType const&) { return true; },
		[&](auto const&) { return false; },
	};
}

inline size_t size_of(BuiltinTypeDef type) { return get_layout(type).size; }

bool builtin_losslessly_convertible(BuiltinTypeDef dest, BuiltinTypeDef src);
bool integer_assignable_to(BuiltinTypeDef target_type, Int128 val);
optional<Type> common_int_type(Type const &a, Type const &b);
BuiltinTypeDef smallest_int_type_for(Int128 low, Int128 high);
BuiltinTypeDef smallest_int_type_for(Int128 value);
Type materialize_known_int(KnownIntType known_int);
