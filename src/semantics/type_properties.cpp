#include "semantics/context.hpp"
#include "semantics/module.hpp"
#include "semantics/type_properties.hpp"
#include "semantics/type_env.hpp"

optional<IsMutable> lvalue_mutability(Expr const &expr)
{
	return expr | match
	{
		[&](VarExpr const &e) -> optional<IsMutable>
		{
			return e.var->mutability;
		},
		[&](DerefExpr const &e) -> optional<IsMutable>
		{
			return std::get<PointerType>(e.addr->type()).mutability;
		},
		[&](IndexExpr const &e) -> optional<IsMutable>
		{
			Type const &indexed_type = e.addr->type();
			return indexed_type | match
			{
				[&](ArrayType const&) -> optional<IsMutable>
				{
					return lvalue_mutability(*e.addr);
				},
				[&](PointerType const &ptr) -> optional<IsMutable>
				{
					assert(ptr.kind == PointerType::MANY);
					return ptr.mutability;
				},
				[](auto const&) -> optional<IsMutable> { assert(!"lvalue_mutability: IndexExpr"); },
			};
		},
		[&](MemberAccessExpr const &e) -> optional<IsMutable>
		{
			return lvalue_mutability(*e.object);
		},
		[](GenericVarExpr const &e) -> optional<IsMutable>
		{
			assert(not is<GenericDeductionVar>(e.var));
			return nullopt;
		},
		[](auto const&) -> optional<IsMutable> { return nullopt; },
	};
}

bool is_cast_ok(Type const &target_type, Expr &src_expr, SemaContext &ctx)
{
	Type const &src_type = src_expr.type();
	try
	{
		TypeEnvReadonlySubst subst({}, ctx.arena);
		unify(
			UnifierOperand(target_type, TypeConversion::NONE),
			UnifierOperand(src_type, TypeConversion::IMPLICIT_CTOR, &src_expr),
			UnifierState(ctx, &subst)
		);
		return true;
	}
	catch(ParseError const&) {}

	return src_type | match
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
		[&](VarType const &t) -> bool
		{
			assert(not is<GenericDeductionVar>(t.var));
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

bool is_builtin_int_type(BuiltinTypeDef type)
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

bool is_signed(BuiltinTypeDef type)
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

bool builtin_losslessly_convertible(BuiltinTypeDef dest, BuiltinTypeDef src)
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

bool integer_assignable_to(BuiltinTypeDef target_type, Int128 val)
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

optional<Type> common_int_type(Type const &a, Type const &b)
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

Type materialize_known_int(KnownIntType known_int)
{
	if(integer_assignable_to(BuiltinTypeDef::I32, known_int.low) and integer_assignable_to(BuiltinTypeDef::I32, known_int.high))
		return BuiltinType(UNKNOWN_TOKEN_RANGE, BuiltinTypeDef::I32);
	else
		return BuiltinType(UNKNOWN_TOKEN_RANGE, smallest_int_type_for(known_int.low, known_int.high));
}
