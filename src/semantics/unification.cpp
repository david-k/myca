#include "semantics/ast_operations.hpp"
#include "semantics/context.hpp"
#include "semantics/instance_registry.hpp"
#include "semantics/type_properties.hpp"
#include "semantics/type_env.hpp"
#include "semantics/unification.hpp"

[[noreturn]] static void throw_unification_error(
	GenericArg const &left,
	GenericArg const &right,
	UnifierState const &state
)
{
	GenericArg const *actual_left = &left;
	GenericArg const *actual_right = &right;
	if(*actual_right < *actual_left)
		std::swap(actual_left, actual_right);

	// TODO Instead of determining actual_left and actual_right based on their order, do so based on
	//      state.are_sides_swapped. Then pass them as arguments to state.err->throw_error() for
	//      better error messages. (The reason I currently use ordering is that it creates the same
	//      error message independent of the order that left and right are passed to the unifier,
	//      which is useful when writing tests that expect specific error messages.)
	//if(state.are_sides_swapped)
	//	std::swap(actual_left, actual_right);

	string reason =
		"Incompatible types " + str(*actual_left, *state.ctx.mod) +
		" and " + str(*actual_right, *state.ctx.mod);

	if(state.err)
		state.err->throw_error(reason, *state.ctx.mod);
	else
		throw ParseError(reason);

	UNREACHABLE;
}

static Expr call_implicit_ctor(StructInstance *implicit_case, Expr const &arg, Arena &arena)
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
				.range = expr->token_range(),
				.alt_expr = clone_ptr(expr, arena),
				.alt_type = clone_ptr(&c.alt, arena),
				.type = arena.alloc<Type>(UnionType(UNKNOWN_TOKEN_RANGE, c.union_)),
			});
		},
		[&](ToPointerConversion c)
		{
			*expr = Expr(AddressOfExpr{
				.range = expr->token_range(),
				.object = clone_ptr(expr, arena),
				.mutability = c.mutability,
				.type = arena.alloc<Type>(PointerType{
					.range = UNKNOWN_TOKEN_RANGE,
					.pointee = clone_ptr(&expr->type(), arena),
					.kind = PointerType::SINGLE,
					.mutability = c.mutability,
				})
			});
		},
	};
}

GenericArg const* TypeEnvReadonlySubst::try_get(GenericDeductionVar var)
{
	return env->try_lookup(var);
}

void TypeEnvReadonlySubst::on_conversion_request(TypeConversionEvent const &event, Expr *expr)
{
	apply_conversion(event, expr, *arena);
}

void TypeEnvReadonlySubst::set(
	GenericDeductionVar,
	TypeConversion,
	Expr *NULLABLE,
	GenericArg const&,
	TypeConversion,
	Expr *NULLABLE,
	optional<LazyErrorMsg>
)
{
	assert(!"TypeEnvReadonlySubst::set()");
}

//--------------------------------------------------------------------
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

static bool try_unify_integer_types(
	UnifierOperand const &left,
	UnifierOperand const &right,
	UnifierState const &state
)
{
	Type const &left_type = std::get<Type>(left.arg);
	Type const &right_type = std::get<Type>(right.arg);

	if(not is_integer_type(left_type) or not is_integer_type(right_type))
		return false;

	optional<Type> common_type = common_int_type(left_type, right_type);
	if(not common_type)
		throw_unification_error(left.arg, right.arg, state);

	optional<TypeConversion> act_conv_left = try_convert_integer_type(left_type, *common_type, left.conv);
	if(not act_conv_left)
		throw_unification_error(left.arg, right.arg, state);

	optional<TypeConversion> act_conv_right = try_convert_integer_type(right_type, *common_type, right.conv);
	if(not act_conv_right)
		throw_unification_error(left.arg, right.arg, state);

	if(state.result)
		*state.result = *common_type;

	if(*act_conv_left > TypeConversion::NONE)
		state.emit_conversion(*act_conv_left, left.expr);

	if(*act_conv_right > TypeConversion::NONE)
		state.emit_conversion(*act_conv_right, right.expr);

	return true;
}

//--------------------------------------------------------------------
// Unification of type deduction variables
//--------------------------------------------------------------------
static optional<GenericDeductionVar> get_if_type_deduction_var(Type const &type)
{
	VarType const *var_type = std::get_if<VarType>(&type);
	if(not var_type)
		return nullopt;

	GenericDeductionVar const* type_deduction_var = std::get_if<GenericDeductionVar>(&var_type->var);
	if(not type_deduction_var)
		return nullopt;

	return *type_deduction_var;
}

static optional<GenericDeductionVar> get_if_type_deduction_var(Expr const &expr)
{
	GenericVarExpr const *var = std::get_if<GenericVarExpr>(&expr);
	if(not var)
		return nullopt;

	GenericDeductionVar const* type_deduction_var = std::get_if<GenericDeductionVar>(&var->var);
	if(not type_deduction_var)
		return nullopt;

	return *type_deduction_var;
}

static optional<GenericDeductionVar> get_if_deduction_var(GenericArg const &arg)
{
	return arg | match
	{
		[](Type const &t) { return get_if_type_deduction_var(t); },
		[](Expr const &e) { return get_if_type_deduction_var(e); },
	};
}

static bool try_unify_type_deduction_vars(
	UnifierOperand const &left,
	UnifierOperand const &right,
	UnifierState const &state
)
{
	optional<GenericDeductionVar> left_var = get_if_deduction_var(left.arg);
	optional<GenericDeductionVar> right_var = get_if_deduction_var(right.arg);

	if(GenericArg const* arg = left_var ? state.try_lookup(*left_var) : nullptr)
	{
		unify(UnifierOperand(left).replace(*arg), right, state);
		return true;
	}

	if(GenericArg const *arg = right_var ? state.try_lookup(*right_var) : nullptr)
	{
		unify(left, UnifierOperand(right).replace(*arg), state);
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

			// TODO What should we do if state.subst is null? Can this even happen?

			state.subst->set(
				*left_var, left.conv, left.expr,
				right.arg, right.conv, right.expr,
				state.err
			);
		}

		if(state.result)
		{
			// It's important here to chose the right side as the result so the constraint solver
			// can make progress (the left side is pretty boring since it is just a
			// GenericDeductionVar, while the right side might be an actual type, giving the constraint
			// solver additional information)
			*state.result = right.arg;
		}

		return true;
	}

	if(right_var)
		return try_unify_type_deduction_vars(right, left, state.with_sides_swapped());

	return false;
}

//--------------------------------------------------------------------
// Unification of struct types
//--------------------------------------------------------------------
static Type const* try_find_matching_alt_for(
	UnionInstance *union_,
	GenericArg const &proposed_alt,
	Expr *NULLABLE expr,
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
	UnifierState const &state
)
{
	assert(left.args->count == right.args->count);
	for(size_t i = 0; i < left.args->count; ++i)
	{
		unify(
			UnifierOperand{.arg = left.args->items[i],  .conv = TypeConversion::NONE},
			UnifierOperand{.arg = right.args->items[i], .conv = TypeConversion::NONE},
			state
		);
	}
}

static void unify_structs_eq(
	StructInstance *left,
	StructInstance *right,
	UnifierState const &state
)
{
	if(left != right)
	{
		if(left->is_deduction_complete() && right->is_deduction_complete())
		{
			throw_unification_error(
				StructType(UNKNOWN_TOKEN_RANGE, left),
				StructType(UNKNOWN_TOKEN_RANGE, right),
				state
			);
		}
		else
		{
			if(left->struct_() != right->struct_())
			{
				throw_unification_error(
					StructType(UNKNOWN_TOKEN_RANGE, left),
					StructType(UNKNOWN_TOKEN_RANGE, right),
					state
				);
			}

			optional<DeclContainerInst> left_cont{left};
			optional<DeclContainerInst> right_cont{right};
			do
			{
				unify_type_args(left_cont->type_args(), right_cont->type_args(), state);
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

static StructInstance* try_get_implicit_ctor_for(
	StructInstance *struct_,
	GenericArg const &arg,
	Expr *NULLABLE arg_expr,
	Subst *subst,
	SemaContext const &ctx
)
{
	StructInstance *implicit_case = struct_->implicit_case();
	if(not implicit_case)
		return nullptr;

	ProcType const &implicit_ctor = std::get<ProcType>(*implicit_case->try_get_ctor_type());
	FixedArray<ProcTypeParameter> const *implicit_ctor_params = implicit_ctor.inst->params;
	GenericArg const &implicit_param_type = implicit_ctor_params->items[0].type;

	try
	{
		unify(
			UnifierOperand{.arg = implicit_param_type, .conv = TypeConversion::NONE},
			UnifierOperand{.arg = arg, .conv = TypeConversion::IMPLICIT_CTOR, .expr = arg_expr},
			UnifierState{.ctx = ctx, .subst = subst}
		);
		return implicit_case;
	}
	catch(ParseError const&) {
		return nullptr;
	}
}

static bool try_unify_structs(
	UnifierOperand const &left,
	UnifierOperand const &right,
	UnifierState const &state
)
{
	Type const &left_type = std::get<Type>(left.arg);
	Type const &right_type = std::get<Type>(right.arg);

	StructInstance *left_struct = get_if_struct(&left_type);
	StructInstance *right_struct = get_if_struct(&right_type);
	if(not left_struct)
	{
		if(not right_struct)
			return false;

		return try_unify_structs(right, left, state.with_sides_swapped());
	}

	// If either common_parent_left or common_parent_right is set, then the other one is also set,
	// and they both refer to the same StructItem
	StructInstance *common_parent_left = nullptr;
	StructInstance *common_parent_right = nullptr;
	if(right_struct)
		std::tie(common_parent_left, common_parent_right) = common_parent(left_struct, right_struct);

	if(common_parent_left)
	{
		// So `left_type` and `right_type` have a common parent. The only way unification can succeed is if
		// both sides can be converted to that same parent.
		if(
			not try_convert_struct_to_parent(left_struct, common_parent_left, left.conv)
			or not try_convert_struct_to_parent(right_struct, common_parent_right, right.conv)
		) {
			throw_unification_error(left.arg, right.arg, state);
		}

		unify_structs_eq(common_parent_left, common_parent_right, state);

		if(state.result)
			*state.result = StructType(UNKNOWN_TOKEN_RANGE, common_parent_left);

		if(left_struct != common_parent_left)
			state.emit_conversion(TypeConversion::TRIVIAL, left.expr);

		if(right_struct != common_parent_right)
			state.emit_conversion(TypeConversion::TRIVIAL, right.expr);
	}
	else if(UnionInstance *right_union = get_if_union(&right_type))
	{
		StructInstance *implicit_case_left = nullptr;
		Type const *matching_alt_right = nullptr;

		if(right.conv == TypeConversion::IMPLICIT_CTOR)
			implicit_case_left = try_get_implicit_ctor_for(left_struct, right.arg, right.expr, state.subst, state.ctx);

		if(left.conv == TypeConversion::IMPLICIT_CTOR)
			matching_alt_right = try_find_matching_alt_for(right_union, left.arg, left.expr, state.subst, state.ctx);

		if(not implicit_case_left and not matching_alt_right)
			throw_unification_error(left.arg, right.arg, state);

		// This shouldn't be possible at the moment because it would imply left_struct and
		// right_union are defined recursively. However, this could change once user-defined
		// constructors are supported.
		if(implicit_case_left and matching_alt_right)
		{
			string reason = "Ambiguous common type for " + str(left.arg, *state.ctx.mod) + " and " + str(right.arg, *state.ctx.mod);
			if(state.err)
				state.err->throw_error(reason, *state.ctx.mod);
			else
				throw ParseError(reason);
		}

		if(implicit_case_left)
		{
			if(state.result)
				*state.result = left.arg;

			state.emit_conversion(ConstructorConversion(implicit_case_left), right.expr);
		}

		if(matching_alt_right)
		{
			if(state.result)
				*state.result = right.arg;

			state.emit_conversion(UnionConversion(right_union, *matching_alt_right), left.expr);
		}
	}
	else
	{
		// No common parent, so the only way forward is using an implicit ctor

		StructInstance *implicit_case_right = nullptr;
		StructInstance *implicit_case_left = nullptr;

		if(right.conv == TypeConversion::IMPLICIT_CTOR)
			implicit_case_left = try_get_implicit_ctor_for(left_struct, right_type, right.expr, state.subst, state.ctx);

		if(left.conv == TypeConversion::IMPLICIT_CTOR and right_struct)
			implicit_case_right = try_get_implicit_ctor_for(right_struct, left_type, left.expr, state.subst, state.ctx);

		if(implicit_case_left and implicit_case_right)
		{
			string reason = "Ambiguous common type for " + str(left.arg, *state.ctx.mod) + " and " + str(right.arg, *state.ctx.mod);
			if(state.err)
				state.err->throw_error(reason, *state.ctx.mod);
			else
				throw ParseError(reason);
		}

		if(not implicit_case_left and not implicit_case_right)
			throw_unification_error(left.arg, right.arg, state);

		if(implicit_case_left)
		{
			if(state.result)
				*state.result = left.arg;

			state.emit_conversion(ConstructorConversion(implicit_case_left), right.expr);
		}

		if(implicit_case_right)
		{
			if(state.result)
				*state.result = right.arg;

			state.emit_conversion(ConstructorConversion(implicit_case_right), left.expr);
		}
	}

	return true;
}

//--------------------------------------------------------------------
// Unification of union types
//--------------------------------------------------------------------
static Type const* try_find_matching_alt_for(
	UnionInstance *union_,
	GenericArg const &proposed_alt,
	Expr *NULLABLE expr,
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
			unify(
				UnifierOperand{.arg = *alt, .conv = TypeConversion::NONE},
				UnifierOperand{.arg = proposed_alt, .conv = TypeConversion::TRIVIAL, .expr = expr},
				UnifierState{.ctx = ctx, .subst = subst}
			);
			success = true;
		}
		catch(ParseError const&) {}

		if(success)
		{
			if(matched_alt)
				throw_sem_error("Assignment to union is ambiguous", proposed_alt.token_range().first, ctx.mod);

			matched_alt = alt;
		}
	}

	return matched_alt;
}

bool try_unify_unions(
	UnifierOperand const &left,
	UnifierOperand const &right,
	UnifierState const &state
)
{
	Type const &left_type = std::get<Type>(left.arg);
	Type const &right_type = std::get<Type>(right.arg);

	UnionInstance *left_union = get_if_union(&left_type);
	UnionInstance *right_union = get_if_union(&right_type);
	if(not left_union)
	{
		if(not right_union)
			return false;

		return try_unify_unions(right, left, state.with_sides_swapped());
	}
	assert(left_union);

	if(left_union and right_union)
	{
		// Unions cannot be nested, so if we have two unions then they can only be unified if they
		// are equal

		if(not equiv(left.arg, right.arg))
			throw_unification_error(left.arg, right.arg, state);

		if(state.result)
			*state.result = left.arg;
	}
	else
	{
		// If we get here we know `left_type` is a union type and `right_type` can be anything except a
		// union. The only way unification can succeed here is if `right_type` can be unified with one of
		// the alternatives of `left_type`. (Note that the case where `right_type` is a struct with an
		// implicit constructor for `left_type` has already been handled in `try_unify_structs()`).

		if(right.conv != TypeConversion::IMPLICIT_CTOR)
			throw_unification_error(left.arg, right.arg, state);

		Type const *matched_alt = try_find_matching_alt_for(left_union, right_type, right.expr, state.subst, state.ctx);
		if(not matched_alt)
			throw_unification_error(left.arg, right.arg, state);

		if(state.result)
			*state.result = left.arg;

		state.emit_conversion(UnionConversion(left_union, *matched_alt), right.expr);
	}

	return true;
}

//--------------------------------------------------------------------
// Unification of pointer types
//--------------------------------------------------------------------
static IsMutable mutability_lub(IsMutable a, IsMutable b)
{
	if(a == IsMutable::YES and b == IsMutable::YES)
		return IsMutable::YES;

	return IsMutable::NO;
}

static optional<TypeConversion> can_convert_mutability(IsMutable from, IsMutable to, TypeConversion conv)
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

static bool try_unify_pointers(
	UnifierOperand const &left,
	UnifierOperand const &right,
	UnifierState const &state
)
{
	Type const &left_type = std::get<Type>(left.arg);
	Type const &right_type = std::get<Type>(right.arg);

	PointerType const *left_pointer = std::get_if<PointerType>(&left_type);
	PointerType const *right_pointer = std::get_if<PointerType>(&right_type);
	if(not left_pointer or not right_pointer)
		return false;

	if(left_pointer->kind != right_pointer->kind)
		throw_unification_error(left.arg, right.arg, state);

	IsMutable common_mutability = mutability_lub(left_pointer->mutability, right_pointer->mutability);
	optional<TypeConversion> act_conv_left = can_convert_mutability(left_pointer->mutability, common_mutability, left.conv);
	optional<TypeConversion> act_conv_right = can_convert_mutability(right_pointer->mutability, common_mutability, right.conv);
	if(not act_conv_left or not act_conv_right)
		throw_unification_error(left.arg, right.arg, state);

	GenericArg common_pointee;
	unify(
		UnifierOperand{
			.arg = *left_pointer->pointee,
			.conv = left.conv == TypeConversion::IMPLICIT_CTOR ? TypeConversion::TRIVIAL : left.conv,
			.expr = left.expr,
		},
		UnifierOperand{
			.arg = *right_pointer->pointee,
			.conv = right.conv == TypeConversion::IMPLICIT_CTOR ? TypeConversion::TRIVIAL : right.conv,
			.expr = right.expr,
		},
		state.with_result(&common_pointee)
	);

	if(state.result)
	{
		*state.result = PointerType{
			.range = UNKNOWN_TOKEN_RANGE,
			.pointee = clone_ptr(&common_pointee.as_type(), state.ctx.arena),
			.kind = left_pointer->kind,
			.mutability = common_mutability,
		};
	}

	if(*act_conv_left > TypeConversion::NONE)
		state.emit_conversion(*act_conv_left, left.expr);

	if(*act_conv_right > TypeConversion::NONE)
		state.emit_conversion(*act_conv_right, right.expr);

	return true;
}

//--------------------------------------------------------------------
// Unification of a reference parameter with its argument
//--------------------------------------------------------------------
static bool try_unify_ref_param(
	UnifierOperand const &left,
	UnifierOperand const &right,
	UnifierState const &state
)
{
	Type const &left_type = std::get<Type>(left.arg);
	Type const &right_type = std::get<Type>(right.arg);
	if(not is<PointerType>(left_type) and not is<PointerType>(right_type))
		return false;

	if(is<PointerType>(left_type) and is<PointerType>(right_type))
		return false;

	if(not is<PointerType>(left_type) and is<PointerType>(right_type))
		return try_unify_ref_param(right, left, state.with_sides_swapped());

	assert(is<PointerType>(left_type) and not is<PointerType>(right_type));
	if(not left.is_ref_param)
		return false;

	// Unifying two reference parameters should never happen
	assert(not right.is_ref_param);

	// We need to convert the right side to a pointer by taking its address, which requires
	// IMPLICIT_CTOR
	if(right.conv != TypeConversion::IMPLICIT_CTOR)
		return false;

	// Since the left is a function parameter we shouln't be allowed to convert its type
	assert(left.conv == TypeConversion::NONE);
	assert(not left.expr);
	PointerType const &ref_param = std::get<PointerType>(left_type);
	assert(ref_param.kind == PointerType::SINGLE);

	GenericArg common_pointee;
	unify(
		UnifierOperand{.arg = *ref_param.pointee, .conv = TypeConversion::NONE},
		UnifierOperand{
			.arg = right_type,
			.conv = TypeConversion::IMPLICIT_CTOR,
			.expr = right.expr,
		},
		state.with_result(&common_pointee)
	);

	if(state.result)
	{
		*state.result = PointerType{
			.range = UNKNOWN_TOKEN_RANGE,
			.pointee = clone_ptr(&common_pointee.as_type(), state.ctx.arena),
			.kind = PointerType::SINGLE,
			.mutability = ref_param.mutability,
		};
	}

	state.emit_conversion(ToPointerConversion(ref_param.mutability), right.expr);
	return true;
}

//--------------------------------------------------------------------
// Unification of array types
//--------------------------------------------------------------------
static bool try_unify_arrays(
	UnifierOperand const &left,
	UnifierOperand const &right,
	UnifierState const &state
)
{
	Type const &left_type = std::get<Type>(left.arg);
	Type const &right_type = std::get<Type>(right.arg);

	ArrayType const *left_array = std::get_if<ArrayType>(&left_type);
	ArrayType const *right_array = std::get_if<ArrayType>(&right_type);
	if(not left_array or not right_array)
		return false;

	GenericArg count_result;
	unify(
		UnifierOperand{.arg = *left_array->count_arg, .conv = TypeConversion::NONE},
		UnifierOperand{.arg = *right_array->count_arg, .conv = TypeConversion::NONE},
		state.with_result(&count_result)
	);

	GenericArg element_type_result;
	unify(
		UnifierOperand{.arg = *left_array->element, .conv = TypeConversion::NONE},
		UnifierOperand{.arg = *right_array->element, .conv = TypeConversion::NONE},
		state.with_result(&element_type_result)
	);

	if(state.result)
	{
		*state.result = ArrayType{
			.range = UNKNOWN_TOKEN_RANGE,
			.element = clone_ptr(&element_type_result.as_type(), state.ctx.arena),
			.count_arg = clone_ptr(&count_result.as_expr(), state.ctx.arena),
		};
	}

	return true;
}

//--------------------------------------------------------------------
// Unification of generic values
//--------------------------------------------------------------------
static void unify_generic_values(
	UnifierOperand const &left,
	UnifierOperand const &right,
	UnifierState const &state
)
{
	Expr const &left_expr = std::get<Expr>(left.arg);
	Expr const &right_expr = std::get<Expr>(right.arg);

	if(equiv(left_expr, right_expr))
	{
		if(state.result)
			*state.result = left.arg;

		return;
	}

	IntLiteralExpr const *left_int = std::get_if<IntLiteralExpr>(&left_expr);
	IntLiteralExpr const *right_int = std::get_if<IntLiteralExpr>(&right_expr);
	if(left_int and right_int)
	{
		if(left_int->value != right_int->value)
			throw_unification_error(left.arg, right.arg, state);

		if(state.result)
			*state.result = left.arg;
	}
	else
		throw_unification_error(left.arg, right.arg, state);
}

//--------------------------------------------------------------------
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
void unify(
	UnifierOperand const &left,
	UnifierOperand const &right,
	UnifierState const &state
)
{
	if(try_unify_type_deduction_vars(left, right, state))
		return;

	if(is<Type>(left.arg) and is<Type>(right.arg))
	{
		// Cases covered: is_integer_type(left) AND is_integer_type(right)
		if(try_unify_integer_types(left, right, state))
			return;

		// Activates if one side is a reference parameter.
		// Needs to come first struct and union uninfication.
		if(try_unify_ref_param(left, right, state))
			return;

		// Cases covered: is<PointerType>(left) AND is<PointerType>(right)
		if(try_unify_pointers(left, right, state))
			return;

		// Cases covered: is<ArrayType>(left) AND is<ArrayType>(right)
		if(try_unify_arrays(left, right, state))
			return;

		// Cases covered: is<StructType>(left) OR is<StructType>(right)
		if(try_unify_structs(left, right, state))
			return;

		// Cases covered: is<UnionType>(left) OR is<UnionType>(right)
		if(try_unify_unions(left, right, state))
			return;

		Type const &left_type = std::get<Type>(left.arg);
		Type const &right_type = std::get<Type>(right.arg);
		left_type | match
		{
			[&](BuiltinType const &left_t)
			{
				BuiltinType const *right_t = std::get_if<BuiltinType>(&right_type);
				if(not right_t)
					throw_unification_error(left.arg, right.arg, state);

				// The case where both left and right are integer types has already been handled above.
				// Non-integer builtin types (Never, Unit, Bool) can only be unified if they are equal.
				if(left_t.builtin != right_t->builtin)
					throw_unification_error(left.arg, right.arg, state);

				if(state.result) *state.result = left.arg;
			},
			[&](VarType const&)
			{
				// If we get here, we know left and right are GenericParameterVars (because we already
				// handled GenericDeductionVars above).
				//
				// Unification of GenericParameterVars only succeeds if both sides refer to the same
				// GenericParameterVar as they may be instantiated with any type.

				if(not equiv(left.arg, right.arg))
					throw_unification_error(left.arg, right.arg, state);

				if(state.result) *state.result = left.arg;
			},
			[&](ProcType const&)
			{
				assert(!"[TODO] unify: ProcType");
			},

			// Handled above
			[&](KnownIntType const&) { throw_unification_error(left.arg, right.arg, state); },
			[&](PointerType const&) { throw_unification_error(left.arg, right.arg, state); },
			[&](ArrayType const&) { throw_unification_error(left.arg, right.arg, state); },
			[&](StructType const&) { throw_unification_error(left.arg, right.arg, state); },
			[&](UnionType const&) { throw_unification_error(left.arg, right.arg, state); },

			[&](ProcTypeUnresolved const&) { assert(!"unify: ProcTypeUnresolved"); },
			[&](UnionTypeUnresolved const&) { assert(!"unify: UnionTypeUnresolved"); },
			[&](Path const&) { assert(!"unify: Path"); },
			[&](InlineStructType const&) { assert(!"unify: InlineStructType"); },
		};
	}
	else if(is<Expr>(left.arg) and is<Expr>(right.arg))
		unify_generic_values(left, right, state);
	else
		throw_unification_error(left.arg, right.arg, state);
}
