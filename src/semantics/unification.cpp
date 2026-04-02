#include "semantics/ast_operations.hpp"
#include "semantics/context.hpp"
#include "semantics/instance_registry.hpp"
#include "semantics/type_properties.hpp"
#include "semantics/type_env.hpp"
#include "semantics/unification.hpp"

[[noreturn]] static void throw_unification_error(
	GenericArg const &left,
	GenericArg const &right,
	optional<LazyErrorMsg> error_msg,
	Module const *mod
)
{
	// To make error messages more predictable (for testing)
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
				.range = expr->token_range(),
				.alt_expr = clone_ptr(expr, arena),
				.alt_type = clone_ptr(&c.alt, arena),
				.type = arena.alloc<Type>(UnionType(UNKNOWN_TOKEN_RANGE, c.union_)),
			});
		},
	};
}

GenericArg const* TypeEnvReadonlySubst::try_get(GenericDeductionVar var)
{
	return env->try_lookup(var);
}

void TypeEnvReadonlySubst::apply_conversion(TypeConversionEvent const &event, Expr *expr)
{
	::apply_conversion(event, expr, *arena);
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

//--------------------------------------------------------------------
// Unification of type deduction variables
//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
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
				throw_sem_error("Assignment to union is ambiguous", type.token_range().first, ctx.mod);

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

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
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
