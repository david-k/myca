#pragma once

#include "syntax/ast.hpp"
#include "semantics/error.hpp"
#include <expected>

struct SemaContext;
class StructInstance;
class UnionInstance;

//--------------------------------------------------------------------
// TypeConversionEvent (emitted during unification)
//--------------------------------------------------------------------
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

struct ConstructorConversion
{
	StructInstance *ctor;
};

struct UnionConversion
{
	UnionInstance *union_;
	Type alt;
};

struct ToPointerConversion
{
	IsMutable mutability;
};

using ConversionInfo = variant<ConstructorConversion, UnionConversion, ToPointerConversion>;

struct TypeConversionEvent
{
	TypeConversionEvent(TypeConversion kind) :
		kind(kind) {}

	TypeConversionEvent(ConstructorConversion info) :
		kind(TypeConversion::IMPLICIT_CTOR),
		info(info) {}

	TypeConversionEvent(UnionConversion info) :
		kind(TypeConversion::IMPLICIT_CTOR),
		info(info) {}

	TypeConversionEvent(ToPointerConversion info) :
		kind(TypeConversion::IMPLICIT_CTOR),
		info(info) {}

	TypeConversion const kind;
	optional<ConversionInfo> const info;
};

void apply_conversion(TypeConversionEvent const &conv, Expr *expr, Arena &arena);

//--------------------------------------------------------------------
// Subst
//--------------------------------------------------------------------
// Subst is a mapping from GenericDeductionVars to GenericArgs
class Subst
{
public:
	virtual GenericArg const* try_get(GenericDeductionVar var) = 0;
	virtual void set(
		GenericDeductionVar var,
		TypeConversion var_conv,
		Expr *NULLABLE var_expr,
		GenericArg const &arg,
		TypeConversion arg_conv,
		Expr *NULLABLE arg_expr,
		optional<LazyErrorMsg> error_msg
	) = 0;

	virtual void on_conversion_request(TypeConversionEvent const &event, Expr *expr) = 0;
};

class TypeEnvReadonlySubst : public Subst
{
public:
	TypeEnvReadonlySubst(TypeEnv const &env, Arena &arena) :
		env(&env),
		arena(&arena) {}

	virtual GenericArg const* try_get(GenericDeductionVar var) override;
	virtual void on_conversion_request(TypeConversionEvent const &event, Expr *expr) override;

	virtual void set(
		GenericDeductionVar var,
		TypeConversion var_conv,
		Expr *NULLABLE var_expr,
		GenericArg const &arg,
		TypeConversion arg_conv,
		Expr *NULLABLE arg_expr,
		optional<LazyErrorMsg> error_msg
	) override;

	TypeEnv const *env;
	Arena *arena;
};

//--------------------------------------------------------------------
// Unification
//--------------------------------------------------------------------
struct UnifierState
{
	GenericArg const* lookup(GenericDeductionVar var) const
	{
		assert(subst);
		GenericArg const *arg = subst->try_get(var);
		assert(arg);

		return arg;
	}

	GenericArg const* try_lookup(GenericDeductionVar var) const
	{
		if(not subst)
			return nullptr;

		return subst->try_get(var);
	}

	void emit_conversion(TypeConversionEvent const &ev, Expr *expr) const
	{
		if(not subst or not expr)
			return;

		subst->on_conversion_request(ev, expr);
	}

	UnifierState with_sides_swapped() const
	{
		UnifierState new_state(*this);
		new_state.are_sides_swapped = not new_state.are_sides_swapped;
		return new_state;
	}

	UnifierState with_result(GenericArg *result) const
	{
		UnifierState new_state(*this);
		new_state.result = result;
		return new_state;
	}

	SemaContext const &ctx;
	Subst *NULLABLE subst = nullptr;
	optional<LazyErrorMsg> err{};
	GenericArg *NULLABLE result = nullptr;
	bool are_sides_swapped = false; // Only used for error reporting
};

struct UnifierOperand
{
	UnifierOperand replace(GenericArg const &new_arg)
	{
		return {
			.arg = new_arg,
			.conv = conv,
			.expr = expr,
			.is_ref_param = is_ref_param,
		};
	}

	GenericArg const &arg;
	TypeConversion conv;
	Expr *NULLABLE expr = nullptr;
	bool is_ref_param = false;
};

void unify(
	UnifierOperand const &left,
	UnifierOperand const &right,
	UnifierState const &state
);

inline std::expected<void, string> try_unify(
	UnifierOperand const &left,
	UnifierOperand const &right,
	UnifierState const &state
)
{
	try
	{
		unify(left, right, state);
		return {};
	}
	catch(ParseError const &exc)
	{
		return std::unexpected(exc.what());
	}
}
