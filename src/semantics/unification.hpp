#pragma once

#include "syntax/ast.hpp"
#include "semantics/error.hpp"

struct SemaContext;
class StructInstance;
class UnionInstance;

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

using ConversionInfo = variant<ConstructorConversion, UnionConversion>;

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

	TypeConversion const kind;
	optional<ConversionInfo> const info;
};

void apply_conversion(TypeConversionEvent const &conv, Expr *expr, Arena &arena);

class Subst
{
public:
	virtual GenericArg const* try_get(GenericDeductionVar var) = 0;
	virtual void apply_conversion(TypeConversionEvent const &event, Expr *expr) = 0;
	virtual void set(
		GenericDeductionVar var,
		TypeConversion var_conv,
		Expr *NULLABLE var_expr,
		GenericArg const &arg,
		TypeConversion arg_conv,
		Expr *NULLABLE arg_expr,
		optional<LazyErrorMsg> error_msg
	) = 0;
};

class TypeEnvReadonlySubst : public Subst
{
public:
	TypeEnvReadonlySubst(TypeEnv const &env, Arena &arena) :
		env(&env),
		arena(&arena) {}

	virtual GenericArg const* try_get(GenericDeductionVar var) override;
	virtual void apply_conversion(TypeConversionEvent const &event, Expr *expr) override;

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

class Unifier
{
public:
	explicit Unifier(SemaContext const &ctx) :
		m_ctx(ctx) {}

	Unifier(Unifier const &rhs) :
		m_ctx(rhs.m_ctx),
		m_left(rhs.m_left),
		m_right(rhs.m_right),
		m_conv_left(rhs.m_conv_left),
		m_conv_right(rhs.m_conv_right),
		m_err(rhs.m_err),
		m_subst(rhs.m_subst),
		m_is_swapped(rhs.m_is_swapped),
		m_expr_left(rhs.m_expr_left),
		m_expr_right(rhs.m_expr_right),
		m_result(rhs.m_result) {}

	Unifier& left(GenericArg const &arg, TypeConversion conv, Expr *expr = nullptr)
	{
		m_left = &arg;
		m_conv_left = conv;
		m_expr_left = expr;
		m_is_swapped = false;

		return *this;
	}

	Unifier& right(GenericArg const &arg, TypeConversion conv, Expr *expr = nullptr)
	{
		m_right = &arg;
		m_conv_right = conv;
		m_expr_right = expr;
		m_is_swapped = false;

		return *this;
	}

	Unifier& set(Subst *subst) { m_subst = subst; return *this; }
	Unifier& set(optional<LazyErrorMsg> const &err) { m_err = err; return *this; }
	Unifier& result(GenericArg *res) { m_result = res; return *this; }

	void go();

	SemaContext const& ctx() const { return m_ctx; }
	optional<LazyErrorMsg> err() const { return m_err; }

private:
	bool try_unify_integer_types();
	bool try_unify_type_deduction_vars();
	bool try_unify_structs();
	bool try_unify_unions();
	bool try_unify_pointers();
	bool try_unify_arrays();
	void unify_generic_values();

	Unifier sides_swapped() const
	{
		Unifier swapped(*this);
		std::swap(swapped.m_left, swapped.m_right);
		std::swap(swapped.m_conv_left, swapped.m_conv_right);
		std::swap(swapped.m_expr_left, swapped.m_expr_right);
		swapped.m_is_swapped = not m_is_swapped;

		return swapped;
	}

	GenericArg const* lookup(GenericDeductionVar var) const
	{
		assert(m_subst);
		GenericArg const *arg = m_subst->try_get(var);
		assert(arg);

		return arg;
	}

	GenericArg const* try_lookup(GenericDeductionVar var) const
	{
		if(not m_subst)
			return nullptr;

		return m_subst->try_get(var);
	}

	void emit_conversion(TypeConversionEvent const &ev, Expr *expr)
	{
		if(not m_subst or not expr)
			return;

		m_subst->apply_conversion(ev, expr);
	}

	SemaContext const &m_ctx;
	GenericArg const *m_left;
	GenericArg const *m_right;
	TypeConversion m_conv_left;
	TypeConversion m_conv_right;
	optional<LazyErrorMsg> m_err;
	Subst *m_subst = nullptr;
	bool m_is_swapped = false;

	Expr *m_expr_left = nullptr;
	Expr *m_expr_right = nullptr;
	GenericArg *m_result = nullptr;
};
