#pragma once

#include "semantics/unification.hpp"

#include <expected>

// Semantic analysis consists of the following passes:
// 1. Name registration            (pass__name_registration.cpp)
// 2. Name resolution              (pass__name_resolution.cpp)
// 3. Typechecking                 (pass__typechecking.cpp)
// 4. Default value cycle checking (pass__default_value_checking.cpp)
// 5. Type layout computation      (pass__layout_computation.cpp)

// Executes all the passes in order
void sema(Module &mod, Arena &arena, optional<class EventLogger> &&logger);

// 1. Name registration
void register_item(TopLevelItem &item, SemaContext &ctx);
void register_items(SemaContext &ctx);

// 2. Name resolution
enum class ResolutionContext
{
	GENERAL,
	DEFAULT_VALUE,
};
void resolve_type(Type &type, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx);
void resolve_expr(Expr &expr, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx);
void resolve_item(TopLevelItem &item, SemaContext &ctx);
void resolve_names(SemaContext &ctx);

// 3. Typechecking
class ConstraintGatheringSubst : public Subst
{
public:
	ConstraintGatheringSubst(ConstraintSolver *solver, SemaContext *ctx) :
		solver(solver),
		ctx(ctx) {}

	virtual GenericArg const * try_get(GenericDeductionVar var) override;
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

	ConstraintSolver *solver;
	SemaContext *ctx;
};

struct ErrorMsg { string msg; };

std::expected<Type, ErrorMsg> get_pointee_type(
	Type const *type,
	PointerType::Kind pointer_kind,
	TokenRange range,
	Module const &mod
);
std::expected<Type, ErrorMsg> get_member_type(
	Type const *type,
	string_view member,
	TokenRange range,
	Module const &mod
);
void typecheck_generic_args(
	FixedArray<GenericArg> *args,
	FixedArray<GenericParameter> const *params,
	ConstraintGatheringSubst &subst,
	SemaContext &ctx
);
void typecheck_type(Type &type, ConstraintGatheringSubst &subst, SemaContext &ctx);
Type* typecheck_subexpr(Expr &expr, ConstraintGatheringSubst &subst, SemaContext &ctx);
Type const* typecheck_expr(Expr &expr, SemaContext &ctx);
void typecheck(SemaContext &ctx);

// 4. Default value cycle checking
void check_default_values(SemaContext const &ctx);

// 5. Type layout computation
struct MemoryLayout
{
	size_t size{};
	size_t alignment{};

	size_t extend(MemoryLayout other)
	{
		// Offset to ensure proper alignment
		size_t offset = 0;
		if(other.alignment > 0 && size % other.alignment)
			offset = other.alignment - (size % other.alignment); 

		size += offset + other.size;
		alignment = std::max(alignment, other.alignment);

		return size - other.size;
	}
};

Type const* is_optional_ptr(Type const &type);
Type const* is_optional_ptr(StructInstance const *struct_);
MemoryLayout get_layout(BuiltinTypeDef type);
MemoryLayout compute_layout(Type const &type);
void compute_type_layouts(Module &mod);
