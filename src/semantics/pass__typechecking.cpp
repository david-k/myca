#include "semantics/ast_traversal.hpp"
#include "semantics/constraint_solver.hpp"
#include "semantics/context.hpp"
#include "semantics/module.hpp"
#include "semantics/passes.hpp"
#include "semantics/type_env.hpp"
#include "semantics/type_properties.hpp"

//--------------------------------------------------------------------
// Constraint generation
//--------------------------------------------------------------------
GenericArg const* ConstraintGatheringSubst::try_get(GenericDeductionVar var)
{
	(void)var;
	return nullptr;
}

void ConstraintGatheringSubst::apply_conversion(TypeConversionEvent const &event, Expr *expr)
{
	::apply_conversion(event, expr, ctx->arena);
}

void ConstraintGatheringSubst::set(
	GenericDeductionVar var,
	TypeConversion var_conv,
	Expr *NULLABLE var_expr,
	GenericArg const &arg,
	TypeConversion arg_conv,
	Expr *NULLABLE arg_expr,
	optional<LazyErrorMsg> error_msg
)
{
	solver->add_relational_constraint(
		var,
		ConstraintEdge{
			.var_conv = var_conv,
			.var_expr = var_expr,
			.arg = arg,
			.arg_conv = arg_conv,
			.arg_expr = arg_expr,
			.arg_modifier = NoModifier(),
			.error_msg = error_msg,
		}
	);
}

static optional<size_t> find_by_name(Callable *callable, string_view name)
{
	size_t count = callable->param_count();
	for(size_t i = 0; i < count; ++i)
	{
		if(callable->param_name_at(i) == name)
			return i;
	}

	return nullopt;
}

static Parameter const* find_var_member(StructInstance *inst, string_view field)
{
	Module *mod = inst->struct_()->sema->type_scope->mod; // This looks disgusting

	for(Parameter const &var_member: inst->own_var_members())
	{
		if(mod->name_of(var_member) == field)
			return &var_member;
	}

	if(inst->variant_parent())
		return find_var_member(inst->variant_parent(), field);

	return nullptr;
}

static PointerType const* get_if_pointer_type(Type const *type, PointerType::Kind kind)
{
	PointerType const *pointer_type = std::get_if<PointerType>(type);
	if(pointer_type and pointer_type->kind == kind)
		return pointer_type;

	return nullptr;
}

static std::expected<Type, ErrorMsg> get_indexed_type(
	Type const *type,
	TokenRange range,
	Module const &mod
)
{
	if(PointerType const *pointer_type = get_if_pointer_type(type, PointerType::MANY))
		return *pointer_type->pointee;
	else if(ArrayType const *array_type = std::get_if<ArrayType>(type))
		return *array_type->element;
	else
	{
		return std::unexpected(
			ErrorMsg(mk_error_msg("Expected indexable type, got " + str(*type, mod), range.first, &mod))
		);
	}
}

std::expected<Type, ErrorMsg> get_pointee_type(
	Type const *type,
	PointerType::Kind pointer_kind,
	TokenRange range,
	Module const &mod
)
{
	if(PointerType const *pointer_type = get_if_pointer_type(type, pointer_kind))
		return *pointer_type->pointee;
	else
	{
		char const *pointer_kind_str = nullptr;
		switch(pointer_kind)
		{
			case PointerType::SINGLE: pointer_kind_str = "single-element"; break;
			case PointerType::MANY: pointer_kind_str = "multi-element"; break;
		}

		return std::unexpected(
			ErrorMsg(mk_error_msg("Expected "s + pointer_kind_str + " pointer type, got " + str(*type, mod), range.first, &mod))
		);
	}
}

std::expected<Type, ErrorMsg> get_member_type(
	Type const *type,
	string_view member,
	TokenRange range,
	Module const &mod
)
{
	(void)range;
	(void)mod;

	StructType const *struct_type = std::get_if<StructType>(type);
	if(not struct_type)
	{
		return std::unexpected(
			ErrorMsg(mk_error_msg("Expected object of struct type for member access, got " + str(*type, mod), range.first, &mod))
		);
	}

	Parameter const *var_member = find_var_member(struct_type->inst, member);
	if(not var_member)
	{
		return std::unexpected(
			ErrorMsg(mk_error_msg("`"s + struct_type->inst->struct_()->name + "` has no field named `"s + member + "`", range.first, &mod))
		);
	}

	return *var_member->type;
}

void generate_constraints_for_type(Type &type, ConstraintGatheringSubst &subst, SemaContext &ctx)
{
	auto visitor = [&](this auto &self, Type &type) -> void
	{
		type | match
		{
			[&](ArrayType &t)
			{
				Type *count_type = generate_constraints_for_expr(*t.count_arg, subst, ctx);
				auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
				{
					throw_sem_error("Array count has invalid type: " + reason, expr.token_range().first, &mod);
				};
				Unifier(ctx)
					.left(*count_type, TypeConversion::NONE)
					.right(BuiltinType(UNKNOWN_TOKEN_RANGE, BuiltinTypeDef::USIZE), TypeConversion::NONE)
					.set(&subst)
					.set(LazyErrorMsg(t.count_arg, error_msg))
					.go();

				type_visit_children(t, self);
			},
			[&](StructType &t)
			{
				t.inst->typecheck_generic_args(subst, ctx);
				type_visit_children(t, self);
			},
			[&](ProcType &t)
			{
				t.inst->typecheck(subst, ctx);
			},
			[&](UnionType &t)
			{
				t.inst->typecheck(subst, ctx);
			},
			[&](auto &t) { type_visit_children(t, self); },
		};
	};
	visitor(type);
}

void generate_constraints_for_generic_args(
	FixedArray<GenericArg> *args,
	FixedArray<GenericParameter> const *params,
	ConstraintGatheringSubst &subst,
	SemaContext &ctx
)
{
	for(auto const &[idx, generic_param]: *params | std::views::enumerate)
	{
		GenericArg &arg = args->items[idx];
		generic_param.kind | match
		{
			[&](GenericTypeParameter const&)
			{
				Type *type_arg = std::get_if<Type>(&arg);
				if(not type_arg)
					throw_sem_error("Expected type, got value", arg.token_range().first, ctx.mod);

				generate_constraints_for_type(*type_arg, subst, ctx);
			},
			[&](GenericValueParameter const &value_param)
			{
				Expr *expr_arg = std::get_if<Expr>(&arg);
				if(not expr_arg)
					throw_sem_error("Expected value, got type", arg.token_range().first, ctx.mod);

				Type *type = generate_constraints_for_expr(*expr_arg, subst, ctx);

				auto error_msg = [](Expr const &ret_expr, string const &reason, Module const &mod)
				{
					throw_sem_error("Invalid generic value: " + reason, ret_expr.token_range().first, &mod);
				};
				Unifier(ctx)
					.left(*type, TypeConversion::NONE)
					.right(*value_param.type, TypeConversion::NONE)
					.set(&subst)
					.set(LazyErrorMsg(expr_arg, error_msg))
					.go();
			},
		};
	}
}

Type* generate_constraints_for_expr(Expr &expr, ConstraintGatheringSubst &subst, SemaContext &ctx)
{
	auto res = expr | match
	{
		[&](IntLiteralExpr &e)
		{
			return e.type = ctx.arena.alloc<Type>(KnownIntType(e.value, e.value));
		},
		[&](BoolLiteralExpr &e)
		{
			return e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
		},
		[&](StringLiteralExpr &e)
		{
			ScopeItem &c_char_item = ctx.mod->sema->globals->lookup("c_char", INVALID_TOKEN_IDX);
			StructInstance *c_char = ctx.mod->sema->insts.get_struct_instance(std::get<StructItem*>(c_char_item), nullptr, nullopt);

			return e.type = ctx.arena.alloc<Type>(PointerType{
				.range = UNKNOWN_TOKEN_RANGE,
				.pointee = ctx.arena.alloc<Type>(StructType(UNKNOWN_TOKEN_RANGE, c_char)),
				.kind = PointerType::MANY,
				.mutability = IsMutable::NO,
			});
		},
		[&](UnaryExpr &e)
		{
			switch(e.op)
			{
				case UnaryOp::NOT:
				{
					e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
					Type const *sub_type = generate_constraints_for_expr(*e.sub, subst, ctx);

					auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Invalid operand for not operator: " + reason, expr.token_range().first, &mod);
					};
					Unifier(ctx)
						.left(*e.type, TypeConversion::NONE)
						.right(*sub_type, TypeConversion::NONE)
						.set(&subst)
						.set(LazyErrorMsg(e.sub, error_msg))
						.go();
				} break;

				case UnaryOp::NEG:
				{
					// Example where type_of(e.sub) is a GenericDeductionVar that is not mapped to any
					// concrete type:
					//
					//     proc foo'S() -> S {}
					//     proc bar'T(a: T, b: T) {}
					//
					//     bar(-foo(), 1)

					Type const *sub_type = generate_constraints_for_expr(*e.sub, subst, ctx);
					if(KnownIntType const *known_int = std::get_if<KnownIntType>(sub_type))
					{
						// TODO Check for overflow
						e.type = mk_known_int_type(-known_int->high, -known_int->low, ctx.arena);
					}
					else
						e.type = ctx.arena.alloc<Type>(clone(*sub_type, ctx.arena));
				} break;
			}

			return e.type;
		},
		[&](BinaryExpr &e)
		{
			switch(e.op)
			{
				case BinaryOp::ADD:
				case BinaryOp::SUB:
				case BinaryOp::MUL:
				case BinaryOp::DIV:
				{
					Type const *left_type = generate_constraints_for_expr(*e.left, subst, ctx);
					Type const *right_type = generate_constraints_for_expr(*e.right, subst, ctx);

					auto uni_error_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Invalid operands for binary operator: " + reason, expr.token_range().first, &mod);
					};
					GenericArg result;
					Unifier(ctx)
						.left(*left_type, TypeConversion::IMPLICIT_CTOR, e.left)
						.right(*right_type, TypeConversion::IMPLICIT_CTOR, e.right)
						.set(&subst)
						.set(LazyErrorMsg(&expr, uni_error_msg))
						.result(&result)
						.go();

					KnownIntType const *left_known_int = std::get_if<KnownIntType>(left_type);
					KnownIntType const *right_known_int = std::get_if<KnownIntType>(right_type);
					if(left_known_int and right_known_int)
					{
						// TODO Implement interval arithmetic
						assert(left_known_int->low == left_known_int->high);
						assert(right_known_int->low == right_known_int->high);

						Int128 result;
						switch(e.op)
						{
							// TODO Handle overflow / division by zero
							case BinaryOp::ADD: result = left_known_int->low + right_known_int->low; break;
							case BinaryOp::SUB: result = left_known_int->low - right_known_int->low; break;
							case BinaryOp::MUL: result = left_known_int->low * right_known_int->low; break;
							case BinaryOp::DIV: result = left_known_int->low / right_known_int->low; break;
							default: UNREACHABLE;
						}

						e.type = mk_known_int_type(result, result, ctx.arena);
					}
					else
					{
						// Example where type_of(e.left) and type_of(e.right) are GenericDeductionVars
						// that are not mapped to any concrete type:
						//
						//     proc foo'S() -> S {}
						//     proc bar'T(a: T, b: T) {}
						//
						//     bar(foo() + foo(), 1)
						//
						//
						// Example where type_of(e.left) and type_of(e.right) change:
						//
						//     proc foo'S() -> S {}
						//     proc bar'T(a: T, b: T, c: T) {}
						//
						//     bar(1, foo() + foo(), 4294967295)

						e.type = clone_ptr(&result.as_type(), ctx.arena);
					}
				} break;

				case BinaryOp::EQ:
				{
					Type const *left_type = generate_constraints_for_expr(*e.left, subst, ctx);
					Type const *right_type = generate_constraints_for_expr(*e.right, subst, ctx);

					auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Equality operator requires equal types: " + reason, expr.token_range().first, &mod);
					};
					Unifier(ctx)
						.left(*left_type, TypeConversion::NONE)
						.right(*right_type, TypeConversion::NONE)
						.set(&subst)
						.set(LazyErrorMsg(&expr, error_msg))
						.go();

					e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
				} break;

				case BinaryOp::LT:
				case BinaryOp::LE:
				case BinaryOp::GT:
				case BinaryOp::GE:
				{
					Type const *left_type = generate_constraints_for_expr(*e.left, subst, ctx);
					Type const *right_type = generate_constraints_for_expr(*e.right, subst, ctx);

					auto uni_error_msg = [](Expr const &expr, string const &reason, Module const &mod)
					{
						throw_sem_error("Invalid operands for comparison operator: " + reason, expr.token_range().first, &mod);
					};
					Unifier(ctx)
						.left(*left_type, TypeConversion::NONE)
						.right(*right_type, TypeConversion::NONE)
						.set(&subst)
						.set(LazyErrorMsg(&expr, uni_error_msg))
						.go();

					e.type = mk_builtin_type(BuiltinTypeDef::BOOL, ctx.arena);
				} break;
			}

			return e.type;
		},
		[&](AddressOfExpr &e) -> Type*
		{
			Type const *object_type = generate_constraints_for_expr(*e.object, subst, ctx);
			return e.type = mk_pointer_type(clone_ptr(object_type, ctx.arena), e.mutability, ctx.arena);
		},
		[&](DerefExpr &e) -> Type*
		{
			Type const *addr_type = generate_constraints_for_expr(*e.addr, subst, ctx);

			if(optional<GenericDeductionVar> pointer_var = addr_type->try_get_deduction_var())
			{
				GenericDeductionVar var = ctx.new_deduction_var(TypeDeductionVar());
				subst.solver->add_relational_constraint(var, ConstraintEdge{
					.arg = mk_generic_arg(*pointer_var),
					.arg_modifier = PointeeTypeModifier(PointerType::SINGLE),
				});
				e.type = ctx.arena.alloc<Type>(VarType(UNKNOWN_TOKEN_RANGE, var));
			}
			else
			{
				std::expected<Type, ErrorMsg> pointee = get_pointee_type(addr_type, PointerType::SINGLE, e.range, *ctx.mod);
				if(not pointee)
					throw ParseError(pointee.error().msg);

				e.type = clone_ptr(&pointee.value(), ctx.arena);
			}

			return e.type;
		},
		[&](IndexExpr &e) -> Type*
		{
			Type const *addr_type = generate_constraints_for_expr(*e.addr, subst, ctx);
			generate_constraints_for_expr(*e.index, subst, ctx);

			if(optional<GenericDeductionVar> array_var = addr_type->try_get_deduction_var())
			{
				GenericDeductionVar var = ctx.new_deduction_var(TypeDeductionVar());
				subst.solver->add_relational_constraint(var, ConstraintEdge{
					.arg = mk_generic_arg(*array_var),
					.arg_modifier = PointeeTypeModifier(PointerType::MANY),
				});
				e.type = ctx.arena.alloc<Type>(VarType(UNKNOWN_TOKEN_RANGE, var));
			}
			else
			{
				std::expected<Type, ErrorMsg> pointee = get_indexed_type(addr_type, e.range, *ctx.mod);
				if(not pointee)
					throw ParseError(pointee.error().msg);

				e.type = clone_ptr(&pointee.value(), ctx.arena);
			}

			return e.type;
		},
		[&](MemberAccessExpr &e) -> Type*
		{
			// Example where type_of(e.object) is a GenericDeductionVar:
			//
			//     struct Foo { i: i32 }
			//     proc id'T(v: T) { return v; }
			//
			//     id(Foo(3)).x

			Type const *object_type = generate_constraints_for_expr(*e.object, subst, ctx);

			if(optional<GenericDeductionVar> object_var = object_type->try_get_deduction_var())
			{
				GenericDeductionVar var = ctx.new_deduction_var(TypeDeductionVar());
				subst.solver->add_relational_constraint(var, ConstraintEdge{
					.arg = mk_generic_arg(*object_var),
					.arg_modifier = MemberTypeModifier(e.member),
				});

				e.type = ctx.arena.alloc<Type>(VarType(UNKNOWN_TOKEN_RANGE, var));
			}
			else
			{
				std::expected<Type, ErrorMsg> member_type = get_member_type(object_type, e.member, e.range, *ctx.mod);
				if(not member_type)
					throw ParseError(member_type.error().msg);

				e.type = clone_ptr(&member_type.value(), ctx.arena);
			}

			return e.type;
		},
		[&](AssignmentExpr &e) -> Type*
		{
			Type const *lhs_type = generate_constraints_for_expr(*e.lhs, subst, ctx);
			Type const *rhs_type = generate_constraints_for_expr(*e.rhs, subst, ctx);

			auto error_msg = [](Expr const &expr, string const &reason, Module const &mod)
			{
				throw_sem_error("Invalid operands for assignment operator: " + reason, expr.token_range().first, &mod);
			};
			Unifier(ctx)
				.left(*lhs_type, TypeConversion::NONE)
				.right(*rhs_type, TypeConversion::IMPLICIT_CTOR, e.rhs)
				.set(&subst)
				.set(LazyErrorMsg(&expr, error_msg))
				.go();

			return e.type = clone_ptr(lhs_type, ctx.arena);
		},
		[&](AsExpr &e) -> Type*
		{
			generate_constraints_for_type(*e.target_type, subst, ctx);
			generate_constraints_for_expr(*e.src_expr, subst, ctx);
			return e.type = clone_ptr(e.target_type, ctx.arena);
		},
		[&](ConstructorExpr &e)
		{
			StructType const *struct_type = std::get_if<StructType>(e.ctor);
			if(not struct_type)
				throw_sem_error("Expected struct type", e.range.first, ctx.mod);

			Type const *ctor_type = struct_type->inst->try_get_ctor_type();
			if(not ctor_type)
				throw_sem_error("Struct does not provide a constructor", e.range.first, ctx.mod);

			return e.type = clone_ptr(ctor_type, ctx.arena);
		},
		[&](ProcExpr &e)
		{
			return e.type = clone_ptr(e.inst->get_type(), ctx.arena);
		},
		[&](CallExpr &e)
		{
			Type *callable_type = generate_constraints_for_expr(*e.callable, subst, ctx);
			ProcType *proc_type = std::get_if<ProcType>(callable_type);
			if(not proc_type)
				throw_sem_error("Expected callable expression", e.range.first, ctx.mod);

			proc_type->inst->typecheck(subst, ctx);
			proc_type->callable->typecheck_generic_args(subst, ctx);

			FixedArray<Type> const *param_types = proc_type->inst->params;
			if(e.args->count > param_types->count)
				throw_sem_error("Too many arguments", e.range.first, ctx.mod);

			auto arg_error_msg = [](Expr const &arg, string const &reason, Module const &mod)
			{
				throw_sem_error("Invalid argument: " + reason, arg.token_range().first, &mod);
			};

			Callable *callable = proc_type->callable;
			unordered_set<int> assigned_params;
			bool has_unordered_named_args = false;
			for(size_t i = 0; i < e.args->count; ++i)
			{
				Argument &arg = e.args->items[i];

				// Find the corresponding parameter depending on whether the argument is named or not
				if(arg.name.size())
				{
					if(optional<size_t> param_idx_opt = find_by_name(callable, arg.name))
					{
						arg.param_idx = *param_idx_opt;
						if(*param_idx_opt != i)
							has_unordered_named_args = true;
					}
					else
						throw_sem_error("Invalid parameter name `"s + arg.name + "`", arg.range.first, ctx.mod);
				}
				else
				{
					if(has_unordered_named_args)
						throw_sem_error("Positional arguments must come before named ones", arg.range.first, ctx.mod);

					arg.param_idx = i;
				}

				auto res = assigned_params.insert(arg.param_idx);
				if(!res.second)
					throw_sem_error("Multiple arguments for same parameter", arg.range.first, ctx.mod);

				Type const &param_type = param_types->items[arg.param_idx];
				Type *arg_type = generate_constraints_for_expr(arg.expr, subst, ctx);
				Unifier(ctx)
					.left(param_type, TypeConversion::NONE)
					.right(*arg_type, TypeConversion::IMPLICIT_CTOR, &arg.expr)
					.set(&subst)
					.set(LazyErrorMsg(&arg.expr, arg_error_msg))
					.go();
			}

			for(size_t param_idx = 0; param_idx < param_types->count; ++param_idx)
			{
				if(!assigned_params.contains(param_idx))
				{
					if(not callable->param_default_value_at(param_idx))
						throw_sem_error("Missing argument for parameter "s + callable->param_name_at(param_idx), e.range.first, ctx.mod);
				}
			}

			return e.type = clone_ptr(proc_type->inst->ret, ctx.arena);
		},
		[&](SizeOfExpr &e)
		{
			return e.type = mk_builtin_type(BuiltinTypeDef::USIZE, ctx.arena);
		},
		[&](MakeExpr&)
		{
			assert(!"[TODO] typecheck_subexpr: MakeExpr");
			return (Type*)nullptr;
		},
		[&](UnionInitExpr &e)
		{
			return e.type;
		},
		[&](VarExpr &e)
		{
			assert(e.var->type && "typecheck_subexpr: VarExpr: Var::type is null");
			return e.type = clone_ptr(e.var->type, ctx.arena);
		},
		[&](GenericVarExpr &e) -> Type*
		{
			e.type = e.var | match
			{
				[&](GenericParameterVar const &v)
				{
					return v.def->kind | match
					{
						[&](GenericTypeParameter const&) -> Type*
						{
							assert(!"typecheck_subexpr: GenericVarExpr: GenericTypeParameter");
						},
						[&](GenericValueParameter const &p) -> Type*
						{
							return clone_ptr(p.type, ctx.arena);
						},
					};
				},
				[&](GenericDeductionVar const &v)
				{
					return v.def->kind | match
					{
						[&](TypeDeductionVar const&) -> Type*
						{
							assert(!"typecheck_subexpr: GenericVarExpr: TypeDeductionVar");
						},
						[&](ValueDeductionVar const &v) -> Type*
						{
							return clone_ptr(v.type, ctx.arena);
						},
					};
				},
			};

			return e.type;
		},
		[&](Path&) -> Type* { assert(!"typecheck_subexpr: Path"); },
	};

	return res;
}

// Checks whether pattern_type <c rhs_type, where <c refers to the subtype relation induced by the
// provided TypeConversion.
//
// Attention: Does not take lhs_pattern.provided_type into account.
static Type const* unify_pattern(
	Pattern &lhs_pattern,
	Type const &rhs_type,
	TypeConversion lhs_conv,
	TypeConversion rhs_conv,
	ConstraintSolver &solver,
	SemaContext &ctx
)
{
	return lhs_pattern | match
	{
		[&](VarPattern &p)
		{
			p.var->type = clone_ptr(&rhs_type, ctx.arena);
			return p.type = clone_ptr(p.var->type, ctx.arena);
		},
		[&](DerefPattern &p)
		{
			PointerType const *pointer_type = std::get_if<PointerType>(&rhs_type);
			if(not pointer_type)
				throw_sem_error("Invalid deref pattern: target type not a pointer", p.range.first, ctx.mod);

			Type const *sub_type = unify_pattern(
				*p.sub,
				*pointer_type->pointee,
				lhs_conv,
				rhs_conv,
				solver,
				ctx
			);
			return p.type = clone_ptr(sub_type, ctx.arena);
		},
		[&](AddressOfPattern &p)
		{
			Type sub_rhs_type(PointerType{
				.range = UNKNOWN_TOKEN_RANGE,
				.pointee = clone_ptr(&rhs_type, ctx.arena),
				.kind = PointerType::SINGLE,
				.mutability = p.mutability,
			});

			Type const *sub_type = unify_pattern(
				*p.sub,
				sub_rhs_type,
				lhs_conv,
				rhs_conv,
				solver,
				ctx
			);

			return p.type = clone_ptr(sub_type, ctx.arena);
		},
		[&](ConstructorPattern &p) -> Type*
		{
			LazyErrorMsg error_msg(&lhs_pattern, [](Pattern const &pattern, string const &reason, Module const &mod)
			{
				throw_sem_error("Invalid constructor pattern: " + reason, pattern.token_range().first, &mod);
			});
			ConstraintGatheringSubst subst(&solver, &ctx);
			Unifier(ctx)
				.left(*p.ctor, lhs_conv)
				.right(rhs_type, rhs_conv)
				.set(&subst)
				.set(error_msg)
				.go();

			// Make sure the constructor is not a type variable
			if(VarType const *var = std::get_if<VarType>(p.ctor))
			{
				var->var | match
				{
					[&](GenericParameterVar)
					{
						throw_sem_error("Type parameters cannot be used as constructor patterns", var->range.first, ctx.mod);
					},
					[&](GenericDeductionVar)
					{
						assert(!"Constructor pattern is GenericDeductionVar");
					},
				};
			}

			// Validate constructor
			StructType const *ctor = std::get_if<StructType>(p.ctor);
			if(ctor)
			{
				if(p.has_parens and (ctor->inst->struct_()->ctor_without_parens or not ctor->inst->try_get_ctor_type()))
					throw_sem_error("Invalid parentheses after type in pattern", p.range.first, ctx.mod);

				// If no parens are given then we only match on the type
				if(p.has_parens and ctor->inst->try_get_ctor_type())
				{
					ProcType const &ctor_proc_type = std::get<ProcType>(*ctor->inst->try_get_ctor_type());
					if(p.args->count > ctor_proc_type.inst->params->count)
						throw_sem_error("Too many arguments provided for constructor in pattern", p.range.first, ctx.mod);

					if(p.args->count < ctor_proc_type.inst->params->count)
						throw_sem_error("TODO: Support default arguments in patterns", p.range.first, ctx.mod);

					for(size_t i = 0; i < p.args->count; ++i)
					{
						PatternArgument &arg = p.args->items[i];

						Type param_type = clone(ctor_proc_type.inst->params->items[i], ctx.arena);

						if(arg.param_name.size())
							throw_sem_error("TODO: Support named arguments in constructor pattern", p.range.first, ctx.mod);

						unify_pattern(
							arg.pattern,
							param_type,
							lhs_conv,
							rhs_conv,
							solver,
							ctx
						);
						arg.param_idx = i;
					}
				}
			}

			return p.type = clone_ptr(p.ctor, ctx.arena);
		},
		[&](WildcardPattern &p) -> Type*
		{
			return p.type = clone_ptr(&rhs_type, ctx.arena);
		},
		[&](VarPatternUnresolved&) -> Type* { assert(!"unify_pattern: VarPatternUnresolved"); },
	};
}

static Type const* generate_constraints_for_pattern(
	Pattern &lhs_pattern,
	Type const &rhs_type,
	bool irrefutable_pattern_required,
	ConstraintGatheringSubst &subst,
	SemaContext &ctx,
	Expr *root_rhs_expr
)
{
	if(lhs_pattern.provided_type)
	{
		// If pattern must be irrefutable:
		//   1. rhs_type <: provided_type
		// Otherwise:
		//   1. provided_type <: rhs_type
		//
		// In both cases:
		//   2. pattern_type <: provided_type

		generate_constraints_for_type(*lhs_pattern.provided_type, subst, ctx);

		auto error_msg = [](Pattern const &pattern, string const &reason, Module const &mod)
		{
			throw_sem_error(
				"Inferred type does not match specified type in let statement: " + reason,
				pattern.token_range().first,
				&mod
			);
		};

		if(irrefutable_pattern_required)
		{
			Unifier(ctx)
				.left(*lhs_pattern.provided_type, TypeConversion::NONE)
				.right(rhs_type, TypeConversion::IMPLICIT_CTOR, root_rhs_expr)
				.set(&subst)
				.set(LazyErrorMsg(&lhs_pattern, error_msg))
				.go();
		}
		else
		{
			Unifier(ctx)
				.left(rhs_type, TypeConversion::NONE)
				.right(*lhs_pattern.provided_type, TypeConversion::IMPLICIT_CTOR)
				.set(&subst)
				.set(LazyErrorMsg(&lhs_pattern, error_msg))
				.go();
		}

		return unify_pattern(
			lhs_pattern,
			*lhs_pattern.provided_type,
			TypeConversion::TRIVIAL, TypeConversion::NONE,
			*subst.solver,
			ctx
		);
	}
	else
	{
		// If pattern must be irrefutable:
		//   rhs_type <: pattern_type
		// Otherwise:
		//   pattern_type <: rhs_type

		if(irrefutable_pattern_required)
		{
			return unify_pattern(
				lhs_pattern,
				rhs_type,
				TypeConversion::NONE,
				TypeConversion::IMPLICIT_CTOR,
				*subst.solver,
				ctx
			);
		}
		else
		{
			return unify_pattern(
				lhs_pattern,
				rhs_type,
				TypeConversion::IMPLICIT_CTOR,
				TypeConversion::NONE,
				*subst.solver,
				ctx
			);
		}
	}
}

//--------------------------------------------------------------------
// Type checking
//--------------------------------------------------------------------
static void require_lvalue(optional<IsMutable> object_mutability, IsMutable required_mutability, TokenRange range, Module const &mod)
{
	if(not object_mutability)
		throw_sem_error("Address-of expression requires lvalue", range.first, &mod);

	if(required_mutability == IsMutable::YES and object_mutability == IsMutable::NO)
		throw_sem_error("Cannot make mutable reference to const object", range.first, &mod);
}

// Performs typechecks after type deduction is complete
struct DeferredTypechecker
{
	void operator () (Expr const &expr)
	{
		expr | match
		{
			[&](UnaryExpr const &e)
			{
				switch(e.op)
				{
					case UnaryOp::NOT: break;
					case UnaryOp::NEG:
					{
						if(not is_integer_type(e.sub->type()))
							throw_sem_error("Expected integer, got " + str(*e.type, *ctx.mod), e.range.first, ctx.mod);
					} break;
				}
			},
			[&](BinaryExpr const &e)
			{
				switch(e.op)
				{
					case BinaryOp::EQ:
						break;

					case BinaryOp::ADD:
					case BinaryOp::SUB:
					case BinaryOp::MUL:
					case BinaryOp::DIV:
					case BinaryOp::LT:
					case BinaryOp::LE:
					case BinaryOp::GT:
					case BinaryOp::GE:
					{
						if(not is_integer_type(e.left->type()))
							throw_sem_error("Invalid type for binary operator: Expected integer, got " + str(*e.type, *ctx.mod), e.left->token_range().first, ctx.mod);

						if(not is_integer_type(e.right->type()))
							throw_sem_error("Invalid type for binary operator: Expected integer, got " + str(*e.type, *ctx.mod), e.right->token_range().first, ctx.mod);
					} break;
				}
			},
			[&](AddressOfExpr const &e)
			{
				optional<IsMutable> object_mutability = lvalue_mutability(*e.object);
				require_lvalue(object_mutability, e.mutability, e.object->token_range(), *ctx.mod);
			},
			[&](IndexExpr const &e)
			{
				if(not is_integer_type(e.index->type()))
					throw_sem_error("Expected integer, got " + str(*e.type, *ctx.mod), e.index->token_range().first, ctx.mod);
			},
			[&](AssignmentExpr const &e)
			{
				optional<IsMutable> lhs_mutability = lvalue_mutability(*e.lhs);
				require_lvalue(lhs_mutability, IsMutable::YES, e.lhs->token_range(), *ctx.mod);
			},
			[&](AsExpr const &e)
			{
				if(not is_cast_ok(*e.target_type, *e.src_expr, ctx))
					throw_sem_error("Invalid cast", e.src_expr->token_range().first, ctx.mod);
			},
			[](const auto&) {}
		};

		expr_visit_children(expr, *this);
	}

	void operator () (Type const &type)
	{
		type | match
		{
			[&](ArrayType const &t)
			{
				if(IntLiteralExpr const *count_val = std::get_if<IntLiteralExpr>(t.count_arg))
				{
					if(count_val->value < 1)
						throw_sem_error("Array count must be strictly larger that zero", t.count_arg->token_range().first, ctx.mod);
				}
				else if(not ctx.well_formed_terms.contains(*t.count_arg))
					throw_sem_error("Array count cannot be determined to be valid: " + str(*t.count_arg, *ctx.mod), t.count_arg->token_range().first, ctx.mod);
			},
			[](const auto&) {}
		};

		type_visit_children(type, *this);
	}

	void operator () (Pattern const &pattern)
	{
		pattern | match
		{
			[&](AddressOfPattern const &p)
			{
				require_lvalue(pattern_rhs_mutability, p.mutability, p.range, *ctx.mod);
			},
			[](const auto&) {},
		};

		pattern_visit_children(pattern, *this);
	}

	SemaContext &ctx;
	optional<IsMutable> pattern_rhs_mutability = nullopt;
};

void solve_and_check(ConstraintSolver &solver, std::initializer_list<ApplyTarget> targets)
{
	LOGGER(solver.ctx().logger(), on_data, solver);

	TypeEnv env = solver.solve();
	LOGGER(solver.ctx().logger(), on_data, env);

	for(ApplyTarget target: targets)
	{
		target | match
		{
			[&](Type *type)
			{
				substitute_in_type(*type, env, solver.ctx().mod->sema->insts, {
					SubstitutionPhase::DEDUCTION,
					SubstitutionMode::FULL,
					type->token_range()
				});
				DeferredTypechecker{solver.ctx()}(*type);
			},
			[&](Expr *expr)
			{
				substitute_in_expr(*expr, env, solver.ctx().mod->sema->insts, {
					SubstitutionPhase::DEDUCTION,
					SubstitutionMode::FULL,
					expr->token_range()
				});
				DeferredTypechecker{solver.ctx()}(*expr);
			},
			[&](std::pair<Pattern*, Expr const*> p)
			{
				Pattern *pattern = p.first;
				Expr const *rhs_expr = p.second;
				optional<IsMutable> rhs_mutability = lvalue_mutability(*rhs_expr);

				substitute_in_pattern(*pattern, env, solver.ctx().mod->sema->insts, {
					SubstitutionPhase::DEDUCTION,
					SubstitutionMode::FULL,
					pattern->token_range()
				});
				DeferredTypechecker{solver.ctx(), rhs_mutability}(*pattern);
			},
		};
	}
}
Type const* typecheck_expr(Expr &expr, SemaContext &ctx)
{
	LOGGER(ctx.logger(), on_expr_start, expr);

	ConstraintSolver solver(ctx);
	ConstraintGatheringSubst constraint_gatherer(&solver, &ctx);
	Type const *type = generate_constraints_for_expr(expr, constraint_gatherer, ctx);
	solve_and_check(solver, {&expr});

	LOGGER(ctx.logger(), on_expr_end);
	return type;
}

static void typecheck_param(Parameter &param, SemaContext &ctx)
{
	ConstraintSolver solver(ctx);
	ConstraintGatheringSubst subst(&solver, &ctx);
	generate_constraints_for_type(*param.type, subst, ctx);

	if(Expr *default_value = param.default_value.try_get_expr())
	{
		LOGGER(ctx.logger(), on_expr_start, *default_value);

		Type const *default_value_type = generate_constraints_for_expr(*default_value, subst, ctx);
		auto error_msg = [](Expr const &init_expr, string const &reason, Module const &mod)
		{
			throw_sem_error("Invalid default value: " + reason, init_expr.token_range().first, &mod);
		};

		Unifier(ctx)
			.left(*param.type, TypeConversion::NONE)
			.right(*default_value_type, TypeConversion::IMPLICIT_CTOR, default_value)
			.set(&subst)
			.set(LazyErrorMsg(default_value, error_msg))
			.go();

		solve_and_check(solver, {param.type, default_value});

		LOGGER(ctx.logger(), on_expr_end);
	}
	else
		solve_and_check(solver, {param.type});
}

static void typecheck_struct(StructItem *struct_, SemaContext &ctx)
{
	// TODO Do something smarter than making a copy
	WellFormedTermSet backup = ctx.well_formed_terms;
	gather_well_formed_terms(*struct_, ctx.well_formed_terms);
	for(Member &m: *struct_->members)
	{
		m | match
		{
			[&](VarMember &var_member) { typecheck_param(var_member.var, ctx); },
			[&](CaseMember case_member) { typecheck_struct(case_member.struct_, ctx); },
			[&](StructMember struct_member) { typecheck_struct(struct_member.struct_, ctx); },
		};
	}
	ctx.well_formed_terms = backup;
}

static void typecheck_stmt(Stmt &stmt, SemaContext &ctx)
{
	LOGGER(ctx.logger(), on_stmt_start, stmt);
	stmt | match
	{
		[&](LetStmt const &s)
		{
			if(s.init_expr)
			{
				LOGGER(ctx.logger(), on_expr_start, *s.init_expr);

				ConstraintSolver solver(ctx);
				ConstraintGatheringSubst constraint_gatherer(&solver, &ctx);
				Type const *init_type = generate_constraints_for_expr(*s.init_expr, constraint_gatherer, ctx);
				generate_constraints_for_pattern(*s.lhs, *init_type, true, constraint_gatherer, ctx, s.init_expr);
				solve_and_check(solver, {s.init_expr, std::pair{s.lhs, s.init_expr}});

				LOGGER(ctx.logger(), on_expr_end);
			}
			else
			{
				assert(!"[TODO] typecheck_stmt: LetStmt: no init expr");
			}
		},
		[&](ExprStmt const &s)
		{
			typecheck_expr(*s.expr, ctx);
		},
		[&](BlockStmt const &s)
		{
			for(Stmt &child_stmt: *s.stmts)
				typecheck_stmt(child_stmt, ctx);
		},
		[&](ReturnStmt const &s)
		{
			if(s.ret_expr)
			{
				LOGGER(ctx.logger(), on_expr_start, *s.ret_expr);

				auto error_msg = [](Expr const &ret_expr, string const &reason, Module const &mod)
				{
					throw_sem_error("Invalid return expression: " + reason, ret_expr.token_range().first, &mod);
				};

				ConstraintSolver solver(ctx);
				ConstraintGatheringSubst constraint_gatherer(&solver, &ctx);
				Type const *ret_expr_type = generate_constraints_for_expr(*s.ret_expr, constraint_gatherer, ctx);
				Unifier(ctx)
					.left(*ctx.proc->ret_type, TypeConversion::NONE)
					.right(*ret_expr_type, TypeConversion::IMPLICIT_CTOR, s.ret_expr)
					.set(&constraint_gatherer)
					.set(LazyErrorMsg(s.ret_expr, error_msg))
					.go();

				solve_and_check(solver, {s.ret_expr});

				LOGGER(ctx.logger(), on_expr_end);
			}
			else
			{
				if(not is_builtin_type(*ctx.proc->ret_type, BuiltinTypeDef::UNIT))
					throw_sem_error("Return statement must return unit value", s.range.first, ctx.mod);
			}
		},
		[&](IfStmt const &s)
		{
			Type const *condition_type = typecheck_expr(*s.condition, ctx);
			if(not is_builtin_type(*condition_type, BuiltinTypeDef::BOOL))
				throw_sem_error("If-condition must be boolean", s.range.first, ctx.mod);

			typecheck_stmt(*s.then, ctx);
			if(s.else_)
				typecheck_stmt(*s.else_, ctx);
		},
		[&](WhileStmt const &s)
		{
			Type const *condition_type = typecheck_expr(*s.condition, ctx);
			if(not is_builtin_type(*condition_type, BuiltinTypeDef::BOOL))
				throw_sem_error("While-condition must be boolean", s.range.first, ctx.mod);

			typecheck_stmt(*s.body, ctx);
		},
		[&](DeclStmt const &s)
		{
			typecheck_struct(s.item, ctx);
		},
		[&](MatchStmt const &s)
		{
			Type const *expr_type = typecheck_expr(*s.expr, ctx);
			*expr_type | match
			{
				[&](StructType const &struct_type)
				{
					assert(struct_type.inst->is_deduction_complete());

					StructInstance *subject = struct_type.inst;
					unordered_set<StructInstance const*> matched_cases;
					bool has_wildcard = false;
					for(MatchArm &arm: *s.arms)
					{
						if(has_wildcard)
							throw_sem_error("Pattern is following wildcard pattern and is therefore unreachable", arm.capture.token_range().first, ctx.mod);

						ConstraintSolver arm_constraints(ctx);
						ConstraintGatheringSubst subst(&arm_constraints, &ctx);
						generate_constraints_for_pattern(arm.capture, s.expr->type(), false, subst, ctx, s.expr);
						solve_and_check(arm_constraints, {std::pair{&arm.capture, s.expr}});

						if(is<WildcardPattern>(arm.capture))
							has_wildcard = true;
						else
						{
							StructType *arm_struct_type = std::get_if<StructType>(&arm.capture.type());
							if(not arm_struct_type)
								throw_sem_error("Must match against a case member", arm.capture.token_range().first, ctx.mod);

							StructInstance *arm_inst = arm_struct_type->inst;
							if(arm_inst->variant_parent() != subject)
								throw_sem_error("Must match against a case member", arm.capture.token_range().first, ctx.mod);

							if(!matched_cases.insert(arm_inst).second)
								throw_sem_error("Duplicate case value", arm.capture.token_range().first, ctx.mod);

							arm.discr = arm_inst->case_idx();
						}

						typecheck_stmt(arm.stmt, ctx);
					}

					if((int)matched_cases.size() != subject->struct_()->num_case_members and not has_wildcard)
						throw_sem_error("Match is not exhaustive", s.range.first, ctx.mod);
				},
				[&](UnionType const &union_type)
				{
					assert(union_type.inst->is_deduction_complete());

					UnionInstance *subject = union_type.inst;
					if(subject->occurring_vars().size())
					{
						// TODO
						// The reason for this current restriction is that we cannot know if the
						// alternatives of the union are actually distinct if type variables are
						// involved.
						//
						// For example, consider the following function:
						//
						//     proc foo'(S,T)(c: S | T)
						//     {
						//         match c {
						//             case let v: S {
						//                 ...
						//             }
						//             case let v: T {
						//                 ...
						//             }
						//         }
						//     }
						//
						// If foo is instantiated such that S and T refer to the same type, then
						// it's unclear which of the two case-clauses to keep.
						//
						// I think a good solution would be to require that the alternatives
						// mentioned in the match statement must be distinct under all possible
						// substitutions. (Note that this requirement is only necessary for the
						// alternatives actually mentioned in the match statement, not for all
						// alternatives of the union.)
						throw_sem_error(
							"Matching on a union that depends on type variables is not supported yet",
							s.expr->token_range().first,
							ctx.mod
						);
					}

					vector<bool> matched_alts(subject->alternatives().size(), false);
					size_t num_matched_alts = 0;
					bool has_wildcard = false;
					for(MatchArm &arm: *s.arms)
					{
						if(has_wildcard)
						{
							throw_sem_error(
								"Pattern is following wildcard pattern and is therefore unreachable",
								arm.capture.token_range().first,
								ctx.mod
							);
						}

						ConstraintSolver arm_constraints(ctx);
						ConstraintGatheringSubst subst(&arm_constraints, &ctx);
						generate_constraints_for_pattern(arm.capture, s.expr->type(), false, subst, ctx, s.expr);
						solve_and_check(arm_constraints, {std::pair{&arm.capture, s.expr}});

						if(is<WildcardPattern>(arm.capture))
							has_wildcard = true;
						else
						{
							optional<size_t> alt_idx = subject->try_get_alt_idx(arm.capture.type());
							if(not alt_idx)
								throw_sem_error("Must match against an alternative of the union", arm.capture.token_range().first, ctx.mod);

							if(matched_alts[*alt_idx])
								throw_sem_error("Duplicate case value", arm.capture.token_range().first, ctx.mod);

							matched_alts[*alt_idx] = true;
							num_matched_alts += 1;
							arm.discr = *alt_idx;
						}

						typecheck_stmt(arm.stmt, ctx);
					}

					if(num_matched_alts != subject->alternatives().size() and not has_wildcard)
						throw_sem_error("Match is not exhaustive", s.range.first, ctx.mod);
				},
				[&](auto const&)
				{
					throw_sem_error("The match subject must refer to a struct or union type", s.expr->token_range().first, ctx.mod);
				},
			};
		},
	};
	LOGGER(ctx.logger(), on_stmt_end);
}

void typecheck(SemaContext &ctx)
{
	for(TopLevelItem &item: to_range(ctx.mod->items.list()))
	{
		item | match
		{
			[&](ProcItem &proc)
			{
				LOGGER(ctx.logger(), on_proc_start, &proc);
				ctx.proc = &proc;
				gather_well_formed_terms(proc, ctx.well_formed_terms);

				for(auto const &[idx, param]: *proc.params | std::views::enumerate)
				{
					typecheck_param(param, ctx);
					proc.sema->param_vars->items[idx]->type = param.type;
				}

				ConstraintSolver solver(ctx);
				ConstraintGatheringSubst subst(&solver, &ctx);
				generate_constraints_for_type(*proc.ret_type, subst, ctx);
				solve_and_check(solver, {proc.ret_type});

				if(proc.body)
					typecheck_stmt(*proc.body, ctx);

				ctx.well_formed_terms.clear();
				ctx.proc = nullptr;
				LOGGER(ctx.logger(), on_proc_end);
			},
			[&](StructItem &struct_)
			{
				typecheck_struct(&struct_, ctx);
			},
			[&](AliasItem &alias)
			{
				ConstraintSolver solver(ctx);
				ConstraintGatheringSubst subst(&solver, &ctx);
				generate_constraints_for_type(*alias.aliased_type, subst, ctx);
				solve_and_check(solver, {alias.aliased_type});
			},
		};
	}

	for_each_struct_instance(ctx.mod->sema->insts, [&](StructInstance *struct_)
	{
		struct_->finalize_typechecking();
	});
}
