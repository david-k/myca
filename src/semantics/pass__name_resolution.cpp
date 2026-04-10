#include "semantics/context.hpp"
#include "semantics/instance_registry.hpp"
#include "semantics/module.hpp"
#include "semantics/passes.hpp"
#include "semantics/type_env.hpp"

static void resolve_generic_arg(GenericArg &arg, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx);
static void resolve_alias(AliasItem &alias, SemaContext &ctx);
static Type resolve_path_to_type(Path const &path, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx);
static Expr resolve_path_to_expr(Path const &path, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx);

static FixedArray<GenericArg>* resolve_type_args(
	FixedArray<GenericArg> *NULLABLE args,
	FixedArray<GenericParameter> *NULLABLE params,
	Scope *scope,
	ResolutionContext res_ctx,
	SemaContext &ctx
)
{
	size_t num_args = args ? args->count : 0;
	assert(num_args <= params->count);
	FixedArray<GenericArg> *resolved_type_args = alloc_fixed_array<GenericArg>(params->count, ctx.arena);

	size_t cur_idx = 0;
	if(num_args)
	{
		for(GenericArg const &arg: *args)
		{
			resolved_type_args->items[cur_idx] = arg;
			resolve_generic_arg(resolved_type_args->items[cur_idx], scope, res_ctx, ctx);
			++cur_idx;
		}
	}

	for(size_t param_idx = num_args; param_idx < params->count; ++param_idx)
	{
		GenericParameter const &param = params->items[param_idx];
		param.kind | match
		{
			[&](GenericTypeParameter const&)
			{
				GenericDeductionVar var = ctx.new_deduction_var(TypeDeductionVar());
				new (&resolved_type_args->items[cur_idx++]) GenericArg(VarType(UNKNOWN_TOKEN_RANGE, var));
			},
			[&](GenericValueParameter const &p)
			{
				GenericDeductionVar var = ctx.new_deduction_var(ValueDeductionVar(p.type));
				new (&resolved_type_args->items[cur_idx++]) GenericArg(GenericVarExpr(UNKNOWN_TOKEN_RANGE, var));
			},
		};
	}

	return resolved_type_args;
}

enum class PathSegment
{
	// This is the first segment of the complete path
	HEAD,
	// This is one of the remaining segments of the complete path
	TAIL,
};

// The lookup of the head of `path` starts at `decl_parent` (or `ambient_scope` is
// there is no `decl_parent`). The type args of `path` are always looked up in `ambient_scope`. The
// resolved type/procedure of the head of `path` becomes the `decl_parent` when resolving
// `path.child`.
//
// Example 1:
// ---------
//
//   proc foo'T() -> A.B'(T) {…}
//
// Assume we want to resolve the path `A.B'(T)`:
//
// 1. path=A.B'(T), decl_parent=foo, ambient_scope=scope_of(foo)
//    - `A` is looked up in the decl_parent `foo`
//    - `resolve_path()` is then called recursively with the remaining path and the decl_parent set to `A`
//
// 2. path=B'(T), decl_parent=A, ambient_scope=scope_of(foo)
//    - `B` is looked up in the decl_parent `A`
//    - The type argument `T` is looked up in the ambient scope `scope_of(foo)`
//
// Example 2:
// ---------
//
//   struct S {
//       struct X {}
//       x: X
//   };
//
// Assume we want to resolve the path `X` occuring in the member declaration of `x`. This consists
// of a single step:
//
// 1. path=X, decl_parent=S, ambient_scope=scope_of(S)
//    - `X` is looked up in the decl_parent `S`
static variant<Expr, Type> resolve_path(
	Path const &path,
	optional<DeclContainerInst> path_parent,
	Scope *ambient_scope,
	ResolutionContext res_ctx,
	SemaContext &ctx
)
{
	ScopeItem *resolved_item = nullptr;
	if(path_parent)
		resolved_item = &path_parent->scope()->lookup(ctx.mod->name_of(path), path.range.first, false);
	else
	{
		string_view name = ctx.mod->name_of(path);
		if(name == "?")
			name = "Option";
		else if(name == "!")
			name = "Result";

		resolved_item = &ambient_scope->lookup(name, path.range.first);
	}

	return *resolved_item | match
	{
		[&](StructItem *struct_) -> variant<Expr, Type>
		{
			if(path.type_args->count > struct_->type_params->count)
				throw_sem_error("Too many type arguments", path.range.first, ctx.mod);

			FixedArray<GenericArg> *resolved_type_args = resolve_type_args(path.type_args, struct_->type_params, ambient_scope, res_ctx, ctx);

			optional<DeclContainerInst> parent_inst = path_parent;
			if(not parent_inst and struct_->sema->decl_parent)
				parent_inst = ctx.mod->sema->insts.get_self_instance(*struct_->sema->decl_parent);

			StructInstance *inst = ctx.mod->sema->insts.get_struct_instance(struct_, resolved_type_args, parent_inst);
			return path.child ?
				resolve_path(*path.child, inst, ambient_scope, res_ctx, ctx) :
				StructType(path.range, inst);
		},
		[&](ProcItem *proc) -> variant<Expr, Type>
		{
			if(path.type_args->count > proc->type_params->count)
				throw_sem_error("Too many type arguments", path.range.first, ctx.mod);

			FixedArray<GenericArg> *resolved_type_args = resolve_type_args(path.type_args, proc->type_params, ambient_scope, res_ctx, ctx);
			ProcInstance *inst = ctx.mod->sema->insts.get_proc_instance(proc, resolved_type_args);;
			return path.child ?
				resolve_path(*path.child, inst, ambient_scope, res_ctx, ctx) :
				ProcExpr(path.range, inst);
		},
		[&](AliasItem *alias) -> variant<Expr, Type>
		{
			assert(not path_parent.has_value());

			resolve_alias(*alias, ctx);

			if(path.type_args->count > alias->type_params->count)
				throw_sem_error("Too many type arguments", path.range.first, ctx.mod);

			FixedArray<GenericArg> *resolved_type_args = resolve_type_args(path.type_args, alias->type_params, ambient_scope, res_ctx, ctx);
			TypeEnv env = TypeEnv::from_type_args(alias->type_params, resolved_type_args, ctx.mod->sema->insts);

			Type type = clone(*alias->aliased_type, ctx.arena);
			substitute_in_type(type, env, ctx.mod->sema->insts, {SubstitutionPhase::DEDUCTION, SubstitutionMode::BEST_EFFORT});

			if(path.child)
			{
				StructType const *struct_type = std::get_if<StructType>(&type);
				if(not struct_type)
					throw_sem_error("Type has no members", path.range.first, ctx.mod);

				return resolve_path(*path.child, struct_type->inst, ambient_scope, res_ctx, ctx);
			}

			return type;
		},
		[&](GenericParameter *type_param) -> variant<Expr, Type>
		{
			if(path.type_args->count)
				throw_sem_error("Cannot apply type arguments to type variable", path.range.first, ctx.mod);

			if(path.child)
				throw_sem_error("Member selection of type parameters not supported", path.child->range.first, ctx.mod);

			return type_param->kind | match
			{
				[&](GenericTypeParameter) -> variant<Expr, Type>
				{
					return VarType(path.range, GenericParameterVar(type_param));
				},
				[&](GenericValueParameter const&) -> variant<Expr, Type>
				{
					return GenericVarExpr(path.range, GenericParameterVar(type_param));
				},
			};
		},
		[&](Var const &var) -> variant<Expr, Type>
		{
			if(path_parent.has_value())
				throw_sem_error("Cannot access local variables from the outside", path.range.first, ctx.mod);

			if(path.type_args->count)
				throw_sem_error("Cannot apply type arguments to variable", path.range.first, ctx.mod);

			// Accessing local variables in a default value expression is forbidden:
			//
			//   proc foo()
			//   {
			//       let a = 3;
			//       struct S
			//       {
			//           value: i32 = a, // Causes the error below
			//       };
			//   }
			//
			// The reason is that the struct can be accessed from the outside using `foo.S` where
			// the value of the local variable is not available.
			if(res_ctx == ResolutionContext::DEFAULT_VALUE)
				throw_sem_error("Cannot access local variables in default value expression", path.range.first, ctx.mod);

			Expr result = VarExpr(path.range, &var);
			Path const *child = path.child;
			while(child)
			{
				if(child->type_args->count)
					throw_sem_error("Cannot apply type arguments to field", child->range.first, ctx.mod);

				result = MemberAccessExpr{
					.range = {result.token_range().first, child->range.last},
					.object = ctx.arena.alloc<Expr>(result),
					.member = ctx.mod->name_of(*child)
				};

				child = child->child;
			}

			return result;
		},
	};
}

static Type resolve_path_to_type(Path const &path, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	variant<Expr, Type> resolved_path = resolve_path(path, nullopt, scope, res_ctx, ctx);
	return resolved_path | match
	{
		[&](Type const &type)
		{
			return type;
		},
		[&](Expr const&) -> Type
		{
			throw_sem_error("Expected type, got expression", path.range.first, ctx.mod);
		},
	};
}

static Expr resolve_path_to_expr(Path const &path, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	variant<Expr, Type> resolved_path = resolve_path(path, nullopt, scope, res_ctx, ctx);
	return resolved_path | match
	{
		[&](Type const &type) -> Expr
		{
			Expr expr = ConstructorExpr{
				.range = path.range,
				.ctor = ctx.arena.alloc<Type>(type),
			};

			if(StructType const *struct_type = std::get_if<StructType>(&type))
			{
				if(struct_type->inst->struct_()->ctor_without_parens)
				{
					expr = CallExpr{
						.range = expr.token_range(),
						.callable = ctx.arena.alloc<Expr>(expr),
						.args = alloc_fixed_array<Argument>(0, ctx.arena),
					};
				}
			}

			return expr;
		},
		[&](Expr const &expr)
		{
			return expr;
		},
	};
}

static void resolve_generic_arg(GenericArg &arg, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	arg | match
	{
		[&](Type &t)
		{
			if(Path const *path = std::get_if<Path>(&t))
			{
				// If we encounter an identifier when parsing a GenericArg we don't know yet if the
				// identifier refers to a type or a variable. However, we still need to decide
				// whether to put the Path into a Type or Expr. What we do is to always treat the
				// Path as a Type, but if the Path later resolved to an Expr, that is fine.
				variant<Expr, Type> resolved_path = resolve_path(*path, nullopt, scope, res_ctx, ctx);
				arg = resolved_path | match
				{
					[&](Expr &e)
					{
						const_eval(e, *ctx.mod);
						return GenericArg(e);
					},
					[](Type &t) { return GenericArg(t); },
				};
			}
			else
				resolve_type(t, scope, res_ctx, ctx);
		},
		[&](Expr &e)
		{
			resolve_expr(e, scope, res_ctx, ctx);
			const_eval(e, *ctx.mod);
		},
	};
}

static void resolve_struct(StructItem &struct_, SemaContext &ctx);

void resolve_type(Type &type, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	type | match
	{
		[&](BuiltinType&) {},
		[&](KnownIntType&) {},
		[&](PointerType &t)
		{
			resolve_type(*t.pointee, scope, res_ctx, ctx);
		},
		[&](ArrayType &t)
		{
			resolve_type(*t.element, scope, res_ctx, ctx);
			if(t.count_arg)
			{
				resolve_expr(*t.count_arg, scope, res_ctx, ctx);
				const_eval(*t.count_arg, *ctx.mod);
			}
			else
			{
				assert(!"[TODO] resolve_type: ArrayType: UnknownCount");
				// TODO Create a GenericDeductionVar for the count
			}
		},
		[&](ProcTypeUnresolved &t)
		{
			for(Type &param: *t.params)
				resolve_type(param, scope, res_ctx, ctx);

			resolve_type(*t.ret, scope, res_ctx, ctx);
			assert(!"[TODO] resolve_type: ProcTypeUnresolved");
		},
		[&](UnionTypeUnresolved &t)
		{
			for(Type &alt: *t.alternatives)
				resolve_type(alt, scope, res_ctx, ctx);

			vector<Type*> canonical_alts = canonicalize_union_alternatives(t.alternatives, ctx.arena);
			type = UnionType{
				.range = t.range,
				.inst = ctx.mod->sema->insts.get_union_instance(std::move(canonical_alts)),
			};
		},
		[&](Path &path)
		{
			type = resolve_path_to_type(path, scope, res_ctx, ctx);
		},
		[&](InlineStructType &t)
		{
			resolve_struct(*t.struct_, ctx);

			// What should happen if you delcare a generic inline struct?
			//
			//   let x: struct X'S { v: S } = X(3);
			//
			// (Ignoring the fact that it doesn't look like a useful feature. But who the hell
			// knows.)
			//
			// What I decided to do is to rewrite the declaration of `x` as follows:
			//
			//   let x: X'(?_0) = X'(i32)(3);
			//
			// Since `struct X'S { v: S }` is a type constructor and not an actual type I
			// automatically generate GenericDeductionVars for its type arguments. After type checking,
			// the type of `x` will be deduced as `X'(i32)`.

			optional<DeclContainerInst> decl_parent_inst;
			if(t.struct_->sema->decl_parent)
				decl_parent_inst = ctx.mod->sema->insts.get_self_instance(*t.struct_->sema->decl_parent);

			FixedArray<GenericArg> *type_args = resolve_type_args(nullptr, t.struct_->type_params, scope, res_ctx, ctx);
			StructInstance *inst = ctx.mod->sema->insts.get_struct_instance(t.struct_, type_args, decl_parent_inst);

			type = StructType(t.range, inst);
		},
		[&](StructType const&) {},
		[&](ProcType const&) {},
		[&](UnionType const&) {},
		[&](VarType const&) {},
	};
}

void resolve_expr(Expr &expr, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	expr | match
	{
		[&](IntLiteralExpr const&) {},
		[&](BoolLiteralExpr const&) {},
		[&](StringLiteralExpr const&) {},
		[&](UnaryExpr const &e)
		{
			resolve_expr(*e.sub, scope, res_ctx, ctx);
		},
		[&](BinaryExpr const &e)
		{
			resolve_expr(*e.left, scope, res_ctx, ctx);
			resolve_expr(*e.right, scope, res_ctx, ctx);
		},
		[&](AddressOfExpr const &e)
		{
			resolve_expr(*e.object, scope, res_ctx, ctx);
		},
		[&](DerefExpr const &e)
		{
			resolve_expr(*e.addr, scope, res_ctx, ctx);
		},
		[&](IndexExpr const &e)
		{
			resolve_expr(*e.addr, scope, res_ctx, ctx);
			resolve_expr(*e.index, scope, res_ctx, ctx);
		},
		[&](MemberAccessExpr const &e)
		{
			resolve_expr(*e.object, scope, res_ctx, ctx);
		},
		[&](AssignmentExpr const &e)
		{
			resolve_expr(*e.lhs, scope, res_ctx, ctx);
			resolve_expr(*e.rhs, scope, res_ctx, ctx);
		},
		[&](AsExpr const &e)
		{
			resolve_expr(*e.src_expr, scope, res_ctx, ctx);
			resolve_type(*e.target_type, scope, res_ctx, ctx);
		},
		[&](ConstructorExpr const &e)
		{
			resolve_type(*e.ctor, scope, res_ctx, ctx);
		},
		[&](ProcExpr const&) {},
		[&](CallExpr const &e)
		{
			resolve_expr(*e.callable, scope, res_ctx, ctx);

			for(Argument &arg: *e.args)
				resolve_expr(arg.expr, scope, res_ctx, ctx);
		},
		[&](SizeOfExpr const &e)
		{
			resolve_type(*e.subject, scope, res_ctx, ctx);
		},
		[&](MakeExpr const &e)
		{
			resolve_expr(*e.addr, scope, res_ctx, ctx);
			resolve_expr(*e.init, scope, res_ctx, ctx);
		},
		[&](Path const &p)
		{
			expr = resolve_path_to_expr(p, scope, res_ctx, ctx);
		},
		[&](VarExpr const&) {},
		[&](GenericVarExpr const&) {},
		[&](UnionInitExpr const &e)
		{
			resolve_expr(*e.alt_expr, scope, res_ctx, ctx);
		},
	};
}

static void resolve_pattern(Pattern &pattern, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	pattern | match
	{
		[&](VarPatternUnresolved const &p)
		{
			Var *var = scope->declare_var(p.name, p.mutability, p.range);
			pattern = Pattern(VarPattern(p.range, var), pattern.provided_type);
		},
		[&](VarPattern const&) {},
		[&](DerefPattern const &p)
		{
			resolve_pattern(*p.sub, scope, res_ctx, ctx);
		},
		[&](AddressOfPattern const &p)
		{
			resolve_pattern(*p.sub, scope, res_ctx, ctx);
		},
		[&](ConstructorPattern const &p)
		{
			resolve_type(*p.ctor, scope, res_ctx, ctx);
			for(PatternArgument &arg: *p.args)
				resolve_pattern(arg.pattern, scope, res_ctx, ctx);
		},
		[&](WildcardPattern const&) {}
	};

	if(pattern.provided_type)
		resolve_type(*pattern.provided_type, scope, res_ctx, ctx);
}

void resolve_stmt(Stmt &stmt, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			// Resolve init expr before the lhs pattern so the init expr cannot access the declared
			// variable
			if(s.init_expr)
				resolve_expr(*s.init_expr, scope, res_ctx, ctx);

			resolve_pattern(*s.lhs, scope, res_ctx, ctx);
		},
		[&](ExprStmt const &s)
		{
			resolve_expr(*s.expr, scope, res_ctx, ctx);
		},
		[&](BlockStmt const &s)
		{
			for(Stmt &child_stmt: *s.stmts)
				resolve_stmt(child_stmt, s.scope, res_ctx, ctx);
		},
		[&](ReturnStmt const &s)
		{
			resolve_expr(*s.ret_expr, scope, res_ctx, ctx);
		},
		[&](IfStmt const &s)
		{
			resolve_expr(*s.condition, scope, res_ctx, ctx);
			resolve_stmt(*s.then, scope, res_ctx, ctx);
			if(s.else_)
				resolve_stmt(*s.else_, scope, res_ctx, ctx);
		},
		[&](WhileStmt const &s)
		{
			resolve_expr(*s.condition, scope, res_ctx, ctx);
			resolve_stmt(*s.body, scope, res_ctx, ctx);
		},
		[&](DeclStmt const &s)
		{
			resolve_struct(*s.item, ctx);
		},
		[&](MatchStmt const &s)
		{
			resolve_expr(*s.expr, scope, res_ctx, ctx);
			for(MatchArm &arm: *s.arms)
			{
				resolve_pattern(arm.capture, arm.scope, res_ctx, ctx);
				resolve_stmt(arm.stmt, arm.scope, res_ctx, ctx);
			}
		},
	};
}

static void resolve_param(Parameter &param, Scope *scope, SemaContext &ctx)
{
	if(param.type)
		resolve_type(*param.type, scope, ResolutionContext::DEFAULT_VALUE, ctx);

	if(Expr *default_value = param.default_value.try_get_expr())
		resolve_expr(*default_value, scope, ResolutionContext::DEFAULT_VALUE, ctx);
}

static bool is_deduction_complete(GenericArg const &arg)
{
	TermInfo info = get_term_info(arg);
	return not info.has_deduction_vars and not info.has_known_ints;
}

static void resolve_struct(StructItem &struct_, SemaContext &ctx)
{
	for(Member &member: *struct_.members)
	{
		member | match
		{
			[&](VarMember &var_member)
			{
				assert(var_member.var.type);
				resolve_param(var_member.var, struct_.sema->type_scope, ctx);
				if(not is_deduction_complete(*var_member.var.type))
				{
					// I was playing with the idea of allowing type deduction basically everywhere,
					// including struct members:
					//
					//   struct Foo'T
					//   {
					//       x: Option = Some(3),
					//   }
					//
					// Here, the type of `x` would be deduced as `Option'i32`. However, the problem
					// is that type deduction happens type checking, and type checking is done one
					// struct/proc at a time. If, for example, some other struct `Bar` is using
					// `Foo` and is type-checked before it, then the type for `x` would not be
					// deduced yet. This could be solved by lazily type-checking on demand, but then
					// I would also need to take care of cycles (though I believe
					// `check_default_value_deps()` already takes care of this?).
					//
					// This is too much trouble for a feature that was the result of asking "Can I?"
					// instead of "Should I?". Nobody is going to miss this feature.
					throw_sem_error(
						"Member type of \""s + ctx.mod->name_of(var_member.var) + "\" is incomplete",
						var_member.var.range.first,
						ctx.mod
					);
				}
			},
			[&](CaseMember case_member)
			{
				resolve_struct(*case_member.struct_, ctx);
			},
			[&](StructMember struct_member)
			{
				resolve_struct(*struct_member.struct_, ctx);
			},
		};
	}
}

static void resolve_alias(AliasItem &alias, SemaContext &ctx)
{
	if(alias.sema->resolution_state == ResolutionState::DONE)
		return;

	if(alias.sema->resolution_state == ResolutionState::IN_PROGRESS)
		throw_sem_error("Alias is defined recursively", alias.range.first, ctx.mod);

	alias.sema->resolution_state = ResolutionState::IN_PROGRESS;
	resolve_type(*alias.aliased_type, alias.sema->scope, ResolutionContext::DEFAULT_VALUE, ctx);
	if(not is_deduction_complete(*alias.aliased_type))
	{
		// Having an alias alternative like
		//
		//   typealias U = struct A'T { v: T } | Foo;
		//
		// doesn't make sense because type constructors are not first-class. What you can do instead
		// is to lift the type parameter to the typealias:
		//
		//   struct A'T { v: T }
		//   typealias U'T = A'T | Foo;
		//
		throw_sem_error("Aliased type is incomplete", alias.range.first, ctx.mod);
	}

	alias.sema->resolution_state = ResolutionState::DONE;
}

void resolve_item(TopLevelItem &item, SemaContext &ctx)
{
	item | match
	{
		[&](ProcItem &proc)
		{
			for(Parameter &param: *proc.params)
			{
				resolve_param(param, proc.sema->scope, ctx);
				if(not is_deduction_complete(*param.type))
					throw_sem_error("Parameter type is incomplete", param.type->token_range().first, ctx.mod);
			}

			resolve_type(*proc.ret_type, proc.sema->scope, ResolutionContext::GENERAL, ctx);
			if(not is_deduction_complete(*proc.ret_type))
				throw_sem_error("Return type is incomplete", proc.ret_type->token_range().first, ctx.mod);

			if(proc.body)
				resolve_stmt(*proc.body, proc.sema->scope, ResolutionContext::GENERAL, ctx);
		},
		[&](StructItem &struct_)
		{
			resolve_struct(struct_, ctx);
		},
		[&](AliasItem &alias)
		{
			resolve_alias(alias, ctx);
		},
	};
}

void resolve_names(SemaContext &ctx)
{
	for(TopLevelItem &item: to_range(ctx.mod->items.list()))
		resolve_item(item, ctx);
}
