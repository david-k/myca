#include "semantics/context.hpp"
#include "semantics/module.hpp"
#include "semantics/passes.hpp"

static void register_struct_item(StructItem *struct_, optional<DeclContainer> decl_parent, Scope *scope, SemaContext &ctx);

static void register_types(Type const &type, Scope *scope, optional<DeclContainer> decl_parent, SemaContext &ctx)
{
	type | match
	{
		[&](BuiltinType const&) {},
		[&](KnownIntType const&) {},
		[&](PointerType const &t)
		{
			register_types(*t.pointee, scope, decl_parent, ctx);
		},
		[&](ArrayType const &t)
		{
			register_types(*t.element, scope, decl_parent, ctx);
		},
		[&](ProcTypeUnresolved const &t)
		{
			for(ProcTypeParameter const &param: *t.params)
				register_types(param.type, scope, decl_parent, ctx);

			register_types(*t.ret, scope, decl_parent, ctx);
		},
		[&](UnionTypeUnresolved const &t)
		{
			for(Type &alt: *t.alternatives)
				register_types(alt, scope, decl_parent, ctx);
		},
		[&](Path const&) {},
		[&](InlineStructType const &t)
		{
			register_struct_item(t.struct_, decl_parent, scope, ctx);
		},
		[&](StructType const&) {},
		[&](ProcType const&) {},
		[&](UnionType const&) {},
		[&](VarType const&) {},
	};
}

static void register_types_in_pattern(Pattern &pattern, Scope *scope, optional<DeclContainer> decl_parent, SemaContext &ctx)
{
	if(pattern.provided_type)
		register_types(*pattern.provided_type, scope, decl_parent, ctx);
}

static void register_types_in_stmt(Stmt &stmt, Scope *scope, optional<DeclContainer> decl_parent, SemaContext &ctx)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			register_types_in_pattern(*s.lhs, scope, decl_parent, ctx);
		},
		[&](ExprStmt const&) {},
		[&](BlockStmt &s)
		{
			// For example, in register_item(), the scope of a proc's BlockStmt is set to the scope
			// of the proc
			if(not s.scope)
				s.scope = scope->new_child(true);

			for(Stmt &child_stmt: *s.stmts)
				register_types_in_stmt(child_stmt, s.scope, decl_parent, ctx);
		},
		[&](ReturnStmt const&) {},
		[&](IfStmt const &s)
		{
			register_types_in_stmt(*s.then, scope, decl_parent, ctx);
			if(s.else_)
				register_types_in_stmt(*s.else_, scope, decl_parent, ctx);
		},
		[&](WhileStmt const &s)
		{
			register_types_in_stmt(*s.body, scope, decl_parent, ctx);
		},
		[&](DeclStmt const &s)
		{
			register_struct_item(s.item, decl_parent, scope, ctx);
		},
		[&](MatchStmt &s)
		{
			for(MatchArm &arm: *s.arms)
			{
				arm.scope = scope->new_child(true);
				register_types_in_stmt(arm.stmt, arm.scope, decl_parent, ctx);
			}
		},
	};
}

static std::generator<Parameter&> initial_var_members(StructItem *struct_)
{
	optional<int> next_idx = struct_->first_initial_var_member_idx;
	while(next_idx)
	{
		VarMember &var_member = std::get<VarMember>(struct_->members->items[*next_idx]);
		co_yield var_member.var;
		next_idx = var_member.next_var_member_idx;
	}
}

static std::generator<Parameter&> trailing_var_members(StructItem *struct_)
{
	optional<int> next_idx = struct_->first_trailing_var_member_idx;
	while(next_idx)
	{
		VarMember &var_member = std::get<VarMember>(struct_->members->items[*next_idx]);
		co_yield var_member.var;
		next_idx = var_member.next_var_member_idx;
	}
}

static std::generator<Parameter&> all_var_members(StructItem *struct_)
{
	if(struct_->is_case_member)
		co_yield std::ranges::elements_of(initial_var_members(struct_->sema->decl_parent->as_struct()));

	co_yield std::ranges::elements_of(initial_var_members(struct_));
	co_yield std::ranges::elements_of(trailing_var_members(struct_));

	if(struct_->is_case_member)
		co_yield std::ranges::elements_of(trailing_var_members(struct_->sema->decl_parent->as_struct()));
}

static void register_struct_item(StructItem *struct_, optional<DeclContainer> decl_parent, Scope *scope, SemaContext &ctx)
{
	assert(not struct_->ctor_without_parens or struct_->members->count == 0);

	scope->declare_struct(struct_);
	struct_->sema = ctx.arena.alloc<Struct>(decl_parent, scope->new_child(true));
	if(struct_->is_case_member)
		struct_->sema->variant_depth = decl_parent->as_struct()->sema->variant_depth + 1;

	// Declare type parameters
	size_t num_type_params = struct_->type_params->count;
	for(size_t i = 0; i < num_type_params; ++i)
	{
		GenericParameter &type_param = struct_->type_params->items[i];
		// TODO Produce error if the type parameter has the same name as the struct
		struct_->sema->type_scope->declare_type_var(&type_param);
	}

	// Perform some checks on variable and case members:
	// - Check that they each have a unique name
	// - Check that there are no variable members between case members
	// - Check that case members that have been declared without braces indeed have no members, not
	//   even inherited ones
	enum MemberListState
	{
		INITIAL_VAR_MEMBERS,
		CASE_MEMBERS,
		TRAILING_VAR_MEMBERS,
	};

	std::unordered_set<string_view> member_names;
	MemberListState state = INITIAL_VAR_MEMBERS;
	for(Member const &member: *struct_->members)
	{
		member | match
		{
			[&](VarMember const &var_member)
			{
				if(state == INITIAL_VAR_MEMBERS)
					struct_->sema->num_initial_var_members += 1;

				if(state == CASE_MEMBERS)
					state = TRAILING_VAR_MEMBERS;

				register_types(*var_member.var.type, struct_->sema->type_scope, struct_, ctx);
				bool inserted = member_names.insert(ctx.mod->name_of(var_member.var)).second;
				if(not inserted)
					throw_sem_error("A member with this name has already been declared", var_member.var.range.first, ctx.mod);
			},
			[&](CaseMember case_member)
			{
				if(state == INITIAL_VAR_MEMBERS)
					state = CASE_MEMBERS;
				else if(state != CASE_MEMBERS)
					throw_sem_error("Variable members must come before or after all case members", case_member.struct_->range.first, ctx.mod);

				bool inserted = member_names.insert(case_member.struct_->name).second;
				if(not inserted)
					throw_sem_error("A member with this name has already been declared", case_member.struct_->range.first, ctx.mod);

				if(case_member.struct_->type_params->count)
					throw_sem_error("Case members cannot be generic", case_member.struct_->range.first, ctx.mod);

				register_struct_item(case_member.struct_, struct_, struct_->sema->type_scope, ctx);

				if(case_member.struct_->ctor_without_parens and struct_->num_var_members)
					throw_sem_error("Struct must have braces because it inherits members from parent", case_member.struct_->range.first, ctx.mod);
			},
			[&](StructMember struct_member)
			{
				register_struct_item(struct_member.struct_, struct_, struct_->sema->type_scope, ctx);
			},
		};
	}

	// If the struct has a constructor, create an array with all the constructor parameters, i.e.,
	// all the variable members.
	if(struct_->has_constructor())
	{
		size_t param_count = 0;
		for(StructItem *cur_item = struct_; cur_item;)
		{
			param_count += cur_item->num_var_members;
			cur_item = cur_item->is_case_member ? cur_item->sema->decl_parent->as_struct() : nullptr;
		}

		struct_->sema->ctor_params = alloc_fixed_array<Parameter const*>(param_count, ctx.arena);
		for(auto const &[idx, var_member]: all_var_members(struct_) | std::views::enumerate)
			struct_->sema->ctor_params->items[idx] = &var_member;
	}
}

void register_item(TopLevelItem &item, SemaContext &ctx)
{
	item | match
	{
		[&](ProcItem &proc)
		{
			proc.sema = ctx.arena.alloc<Proc>(ctx.mod->sema->globals->new_child(true));
			proc.sema->param_vars = alloc_fixed_array<Var*>(proc.params->count, ctx.arena);
			for(auto const& [idx, param]: *proc.params | std::views::enumerate)
			{
				register_types(*param.type, proc.sema->scope, &proc, ctx);
				Var *param_var = proc.sema->scope->declare_var(ctx.mod->name_of(param), IsMutable::NO, param.range);
				proc.sema->param_vars->items[idx] = param_var;
			}

			register_types(*proc.ret_type, proc.sema->scope, &proc, ctx);

			size_t num_type_params = proc.type_params->count;
			for(size_t i = 0; i < num_type_params; ++i)
			{
				GenericParameter &type_param = proc.type_params->items[i];
				proc.sema->scope->declare_type_var(&type_param);
			}

			if(proc.body)
			{
				std::get<BlockStmt>(*proc.body).scope = proc.sema->scope;
				register_types_in_stmt(*proc.body, proc.sema->scope, &proc, ctx);
			}

			ctx.mod->sema->globals->declare_proc(&proc);
		},
		[&](StructItem &struct_)
		{
			register_struct_item(&struct_, nullopt, ctx.mod->sema->globals.get(), ctx);
		},
		[&](AliasItem &alias)
		{
			alias.sema = ctx.arena.alloc<Alias>(ctx.mod->sema->globals->new_child(false));
			register_types(*alias.aliased_type, alias.sema->scope, nullopt, ctx);

			size_t num_type_params = alias.type_params->count;
			for(size_t i = 0; i < num_type_params; ++i)
			{
				GenericParameter &type_param = alias.type_params->items[i];
				alias.sema->scope->declare_type_var(&type_param);
			}

			ctx.mod->sema->globals->declare_alias(&alias);
		},
	};
}

void register_items(SemaContext &ctx)
{
	for(TopLevelItem &item: to_range(ctx.mod->items.list()))
		register_item(item, ctx);
}
