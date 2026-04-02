#include "semantics/context.hpp"
#include "semantics/instance_registry.hpp"
#include "semantics/passes.hpp"

struct DefaultValueDep
{
	variant<ProcItem const*, StructItem const*> callable;
	string_view field;

	friend bool operator == (DefaultValueDep const &a, DefaultValueDep const &b) = default;
};

template<>
struct std::hash<DefaultValueDep>
{
	size_t operator () (DefaultValueDep const &dep) const
	{
		size_t h = compute_hash(dep.field);
		dep.callable | match
		{
			[&](ProcItem const *proc) { combine_hashes(h, compute_hash(proc)); },
			[&](StructItem const *struct_) { combine_hashes(h, compute_hash(struct_)); },
		};

		return h;
	}
};

enum class VisitState
{
	IN_PROGRESS,
	DONE,
};

static void check_default_value_deps(
	variant<ProcItem const*, StructItem const*> callable,
	string_view field,
	Expr const &default_value,
	std::unordered_map<DefaultValueDep, VisitState> &deps,
	Module const &mod
);

static void expr_default_value_deps(Expr const &expr, std::unordered_map<DefaultValueDep, VisitState> &deps, Module const &mod)
{
	expr | match
	{
		[&](IntLiteralExpr const&) {},
		[&](BoolLiteralExpr const&) {},
		[&](StringLiteralExpr const&) {},
		[&](UnaryExpr const &e)
		{
			expr_default_value_deps(*e.sub, deps, mod);
		},
		[&](BinaryExpr const &e)
		{
			expr_default_value_deps(*e.left, deps, mod);
			expr_default_value_deps(*e.right, deps, mod);
		},
		[&](AddressOfExpr const &e)
		{
			expr_default_value_deps(*e.object, deps, mod);
		},
		[&](DerefExpr const &e)
		{
			expr_default_value_deps(*e.addr, deps, mod);
		},
		[&](IndexExpr const &e)
		{
			expr_default_value_deps(*e.addr, deps, mod);
			expr_default_value_deps(*e.index, deps, mod);
		},
		[&](MemberAccessExpr const &e)
		{
			expr_default_value_deps(*e.object, deps, mod);
		},
		[&](AssignmentExpr const &e)
		{
			expr_default_value_deps(*e.lhs, deps, mod);
			expr_default_value_deps(*e.rhs, deps, mod);
		},
		[&](AsExpr const &e)
		{
			expr_default_value_deps(*e.src_expr, deps, mod);
		},
		[&](ConstructorExpr const&) {},
		[&](ProcExpr const&) {},
		[&](CallExpr const &e)
		{
			ProcType const &proc_type = std::get<ProcType>(e.callable->type());
			Callable *callable = proc_type.callable;
			vector<bool> provided_args(callable->param_count(), false);
			for(Argument const &arg: *e.args)
			{
				expr_default_value_deps(arg.expr, deps, mod);
				provided_args[arg.param_idx] = true;
			}

			for(auto const &[idx, arg_provided]: provided_args | std::views::enumerate)
			{
				if(not arg_provided)
				{
					DefaultValueExpr default_value = callable->param_default_value_at(idx);
					check_default_value_deps(callable->callable_item(), callable->param_name_at(idx), default_value.get_expr(), deps, mod);
				}
			}
		},
		[&](SizeOfExpr const&) {},
		[&](MakeExpr const &e)
		{
			expr_default_value_deps(*e.addr, deps, mod);
			expr_default_value_deps(*e.init, deps, mod);
		},
		[&](UnionInitExpr const &e)
		{
			expr_default_value_deps(*e.alt_expr, deps, mod);
		},
		[&](VarExpr const&) {},
		[&](GenericVarExpr const&) {},
		[&](Path const&) { assert(!"expr_default_value_deps: Path"); },
	};
}

static void check_default_value_deps(
	variant<ProcItem const*, StructItem const*> callable,
	string_view field,
	Expr const &default_value,
	std::unordered_map<DefaultValueDep, VisitState> &deps,
	Module const &mod
)
{
	auto res = deps.emplace(DefaultValueDep(callable, field), VisitState::IN_PROGRESS);
	if(res.second) // inserted?
		expr_default_value_deps(default_value, deps, mod);
	else
	{
		if(res.first->second == VisitState::IN_PROGRESS)
			throw_sem_error("Cyclic default arguments", default_value.token_range().first, &mod);
	}

	res.first->second = VisitState::DONE;
}

static void check_default_values_in_struct(
	StructItem const *struct_,
	std::unordered_map<DefaultValueDep, VisitState> &default_value_deps,
	SemaContext const &ctx
)
{
	for(Member const &m: *struct_->members)
	{
		m | match
		{
			[&](VarMember const &var_member)
			{
				if(Expr *default_value = var_member.var.default_value.try_get_expr())
					check_default_value_deps(struct_, ctx.mod->name_of(var_member.var), *default_value, default_value_deps, *ctx.mod);
			},
			[&](CaseMember case_member)
			{
				check_default_values_in_struct(case_member.struct_, default_value_deps, ctx);
			},
			[&](StructMember struct_member)
			{
				check_default_values_in_struct(struct_member.struct_, default_value_deps, ctx);
			},
		};
	}
}

void check_default_values(SemaContext const &ctx)
{
	std::unordered_map<DefaultValueDep, VisitState> default_value_deps;
	for(TopLevelItem const &item: to_range(ctx.mod->items.list()))
	{
		item | match
		{
			[&](ProcItem const&)
			{
				// TODO
			},
			[&](StructItem const &struct_)
			{
				check_default_values_in_struct(&struct_, default_value_deps, ctx);
			},
			[&](AliasItem const&) {},
		};
	}
}
