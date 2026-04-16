#include "semantics/context.hpp"
#include "semantics/constraint_solver.hpp"
#include "semantics/type_env.hpp"
#include "semantics/module.hpp"

#include <expected>
#include <stdexcept>

size_t std::hash<ConstraintModifier>::operator () (ConstraintModifier m) const
{
	size_t h = compute_hash(m.index());
	m | match
	{
		[&](MemberTypeModifier t) { combine_hashes(h, compute_hash(t.member)); },
		[&](PointeeTypeModifier t) { combine_hashes(h, compute_hash((int)t.pointer_kind)); },
		[&](NoModifier) {},
	};

	return h;
}

bool operator == (ConstraintEdge const &a, ConstraintEdge const &b)
{
	return
		a.var_conv == b.var_conv and
		a.var_expr == b.var_expr and
		equiv(a.arg, b.arg) and
		a.arg_conv == b.arg_conv and
		a.arg_expr == b.arg_expr and
		a.arg_modifier == b.arg_modifier;
}

size_t std::hash<ConstraintEdge>::operator () (ConstraintEdge const &edge) const
{
	size_t h = 0;
	combine_hashes(h, compute_hash((int)edge.var_conv));
	combine_hashes(h, compute_hash(edge.var_expr));
	combine_hashes(h, compute_hash(edge.arg));
	combine_hashes(h, compute_hash((int)edge.arg_conv));
	combine_hashes(h, compute_hash(edge.arg_expr));
	combine_hashes(h, compute_hash(edge.arg_modifier));

	return h;
}

static TypeConversion glb(TypeConversion a, TypeConversion b)
{
	return (TypeConversion)std::min((int)a, (int)b);
}

std::expected<GenericArg, ErrorMsg> follow_arg(
	GenericArg const &arg,
	ConstraintModifier const &modifier,
	TypeEnv const *env,
	SemaContext *ctx
)
{
	std::expected<GenericArg, ErrorMsg> result = modifier | match
	{
		[&](NoModifier) -> std::expected<GenericArg, ErrorMsg>
		{
			return arg;
		},
		[&](PointeeTypeModifier m) -> std::expected<GenericArg, ErrorMsg>
		{
			GenericArg const *pointer_type = &arg;
			if(optional<GenericDeductionVar> var = arg.try_get_deduction_var())
				pointer_type = env ? env->try_lookup(*var) : nullptr;

			if(not pointer_type)
			{
				return std::unexpected(ErrorMsg(mk_error_msg(
					"Insufficiently constrained type "s + str(arg, *ctx->mod), arg.token_range().first, ctx->mod
				)));
			}

			return get_pointee_type(
				&pointer_type->as_type(),
				m.pointer_kind,
				pointer_type->token_range(),
				*ctx->mod
			);
		},
		[&](MemberTypeModifier m) -> std::expected<GenericArg, ErrorMsg>
		{
			GenericArg const *struct_type = &arg;
			if(optional<GenericDeductionVar> var = arg.try_get_deduction_var())
				struct_type = env ? env->try_lookup(*var) : nullptr;

			if(not struct_type)
			{
				return std::unexpected(ErrorMsg(mk_error_msg(
					"Insufficiently constrained type "s + str(arg, *ctx->mod), arg.token_range().first, ctx->mod
				)));
			}

			return get_member_type(
				&struct_type->as_type(),
				m.member,
				struct_type->token_range(),
				*ctx->mod
			);
		},
	};

	if(result and env)
		substitute_in_generic_arg(*result, *env, ctx->mod->sema->insts, {
			SubstitutionPhase::DEDUCTION,
			SubstitutionMode::BEST_EFFORT
		});

	return result;
}

string_view type_conv_symbol(TypeConversion conv)
{
	switch(conv)
	{
		case TypeConversion::NONE: return "=";
		case TypeConversion::TRIVIAL: return "^";
		case TypeConversion::IMPLICIT_CTOR: return "^^";
	}

	UNREACHABLE;
}

bool ConstraintSolver::add_relational_constraint(
	GenericDeductionVar var,
	ConstraintEdge const &edge
)
{
	bool changed = false;
	if(get_node(var).add_edge(edge))
		changed = true;

	if(
		optional<GenericDeductionVar> other_var = edge.arg.try_get_deduction_var();
		other_var and is<NoModifier>(edge.arg_modifier)
	)
	{
		changed |= get_node(*other_var).add_edge(ConstraintEdge{
			.var_conv = edge.arg_conv,
			.var_expr = edge.arg_expr,
			.arg = mk_generic_arg(var),
			.arg_conv = edge.var_conv,
			.arg_expr = edge.var_expr,
			.arg_modifier = NoModifier(),
			.error_msg = edge.error_msg,
		});
	}

	return changed;
}

string edge_to_str(ConstraintEdge const &edge, Module const &mod)
{
	std::stringstream ss;
	ss << " " << type_conv_symbol(edge.var_conv) << ":" << type_conv_symbol(edge.arg_conv) << " ";
	::print(edge.arg, mod, ss);
	edge.arg_modifier | match
	{
		[&](NoModifier) {},
		[&](MemberTypeModifier m)
		{
			ss << " /member: " << m.member;
		},
		[&](PointeeTypeModifier m)
		{
			switch(m.pointer_kind)
			{
				case PointerType::SINGLE: ss << " /single_deref"; break;
				case PointerType::MANY: ss << " /many_deref"; break;
			}
		},
	};
	return std::move(ss).str();
}

void ConstraintSolver::print(std::ostream &os) const
{
	for(auto const &[var, node]: m_nodes)
	{
		::print(mk_generic_arg(var), *m_ctx.mod, os);
		os << std::endl;

		TTable table;
		for(ConstraintEdge const &edge: node.push_edges)
			table.add_row({edge_to_str(edge, *m_ctx.mod)});

		for(ConstraintEdge const &edge: node.pull_edges)
			table.add_row({edge_to_str(edge, *m_ctx.mod)});

		table.print(os);
	}
}

class NewConstraintGatheringSubst : public Subst
{
public:
	virtual GenericArg const* try_get(GenericDeductionVar var) override
	{
		// Q: Why not look up the current type from TypeEnv?
		// A: Because TypeEnv contains in-progress types which may differ from the final type.
		//
		//      a <<- i8
		//      b <<- Option'(a)
		//      b <<- Option'(i32)
		//
		//    In the above example, we may at some point have `env[a] == i8`. If we then try to
		//    unify `Option'(a)` and `Option'(i32)` we get an error because `i8 != i32`, even though
		//    `env[a] == i32` would be a valid solution.

		// A possible solution to the above problem would be to store for each variable the allowed
		// TypeConversion. In the above example we would have `env[a] = (i8, IMPLICIT_CTOR)`. Then
		// the unification of `Option'(a)` and `Option'(i32)` would succeed because by using a
		// `IMPLICIT_CTOR` conversion we can make `a` equal to `i32`.
		//
		// The problem with this solution is that the unifier doesn't tell us about the conversion.
		// (The conversion happens when unifying the type args of `Option'(a)` and `Option'(i32)` so
		// no result type is produced.)
		//
		// This in turn can be solved by splitting the constraint solving algorithm into two steps:
		// 1. Constraint generation: Run the existing algorithm, but do not update the TypeEnv,
		//    making sure that the unifier never sees intermediate results. This ensures that all
		//    implicit constraints are made explicit. (For the example above, we would get an
		//    additional constraint `a -- i32` when unifying `Option'(a)` and `Option'(i32)`.)
		// 2. Constraint solving: Run the algorithm again, but this time update the TypeEnv and pass
		//    it to the unifier.
		//
		// Though now I'm back at the problem I had at the beginning: During this process,
		// additional pull constraints may be generated for nodes that do no have any push
		// constraints. Currently, when there are no push constriants, all pull constraints must be
		// equal. This is a stronger requirement than push constraints have, which leads to
		// situations where these additionally generated constraints lead to a constraint violation.

		(void)var;
		return nullptr;
	}

	virtual void on_conversion_request(TypeConversionEvent const &event, Expr *expr) override
	{
		if(not conversions)
			return;

		if(expr == left_expr)
		{
			conversions->insert({expr, {}});
			for(auto &[_, expr_events]: *conversions)
				expr_events.push_back(event);
		}

		if(expr == right_expr)
			(*conversions)[expr].push_back(event);
	}

	virtual void set(
		GenericDeductionVar var,
		TypeConversion var_conv,
		Expr *NULLABLE var_expr,
		GenericArg const &arg,
		TypeConversion arg_conv,
		Expr *NULLABLE arg_expr,
		optional<LazyErrorMsg> error_msg
	) override
	{
		new_nodes.push_back({
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
		});
	}

	vector<std::pair<GenericDeductionVar, ConstraintEdge>> new_nodes;
	unordered_map<Expr*, vector<TypeConversionEvent>> *conversions = nullptr;
	Expr *NULLABLE left_expr;
	Expr *NULLABLE right_expr;
};

struct ReductionResult
{
	GenericArg arg;
	TypeConversion conv;
};

struct ReductionState
{
	void set_error(ErrorMsg const &err)
	{
		error = err;
		all_edges_merged = false;
	}

	optional<ReductionResult> result = nullopt;
	unordered_map<Expr*, vector<TypeConversionEvent>> conversions;
	optional<ErrorMsg> error;
	bool all_edges_merged = true;
};

enum class ReductionBehavior
{
	MERGE,
	MERGE_EQUAL,
	NO_MERGE,
};

// "Reduction" refers to the process of calculating a concrete type for a GenericDeductionVar by,
// essentially, unifying all the types that are connected to the GenericDeductionVar.
//
// As an example, assume we have constraints C1, C2 and C3 that constrain `var` to the types A, B
// and C, respectively. In order to compute `result` we essentially do the following:
//
//     (1) result = unify(C1, C2)      // result contains the common type of A and B
//     (2) result = unify(result, C3)  // result contains the common type of A, B and C
//
// where (1) and (2) represent individual reduction steps.
//
// Care must be taken to ensure that type conversions are always correctly inserted. As an example,
// assume the following relationships: A <c B <c C, where A <c B means that A can be converted to B,
// but we do need to insert the corresponding conversion function.
//
// Now, when we do step (1) above, the unification needs to insert a conversion for the expression
// associated with C1 from A to B. In step (2), we need to insert conversions for both C1 and C2
// from B to A. In other words, we need to apply conversions to all previously visited constraints.
//
// See for example tests/common_type_struct_2
void reduction_step(
	ReductionState *state,
	ReductionBehavior behavior,
	ConstraintEdge const &edge,
	NewConstraintGatheringSubst *subst,
	TypeEnv const *env,
	SemaContext *ctx
)
{
	std::expected<GenericArg, ErrorMsg> edge_type = follow_arg(edge.arg, edge.arg_modifier, env, ctx);
	if(not edge_type)
		return state->set_error(edge_type.error());

	// If the edge type is a GenericDeductionVar do not merge (i.e. unify) it into `state` as it does
	// not contain any useful information.
	// This may sound like it's merely an optimization but it's also needed for correctness:
	//
	//   b ->> a ->> i8
	//
	// If we did allow GenericDeductionVar to be merged into `state`, then `b ->> a` would constitute a
	// valid push constraint for `a`. This in turn would mean that the pull constraint `a ->> i8`
	// would not update `a` (push takes precedence over pull), hence `env[a]` would never be set.
	if(edge_type->try_get_deduction_var())
	{
		// If edge_type becomes constrained later on then this error message will be cleared
		return state->set_error(ErrorMsg(mk_error_msg(
			"Insufficiently constrained type "s + str(*edge_type, *ctx->mod), edge_type->token_range().first, ctx->mod
		)));
	}

	TypeConversion edge_conv = edge.arg_conv;
	if(behavior == ReductionBehavior::MERGE_EQUAL)
		edge_conv = TypeConversion::NONE;

	Expr *left_expr = edge.var_expr;
	if(left_expr)
		state->conversions.insert({left_expr, {}});
	else if(state->conversions.size())
		left_expr = state->conversions.begin()->first;

	if(state->result)
	{
		if(behavior == ReductionBehavior::MERGE_EQUAL)
			state->result->conv = TypeConversion::NONE;

		subst->conversions = &state->conversions;
		subst->left_expr = left_expr;
		subst->right_expr = edge.arg_expr;

		GenericArg result;
		try
		{
			unify(
				UnifierOperand(state->result->arg, state->result->conv, left_expr),
				UnifierOperand(*edge_type, edge_conv, edge.arg_expr),
				UnifierState(*ctx, subst, edge.error_msg, &result)
			);
			if(behavior != ReductionBehavior::NO_MERGE)
			{
				state->result->arg = result;
				state->result->conv = glb(state->result->conv, edge.arg_conv);

				if(edge.arg_expr)
					state->conversions.insert({edge.arg_expr, {}});
			}
		}
		catch(ParseError const &exc) {
			state->set_error(ErrorMsg(exc.what()));
		}

		subst->conversions = nullptr;
	}
	else
	{
		if(behavior != ReductionBehavior::NO_MERGE)
		{
			state->result = ReductionResult(*edge_type, edge_conv);
			if(edge.arg_expr)
				state->conversions.insert({edge.arg_expr, {}});
		}
	}
}

static bool is_known_int(GenericArg const &arg)
{
	if(Type const *t = std::get_if<Type>(&arg))
		return is<KnownIntType>(*t);

	return false;
}

void ConstraintSolver::reduce(
	TypeEnv *NULLABLE env,
	unordered_map<Expr*, vector<TypeConversionEvent>> *NULLABLE conversions
)
{
	// int iteration_counter = 0;
	bool changed = true;
	while(changed)
	{
		changed = false;
		if(conversions) conversions->clear();
		for(auto &[var, node]: m_nodes)
		{
			NewConstraintGatheringSubst subst;
			ReductionState red_state;

			// Reduce all push edges to a single type (stored in red_state). This will be the type
			// of the node.
			for(ConstraintEdge const &edge: node.push_edges)
				reduction_step(&red_state, ReductionBehavior::MERGE, edge, &subst, env, &m_ctx);

			// How pull edges are handled depends on whether there are any push edges.
			// - If the node has at least one push edge, then it fully determines the node's type.
			//   In this case, we only check whether the node can be unified with the targets of
			//   each pull edge, but do not actually update the node's type while doing so
			//   (ReductionBehavior::NO_MERGE).
			// - If the node does not have any push edges, then we use the pull edges to determine
			//   the node's type. Because the node needs to be assignable to all pull edges, we
			//   require the pull edges to all be unifiable (ReductionBehavior::NO_MERGE). An
			//   alternative would be to calculate their intersection, but I prefer to keep things
			//   simple for now and I don't even know if it would be desirable.
			//
			// If the node *does* have push edges but `red_state` is still empty we use
			// TRY_MERGE_EQUAL. Consider the following:
			//
			//   b ->> a ->> i8
			//
			// `a` has only one push edge, but because `b` is otherwise unconstrained, `red_state`
			// will still be empty when we get here. So we use TRY_MERGE_EQUAL to merge i8 into `a`
			// which is then backpropagated to `b`. TRY_MERGE_EQUAL means that unification failures
			// are ignored. This is because MERGE_EQUAL is stricter than what is required once we
			// get a type via one of the pull edges.
			//
			//   a ->> i32
			//   a ->> i8
			//   a <<- b
			//   b <<- i8
			//
			// Here, MERGE_EQUAL fails for `a ->> i32` and `a ->> i8`, but once `a` receives `i8`
			// via its push edge from `b` it all works out.
			ReductionBehavior pull_reduction_behavior = ReductionBehavior::NO_MERGE;
			if(
				node.push_edges.empty() or
				not red_state.result or
				is_known_int(red_state.result->arg)
			) {
				pull_reduction_behavior = ReductionBehavior::MERGE_EQUAL;
			}

			for(ConstraintEdge const &edge: node.pull_edges)
				reduction_step(&red_state, pull_reduction_behavior, edge, &subst, env, &m_ctx);

			// Add all constraints that where generated during the previous reduction steps to the
			// cosntraint system.
			for(auto const &new_node: subst.new_nodes)
				changed |= add_relational_constraint(new_node.first, new_node.second);

			bool result_is_concrete = false;
			if(red_state.result)
			{
				result_is_concrete = not get_term_info(red_state.result->arg).has_deduction_vars;
				if(env and not type_var_occurs_in(var, red_state.result->arg))
					changed |= env->update(var, red_state.result->arg);
			}

			if(red_state.error)
				node.error = red_state.error;
			else if(not result_is_concrete)
			{
				// If edge_type becomes constrained later on then this error message will be cleared
				if(red_state.result)
				{
					GenericArg const *arg = &red_state.result->arg;
					node.error = ErrorMsg(mk_error_msg(
						"Insufficiently constrained type "s + str(*arg, *m_ctx.mod), arg->token_range().first, m_ctx.mod
					));
				}
				else
					node.error = ErrorMsg("Insufficiently constrained type");
			}
			else
				node.error = nullopt;

			if(conversions)
				conversions->insert_range(red_state.conversions);
		}

		// std::cout << "Iteration " << (iteration_counter+1) << std::endl;
		// constraints->print(std::cout);
		// if(env) env->print(std::cout, *ctx->mod);
		// iteration_counter += 1;
	}
}

TypeEnv ConstraintSolver::solve()
{
	// std::cout << std::endl;
	// constraints.print(std::cout);

	// Constraint solving is separated into two steps:
	//
	// 1. Constraint generation: Unify the edges of a node without updating the TypeEnv.
	// 2. Constraint solving: Unify the edges of a node *and* update the TypeEnv.
	//
	// During constraint generation we want to discover implicit constraints:
	//
	//      a <<- i8
	//      b <<- Option'(a)
	//      b <<- Option'(i32)
	//
	// Here, the implicit constraint `a -- i32` will be made explicit during the first step.
	// If we were also updating the TypeEnv during this step, then after visiting `a <<- i8` we
	// would have `env[a] == i8`. However, this would mean that the unification of `Option'(a)` and
	// `Option'(i32)` fails. This is solved by simply not updating the TypeEnv.

	TypeEnv env;
	unordered_map<Expr*, vector<TypeConversionEvent>> conversions;
	reduce(nullptr, nullptr);
	reduce(&env, &conversions);

	for(auto &[var, node]: m_nodes)
	{
		if(node.error)
			throw ParseError(node.error->msg);
	}

	env.materialize(m_ctx.mod->sema->insts);

	for(auto &[expr, events]: conversions)
	{
		for(TypeConversionEvent const &event: events)
			apply_conversion(event, expr, m_ctx.arena);
	}

	return env;
}
