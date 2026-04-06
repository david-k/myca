#include "semantics/ast_traversal.hpp"
#include "semantics/instance_registry.hpp"
#include "semantics/well_formed_term_set.hpp"

void WellFormedTermSet::add(Type const &type)
{
	Type t = clone(type, *m_arena);
	const_eval(t, *m_mod);
	m_well_formed_types.insert(t);
}

void WellFormedTermSet::add(Expr const &expr)
{
	Expr e = clone(expr, *m_arena);
	const_eval(e, *m_mod);
	m_well_formed_exprs.insert(e);
}

static void add_terms_to_set(Type const &type, WellFormedTermSet &well_formed_terms)
{
	type_visit(type, [&](this auto &&self, Type const &type) -> void
	{
		type | match
		{
			[&](ArrayType const &t)
			{
				assert(t.count_arg);
				TermInfo element_info = get_term_info(*t.element);
				TermInfo count_info = get_term_info(*t.count_arg);
				assert(not element_info.has_deduction_vars and not count_info.has_deduction_vars);
				if(element_info.has_generic_parameter_vars or count_info.has_generic_parameter_vars)
				{
					well_formed_terms.add(*t.count_arg);
					well_formed_terms.add(type);
				}
			},
			[&](auto const&) {},
		};
		type_visit_children(type, self);
	});
}

static void gather_well_formed_terms(
	FixedArray<GenericParameter> const *type_params,
	WellFormedTermSet &well_formed_terms
)
{
	for(GenericParameter const &gen_param: *type_params)
	{
		gen_param.kind | match
		{
			[&](GenericTypeParameter const&)
			{

			},
			[&](GenericValueParameter const &p)
			{
				Expr param_var = GenericVarExpr(UNKNOWN_TOKEN_RANGE, GenericVar(GenericParameterVar(&gen_param)));
				well_formed_terms.add(param_var);
				well_formed_terms.add(*p.type);
			},
		};
	}
}

void gather_well_formed_terms(ProcItem const &proc, WellFormedTermSet &well_formed_terms)
{
	gather_well_formed_terms(proc.type_params, well_formed_terms);
	add_terms_to_set(*proc.ret_type, well_formed_terms);
	for(Parameter const &param: *proc.params)
		add_terms_to_set(*param.type, well_formed_terms);
}

void gather_well_formed_terms(StructItem const &struct_, WellFormedTermSet &well_formed_terms)
{
	gather_well_formed_terms(struct_.type_params, well_formed_terms);
}
