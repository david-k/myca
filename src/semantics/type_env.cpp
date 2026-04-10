#include "semantics/context.hpp"
#include "semantics/error.hpp"
#include "semantics/instance_registry.hpp"
#include "semantics/substitution.hpp"
#include "semantics/type_env.hpp"

#include <algorithm>

TypeEnv TypeEnv::from_type_args(
	FixedArray<GenericParameter> const *type_params,
	FixedArray<GenericArg> const *type_args,
	InstanceRegistry &registry
)
{
	TypeEnv env;
	add_type_args_to_env(env, type_params, type_args, registry);
	return env;
}

GenericArg const* TypeEnv::try_lookup(GenericVar var) const
{
	auto it = m_env.find(var);
	if(it == m_env.end())
		return nullptr;

	return &it->second;
}

GenericArg const& TypeEnv::lookup(GenericVar var) const
{
	auto it = m_env.find(var);
	assert(it != m_env.end());

	return it->second;
}

GenericArg& TypeEnv::lookup(GenericVar var)
{
	auto it = m_env.find(var);
	assert(it != m_env.end());

	return it->second;
}

bool TypeEnv::update(GenericVar var, GenericArg const &new_type)
{
	assert(not type_var_occurs_in(var, new_type));

	auto it = m_env.find(var);
	if(it == m_env.end())
	{
		update_allowed_var_kind(var);
		m_env.emplace(var, new_type);
		return true;
	}

	bool updated = not equiv(new_type, it->second);
	it->second = new_type;
	return updated;
}

void TypeEnv::add(GenericVar var, GenericArg const &type)
{
	// Required by unification and would lead to infinite recursion in substitute()
	assert(not type_var_occurs_in(var, type));

	auto res = m_env.emplace(var, type);
	update_allowed_var_kind(var);
	assert(res.second);
}

void TypeEnv::materialize(class InstanceRegistry &registry)
{
	assert(has_only_deduction_vars());
	for(auto &[_, gen_arg]: m_env)
	{
		if(Type *t = std::get_if<Type>(&gen_arg))
			substitute_in_type(*t, *this, registry, {
				SubstitutionPhase::DEDUCTION,
				SubstitutionMode::FULL,
			});
	}
}

void TypeEnv::print(std::ostream &os, Module const &mod) const
{
	vector<std::pair<GenericVar, GenericArg const*>> sorted;
	for(auto const &[var, type]: m_env)
		sorted.push_back({var, &type});

	std::ranges::sort(sorted, [](auto const &a, auto const &b)
	{
		GenericVar va = a.first;
		GenericVar vb = b.first;

		if(va.index() < vb.index())
			return true;

		return va | match
		{
			[&](GenericParameterVar p)
			{
				return p.def->name < std::get<GenericParameterVar>(vb).def->name;
			},
			[&](GenericDeductionVar d)
			{
				return d.def->id < std::get<GenericDeductionVar>(vb).def->id;
			}
		};
	});

	for(auto const &[var, type]: sorted)
	{
		::print(var, os);
		os << " ==> ";
		::print(*type, mod, os);
		os << std::endl;
	}
}

void TypeEnv::update_allowed_var_kind(GenericVar const &var)
{
	if(m_allowed_var_kind)
	{
		if(*m_allowed_var_kind == AllowedVarKind::DEDUCTION) {
			assert(is<GenericDeductionVar>(var));
		}

		if(*m_allowed_var_kind == AllowedVarKind::PARAMETER) {
			assert(is<GenericParameterVar>(var));
		}
	}
	else
	{
		if(is<GenericDeductionVar>(var))
			m_allowed_var_kind = AllowedVarKind::DEDUCTION;
		else
			m_allowed_var_kind = AllowedVarKind::PARAMETER;
	}
}

void add_type_args_to_env(
	TypeEnv &result,
	FixedArray<GenericParameter> const *type_params,
	FixedArray<GenericArg> const *type_args,
	InstanceRegistry &registry)
{
	assert(result.has_only_parameter_vars());
	for(size_t i = 0; i < type_params->count; ++i)
	{
		GenericParameterVar type_param(&type_params->items[i]);
		GenericArg type_arg = type_args->items[i];

		// See tests struct_recursive_type_param_<N> for why we call substitute() and perform the
		// type_var_occurs_in() check
		substitute_in_generic_arg(type_arg, result, registry, {
			SubstitutionPhase::INSTANTIATION,
			SubstitutionMode::BEST_EFFORT,
		});
		if(not equiv(mk_generic_arg(type_param.def, UNKNOWN_TOKEN_RANGE), type_arg))
		{
			if(type_var_occurs_in(type_param, type_arg))
			{
				throw_sem_error(
					"The type argument " + str(type_args->items[i], registry.mod()) + " causes the type term to grow indefinitely",
					type_args->items[i].token_range().first,
					&registry.mod()
				);
			}

			result.add(type_param, type_arg);
		}
	}
}
