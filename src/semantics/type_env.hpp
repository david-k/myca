#pragma once

#include "semantics/ast_operations.hpp"

class InstanceRegistry;

class TypeEnv
{
public:
	static TypeEnv from_type_args(
		FixedArray<GenericParameter> const *type_params,
		FixedArray<GenericArg> const *type_args,
		InstanceRegistry &registry
	);

	GenericArg const* try_lookup(GenericVar var) const;
	GenericArg const& lookup(GenericVar var) const;
	GenericArg& lookup(GenericVar var);

	bool update(GenericVar var, GenericArg const &new_type);
	void add(GenericVar var, GenericArg const &type);

	bool empty() const { return m_env.empty(); }
	unordered_map<GenericVar, GenericArg> const& mapping() const { return m_env; }

	bool has_only_deduction_vars() const
	{
		return not m_allowed_var_kind or *m_allowed_var_kind == DEDUCTION;
	}

	bool has_only_parameter_vars() const
	{
		return not m_allowed_var_kind or *m_allowed_var_kind == PARAMETER;
	}

	void materialize(class InstanceRegistry &registry);
	void print(std::ostream &os, Module const &mod) const;

private:
	// Stores which kind of GenericVars are allowed as keys in this TypeEnv.
	// The first GenericVar added to the TypeEnv determines the kind.
	// TypeEnvs are used for both type deduction and during instantiation, but these are distinct
	// phases that never happen at the same time.
	enum AllowedVarKind
	{
		DEDUCTION,
		PARAMETER,
	};
	void update_allowed_var_kind(GenericVar const &var);

	unordered_map<GenericVar, GenericArg> m_env;
	optional<AllowedVarKind> m_allowed_var_kind;
};

void add_type_args_to_env(
	TypeEnv &result,
	FixedArray<GenericParameter> const *type_params,
	FixedArray<GenericArg> const *type_args,
	InstanceRegistry &registry
);
