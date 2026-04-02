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

	void materialize(class InstanceRegistry &registry);
	void print(std::ostream &os, Module const &mod) const;

private:
	unordered_map<GenericVar, GenericArg> m_env;
};

void add_type_args_to_env(
	TypeEnv &result,
	FixedArray<GenericParameter> const *type_params,
	FixedArray<GenericArg> const *type_args,
	InstanceRegistry &registry
);
