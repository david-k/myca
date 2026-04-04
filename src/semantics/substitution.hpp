#pragma once

#include "syntax/ast.hpp"

class InstanceRegistry;
struct TypeArgList;

enum class SubstitutionMode
{
	BEST_EFFORT,
	FULL_DEDUCTION,
	FULL,
};

struct SubstitutionOptions
{
	SubstitutionMode mode;
	TokenRange region_being_substituted{};
};

void substitute_in_type(
	Type &type,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions options
);

void substitute(
	TypeArgList &args,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions options
);

void substitute_in_expr(
	Expr &expr,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions options
);

void substitute_in_pattern(
	Pattern &pattern,
	TypeEnv const &subst,
	InstanceRegistry &registry,
	SubstitutionOptions options
);

void substitute_in_stmt(
	Stmt &stmt,
	TypeEnv const &subst,
	InstanceRegistry &registry,
	SubstitutionOptions options
);

StructInstance* substitute_in_struct(
	StructInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions options,
	bool *modified = nullptr
);

ProcInstance* substitute_in_proc(
	ProcInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions options,
	bool *modified = nullptr
);

void substitute_in_generic_arg(
	GenericArg &arg,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionOptions options
);
