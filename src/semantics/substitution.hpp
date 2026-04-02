#pragma once

#include "syntax/ast.hpp"

class InstanceRegistry;
struct TypeArgList;

struct BestEffortSubstitution {};
struct FullDeductionSubstitution { TokenRange region_being_substituted; };
struct FullSubstitution {};

using SubstitutionMode = variant<
	BestEffortSubstitution,
	FullDeductionSubstitution,
	FullSubstitution
>;

void substitute(
	Type &type,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
);

void substitute(
	TypeArgList &args,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
);

void substitute_types_in_expr(
	Expr &expr,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
);

void substitute_types_in_pattern(
	Pattern &pattern,
	TypeEnv const &subst,
	InstanceRegistry &registry,
	SubstitutionMode mode
);

void substitute_types_in_stmt(
	Stmt &stmt,
	TypeEnv const &subst,
	InstanceRegistry &registry,
	SubstitutionMode mode
);

StructInstance* substitute_types_in_struct(
	StructInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode,
	bool *modified = nullptr
);

ProcInstance* substitute_types_in_proc(
	ProcInstance *inst,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode,
	bool *modified = nullptr
);

void substitute(
	GenericArg &arg,
	TypeEnv const &env,
	InstanceRegistry &registry,
	SubstitutionMode mode
);
