#pragma once

#include "syntax/ast.hpp"

struct TypeDeductionVar {};
struct ValueDeductionVar { Type *type; };
using DeductionVarKind = variant<TypeDeductionVar, ValueDeductionVar>;

struct DeductionVarDef
{
	uint32_t id;
	DeductionVarKind kind;
};

// Holds all the state needed during semantic analysis
struct SemaContext
{
	SemaContext(Module &mod, Arena &arena) :
		mod(&mod),
		arena(arena) {}

	optional<class EventLogger>& logger();
	GenericDeductionVar new_deduction_var(DeductionVarKind kind)
	{
		DeductionVarDef *def = arena.alloc<DeductionVarDef>(next_deduction_var_id++, kind);
		return GenericDeductionVar(def);
	}

	Module *mod;
	Arena &arena;
	ProcItem *NULLABLE proc = nullptr; // The current procedure being analyzed
	uint32_t next_deduction_var_id = 0;
};
