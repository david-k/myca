#pragma once

#include "semantics/ast_operations.hpp"
#include "semantics/error.hpp"
#include "semantics/unification.hpp"
#include "semantics/passes.hpp"

struct IntegerCheck
{
	LazyErrorMsg error_msg;
	Type const *type;
};

struct CastCheck
{
	Expr *expr;
	Type const *target_type;
};

struct LValueCheck
{
	Expr *expr;
	IsMutable mutability;
};

// A check that is performed on a type after all TypeDeductionVars have been deduced
using TypeCheck = variant<
	IntegerCheck,
	CastCheck,
	LValueCheck
>;


struct MemberTypeModifier
{
	friend bool operator == (MemberTypeModifier, MemberTypeModifier) = default;
	string_view member;
};
struct PointeeTypeModifier
{
	friend bool operator == (PointeeTypeModifier, PointeeTypeModifier) = default;
	PointerType::Kind pointer_kind;
};
struct NoModifier
{
	friend bool operator == (NoModifier, NoModifier) = default;
};
using ConstraintModifier = variant<NoModifier, MemberTypeModifier, PointeeTypeModifier>;

template<>
struct std::hash<ConstraintModifier>
{
	size_t operator () (ConstraintModifier m) const;
};


struct ConstraintEdge
{
	TypeConversion var_conv = TypeConversion::NONE;
	Expr *NULLABLE var_expr = nullptr;

	GenericArg arg;
	TypeConversion arg_conv = TypeConversion::NONE;
	Expr *NULLABLE arg_expr = nullptr;
	ConstraintModifier arg_modifier = NoModifier();

	optional<LazyErrorMsg> error_msg{};
};

bool operator == (ConstraintEdge const &a, ConstraintEdge const &b);

template<>
struct std::hash<ConstraintEdge>
{
	size_t operator () (ConstraintEdge const &edge) const;
};


struct ConstraintNode
{
	bool add_edge(ConstraintEdge const &edge)
	{
		if(edge.var_conv == TypeConversion::NONE)
			return push_edges.insert(edge).second;

		return pull_edges.insert(edge).second;
	}

	GenericDeductionVar var;
	unordered_set<ConstraintEdge> push_edges{};
	unordered_set<ConstraintEdge> pull_edges{};
	optional<ErrorMsg> error = nullopt;
};

class ConstraintSolver
{
public:
	explicit ConstraintSolver(SemaContext &ctx) :
		m_ctx(ctx) {}

	void add_check(TypeCheck const &check);
	bool add_relational_constraint(
		GenericDeductionVar var,
		ConstraintEdge const &edge
	);

	ConstraintNode& get_node(GenericDeductionVar var)
	{
		auto it = m_nodes.find(var);
		if(it != m_nodes.end())
			return it->second;

		auto res = m_nodes.insert({var, ConstraintNode{
			.var = var,
			.push_edges = {},
			.pull_edges = {},
		}});

		return res.first->second;
	}

	void print(std::ostream &os) const;
	bool empty() const { return m_checks.empty() and m_nodes.empty(); }

	TypeEnv solve();

private:
	void reduce(
		TypeEnv *NULLABLE env,
		unordered_map<Expr*, vector<TypeConversionEvent>> *NULLABLE conversions
	);

	SemaContext &m_ctx;
	unordered_map<GenericDeductionVar, ConstraintNode> m_nodes;
	vector<TypeCheck> m_checks;
};

