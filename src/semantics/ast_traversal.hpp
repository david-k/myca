#pragma once

#include "utils.hpp"
#include "syntax/ast.hpp"
#include "semantics/instance_registry.hpp"
#include <utility>

template<typename Type, bool Condition>
struct AddConstIf;

template<typename Type>
struct AddConstIf<Type, false>
{
	using type = Type;
};

template<typename Type>
struct AddConstIf<Type, true>
{
	using type = Type const;
};

template<typename Target, bool Condition>
using AddConstIf_t = AddConstIf<Target, Condition>::type;

template<typename T, typename BaseType>
concept MaybeConst = std::is_same_v<std::remove_cvref_t<T>, BaseType>;

template<bool AddConst, typename T>
auto make_const_if(T &t) -> AddConstIf_t<T, AddConst>&
{
	return t;
}

#define FORWARD_CONST(T) AddConstIf_t<T, is_const>
#define FORWARD_CONST_VAL(p) make_const_if<is_const>(p)

//--------------------------------------------------------------------
// Visiting types
//--------------------------------------------------------------------
template<typename Visitor>
void visit_child_types(BuiltinType const&, Visitor&&) {}

template<typename Visitor>
void visit_child_types(KnownIntType const&, Visitor&&) {}

template<typename Visitor>
void visit_child_types(VarType const&, Visitor&&) {}

template<
	MaybeConst<PointerType> T,
	typename Visitor
>
void visit_child_types(T &&t, Visitor &&visitor)
{
	std::visit(visitor, *t.pointee);
}

template<
	MaybeConst<ArrayType> T,
	typename Visitor
>
void visit_child_types(T &&t, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;
	std::visit(visitor, FORWARD_CONST_VAL(*t.element));
}

template<typename Visitor>
void visit_child_types(ProcType const &t, Visitor &&visitor)
{
	for(Type const &p: *t.inst->params)
		std::visit(visitor, p);

	std::visit(visitor, std::as_const(*t.inst->ret));
}

template<
	MaybeConst<ProcTypeUnresolved> T,
	typename Visitor
>
void visit_child_types(T &&t, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;

	for(FORWARD_CONST(Type) &p: *t.params)
		std::visit(visitor, FORWARD_CONST_VAL(p));

	std::visit(visitor, FORWARD_CONST_VAL(*t.ret));
}

template<typename Visitor>
void visit_child_types(FixedArray<GenericArg> const *type_args, Visitor &&visitor)
{
	for(GenericArg const &arg: *type_args)
	{
		arg | match
		{
			[&](Type const &type_arg)
			{
				std::visit(visitor, type_arg);
			},
			[&](Expr const&)
			{
				// TODO Exprs can also contain types, e.g. in as-expressions and when explicitly
				//      specifying the types for generic functions
			},
		};
	}
}

template<typename Visitor>
void visit_child_types(StructType const &t, Visitor &&visitor)
{
	visit_child_types(t.inst->type_args().args, visitor);
}

template<typename Visitor>
void visit_child_types(UnionType const &t, Visitor &&visitor)
{
	for(Type const *p: t.inst->alternatives())
		std::visit(visitor, *p);
}

template<
	MaybeConst<UnionTypeUnresolved> T,
	typename Visitor
>
void visit_child_types(T &&t, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;

	for(FORWARD_CONST(Type) &p: *t.alternatives)
		std::visit(visitor, p);
}

template<
	MaybeConst<Path> T,
	typename Visitor
>
void visit_child_types(T &&path, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;

	visit_child_types(path.type_args, visitor);
	if(path.child)
		visitor(FORWARD_CONST_VAL(*path.child));
}

template<typename Visitor>
void visit_child_types(InlineStructType const&, Visitor&&) {}

template<
	MaybeConst<Type> T,
	typename Visitor
>
void visit_child_types(T &&type, Visitor &&visitor)
{
	type | match
	{
		[&](auto &t) { visit_child_types(t, visitor); },
	};
}

//--------------------------------------------------------------------
// Visiting expressions
//--------------------------------------------------------------------
template<typename T>
struct VisitorResultWrapper
{
	template<typename Visitor, typename Arg>
	void visit(Visitor &&visitor, Arg &&arg) {
		inner.combine(visitor(std::forward<Arg>(arg)));
	}

	T get() { return inner; }

	T inner;
};

template<>
struct VisitorResultWrapper<void>
{
	template<typename Visitor, typename Arg>
	void visit(Visitor &&visitor, Arg &&arg) {
		visitor(std::forward<Arg>(arg));
	}
	void get() {}
};

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(IntLiteralExpr const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(BoolLiteralExpr const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(StringLiteralExpr const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	MaybeConst<UnaryExpr> E,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;

	VisitorResultWrapper<Result> result;
	result.visit(std::forward<Visitor>(visitor), FORWARD_CONST_VAL(*expr.sub));
	return result.get();
}

template<
	MaybeConst<BinaryExpr> E,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*expr.left));
	result.visit(visitor, FORWARD_CONST_VAL(*expr.right));
	return result.get();
}

template<
	MaybeConst<AddressOfExpr> E,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*expr.object));
	return result.get();
}

template<
	MaybeConst<DerefExpr> E,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*expr.addr));
	return result.get();
}

template<
	MaybeConst<IndexExpr> E,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*expr.addr));
	result.visit(visitor, FORWARD_CONST_VAL(*expr.index));
	return result.get();
}

template<
	MaybeConst<MemberAccessExpr> E,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*expr.object));
	return result.get();
}

template<
	MaybeConst<AssignmentExpr> E,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*expr.lhs));
	result.visit(visitor, FORWARD_CONST_VAL(*expr.rhs));
	return result.get();
}

template<
	MaybeConst<AsExpr> E,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*expr.src_expr));
	return result.get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(ConstructorExpr const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(ProcExpr const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	MaybeConst<CallExpr> E,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*expr.callable));
	for(FORWARD_CONST(Argument) &arg: *expr.args)
		result.visit(visitor, arg.expr);
	return result.get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(SizeOfExpr const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	MaybeConst<MakeExpr> E,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*expr.addr));
	result.visit(visitor, FORWARD_CONST_VAL(*expr.init));
	return result.get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(FixedArray<GenericArg> const *type_args, Visitor &&visitor)
{
	VisitorResultWrapper<Result> result;
	for(GenericArg const &arg: *type_args)
	{
		arg | match
		{
			[&](Expr const &expr_arg)
			{
				result.visit(visitor, expr_arg);
			},
			[&](Type const&)
			{
				// TODO Types can also contain exprs, e.g. ArrayTypes
			},
		};
	}
	return result.get();
}

template<
	MaybeConst<Path> T,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(T &&path, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;

	VisitorResultWrapper<Result> result;
	visit_child_exprs(path.type_args, visitor);
	if(path.child)
		visitor(FORWARD_CONST_VAL(*path.child));

	return result.get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(VarExpr const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(GenericVarExpr const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	MaybeConst<UnionInitExpr> E,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*expr.alt_expr));
	return result.get();
}

template<
	MaybeConst<Expr> T,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Expr&>
>
Result visit_child_exprs(T &&expr, Visitor &&visitor)
{
	return expr | match
	{
		[&](auto &e) { return visit_child_exprs(e, visitor); },
	};
}

//--------------------------------------------------------------------
// Visiting patterns
//--------------------------------------------------------------------
template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Pattern&>
>
Result visit_child_patterns(VarPatternUnresolved const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Pattern&>
>
Result visit_child_patterns(VarPattern const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	MaybeConst<DerefPattern> P,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Pattern&>
>
Result visit_child_patterns(P &&pattern, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<P>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*pattern.sub));
	return result.get();
}

template<
	MaybeConst<AddressOfPattern> P,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Pattern&>
>
Result visit_child_patterns(P &&pattern, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<P>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*pattern.sub));
	return result.get();
}

template<
	MaybeConst<ConstructorPattern> P,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Pattern&>
>
Result visit_child_patterns(P &&pattern, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<P>>;

	VisitorResultWrapper<Result> result;
	for(FORWARD_CONST(PatternArgument) &arg: *pattern.args)
		result.visit(visitor, arg.pattern);
	return result.get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Pattern&>
>
Result visit_child_patterns(WildcardPattern const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	MaybeConst<Pattern> P,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Pattern&>
>
Result visit_child_patterns(P &&pattern, Visitor &&visitor)
{
	return pattern | match
	{
		[&](auto &p) { return visit_child_patterns(p, visitor); },
	};
}

//--------------------------------------------------------------------
// Visiting statements
//--------------------------------------------------------------------
template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Stmt&>
>
Result visit_child_stmts(LetStmt const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Stmt&>
>
Result visit_child_stmts(DeclStmt const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Stmt&>
>
Result visit_child_stmts(ExprStmt const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	MaybeConst<BlockStmt> S,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Stmt&>
>
Result visit_child_stmts(S &&stmt, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<S>>;

	VisitorResultWrapper<Result> result;
	for(FORWARD_CONST(Stmt) &child_stmt: *stmt.stmts)
		result.visit(visitor, child_stmt);
	return result.get();
}

template<
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Stmt&>
>
Result visit_child_stmts(ReturnStmt const&, Visitor&&)
{
	return VisitorResultWrapper<Result>().get();
}

template<
	MaybeConst<IfStmt> S,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Stmt&>
>
Result visit_child_stmts(S &&stmt, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<S>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*stmt.then));
	if(stmt.else_)
		result.visit(visitor, FORWARD_CONST_VAL(*stmt.else_));
	return result.get();
}

template<
	MaybeConst<WhileStmt> S,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Stmt&>
>
Result visit_child_stmts(S &&stmt, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<S>>;

	VisitorResultWrapper<Result> result;
	result.visit(visitor, FORWARD_CONST_VAL(*stmt.body));
	return result.get();
}

template<
	MaybeConst<MatchStmt> S,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Stmt&>
>
Result visit_child_stmts(S &&stmt, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<S>>;

	VisitorResultWrapper<Result> result;
	for(FORWARD_CONST(MatchArm) &arm: *stmt.arms)
		result.visit(visitor, arm.stmt);
	return result.get();
}

template<
	MaybeConst<Stmt> S,
	typename Visitor,
	typename Result = std::invoke_result_t<Visitor, Stmt&>
>
Result visit_child_stmts(S &&stmt, Visitor &&visitor)
{
	return stmt | match
	{
		[&](auto &s) { return visit_child_stmts(s, visitor); },
	};
}

/*template<typename Visitor>
void traverse(Stmt const &stmt, Visitor &&visitor)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			visitor(s);
		},
		[&](ExprStmt const &s)
		{
			visitor(s);
		},
		[&](BlockStmt const &s)
		{
			visitor(s);
			for(Stmt const &child_stmt: *s.stmts)
				traverse(child_stmt, visitor);
		},
		[&](ReturnStmt const &s)
		{
			visitor(s);
		},
		[&](IfStmt const &s)
		{
			visitor(s);
			traverse(*s.then, visitor);
			if(s.else_)
				traverse(*s.else_, visitor);
		},
		[&](WhileStmt const &s)
		{
			visitor(s);
			traverse(*s.body, visitor);
		},
		[&](DeclStmt const &s)
		{
			visitor(s);
		},
		[&](MatchStmt const &s)
		{
			visitor(s);
			for(MatchArm const &arm: *s.arms)
				traverse(arm.stmt, visitor);
		},
	};
}*/

#undef FORWARD_CONST
#undef FORWARD_CONST_VAL
