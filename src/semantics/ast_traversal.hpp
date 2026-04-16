#pragma once

#include "utils.hpp"
#include "syntax/ast.hpp"
#include "semantics/instance_registry.hpp"
#include <type_traits>
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

//--------------------------------------------------------------------
// Visiting types
//--------------------------------------------------------------------
template<
	MaybeConst<FixedArray<GenericArg>> T,
	typename Visitor
>
void args_visit(T *type_args, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;
	for(FORWARD_CONST(GenericArg) &arg: *type_args)
	{
		arg | match
		{
			[&](FORWARD_CONST(Type) &type_arg)
			{
				type_visit(type_arg, visitor);
			},
			[&](FORWARD_CONST(Expr) &expr_arg)
			{
				expr_visit(expr_arg, visitor);
			},
		};
	}
}

template<
	MaybeConst<Path> T,
	typename Visitor
>
void path_visit_children(T &&path, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;
	FORWARD_CONST(Path) *p = &path;
	while(p)
	{
		args_visit(FORWARD_CONST_VAL(p->type_args), visitor);
		p = p->child;
	}
}

template<typename Visitor>
void type_visit_children(BuiltinType const&, Visitor&&) {}

template<typename Visitor>
void type_visit_children(KnownIntType const&, Visitor&&) {}

template<typename Visitor>
void type_visit_children(VarType const&, Visitor&&) {}

template<
	MaybeConst<PointerType> T,
	typename Visitor
>
void type_visit_children(T &&t, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;
	type_visit(FORWARD_CONST_VAL(*t.pointee), visitor);
}

template<
	MaybeConst<ArrayType> T,
	typename Visitor
>
void type_visit_children(T &&t, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;
	type_visit(FORWARD_CONST_VAL(*t.element), visitor);
	expr_visit(FORWARD_CONST_VAL(*t.count_arg), visitor);
}

template<typename Visitor>
void type_visit_children(ProcType const &t, Visitor &&visitor)
{
	for(ProcTypeParameter const &p: *t.inst->params)
		type_visit(p.type, visitor);

	type_visit(std::as_const(*t.inst->ret), visitor);
}

template<
	MaybeConst<ProcTypeUnresolved> T,
	typename Visitor
>
void type_visit_children(T &&t, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;

	for(FORWARD_CONST(ProcTypeParameter) &p: *t.params)
		type_visit(FORWARD_CONST_VAL(p.type), visitor);

	type_visit(FORWARD_CONST_VAL(*t.ret), visitor);
}

template<typename Visitor>
void type_visit_children(StructType const &t, Visitor &&visitor)
{
	args_visit(std::as_const(t.inst->type_args().args), visitor);
}

template<typename Visitor>
void type_visit_children(UnionType const &t, Visitor &&visitor)
{
	for(Type const *p: t.inst->alternatives())
		type_visit(*p, visitor);
}

template<
	MaybeConst<UnionTypeUnresolved> T,
	typename Visitor
>
void type_visit_children(T &&t, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;

	for(FORWARD_CONST(Type) &p: *t.alternatives)
		type_visit(p, visitor);
}

template<
	MaybeConst<Path> T,
	typename Visitor
>
void type_visit_children(T &&path, Visitor &&visitor)
{
	path_visit_children(std::forward<T>(path), std::forward<Visitor>(visitor));
}

template<typename Visitor>
void type_visit_children(InlineStructType const&, Visitor&&) {}

template<
	MaybeConst<Type> T,
	typename Visitor
>
void type_visit_children(T &&type, Visitor &&visitor)
{
	type | match
	{
		[&](auto &t) { type_visit_children(t, visitor); },
	};
}

template<
	MaybeConst<Type> T,
	typename Visitor
>
void type_visit(T &&type, Visitor &&visitor)
{
	if constexpr(std::is_invocable_v<Visitor&&, Type&>)
		visitor(type);
	else
		type_visit_children(type, visitor);
}

//--------------------------------------------------------------------
// Visiting expressions
//--------------------------------------------------------------------
template<
	MaybeConst<IntLiteralExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
}

template<
	MaybeConst<BoolLiteralExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
}

template<
	MaybeConst<StringLiteralExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
}

template<
	MaybeConst<UnaryExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);

	expr_visit(FORWARD_CONST_VAL(*expr.sub), visitor);
}

template<
	MaybeConst<BinaryExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.left), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.right), visitor);
}

template<
	MaybeConst<AddressOfExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.object), visitor);
}

template<
	MaybeConst<DerefExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.addr), visitor);
}

template<
	MaybeConst<IndexExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.addr), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.index), visitor);
}

template<
	MaybeConst<MemberAccessExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.object), visitor);
}

template<
	MaybeConst<AssignmentExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.lhs), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.rhs), visitor);
}

template<
	MaybeConst<AsExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	type_visit(FORWARD_CONST_VAL(*expr.target_type), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.src_expr), visitor);
}

template<
	MaybeConst<ConstructorExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	type_visit(FORWARD_CONST_VAL(*expr.ctor), visitor);
}

template<
	MaybeConst<ProcExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	args_visit(std::as_const(expr.inst->type_args().args), visitor);
}

template<
	MaybeConst<CallExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.callable), visitor);
	for(FORWARD_CONST(Argument) &arg: *expr.args)
		expr_visit(arg.expr, visitor);
}

template<
	MaybeConst<SizeOfExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	type_visit(FORWARD_CONST_VAL(*expr.subject), visitor);
}

template<
	MaybeConst<MakeExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.addr), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.init), visitor);
}

template<
	MaybeConst<Path> E,
	typename Visitor
>
void expr_visit_children(E &&path, Visitor &&visitor)
{
	path_visit_children(std::forward<E>(path), std::forward<Visitor>(visitor));
}

template<
	MaybeConst<VarExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
}

template<
	MaybeConst<GenericVarExpr> E,
	typename Visitor
>
void expr_visit_children(E && expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
}

template<
	MaybeConst<UnionInitExpr> E,
	typename Visitor
>
void expr_visit_children(E &&expr, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<E>>;
	if(expr.type) type_visit(FORWARD_CONST_VAL(*expr.type), visitor);
	type_visit(FORWARD_CONST_VAL(*expr.alt_type), visitor);
	expr_visit(FORWARD_CONST_VAL(*expr.alt_expr), visitor);
}

template<
	MaybeConst<Expr> T,
	typename Visitor
>
void expr_visit_children(T &&expr, Visitor &&visitor)
{
	return expr | match
	{
		[&](auto &e) { return expr_visit_children(e, visitor); },
	};
}

template<
	MaybeConst<Expr> T,
	typename Visitor
>
void expr_visit(T &&expr, Visitor &&visitor)
{
	if constexpr(std::is_invocable_v<Visitor, Expr&>)
		visitor(expr);
	else
		expr_visit_children(expr, visitor);
}

//--------------------------------------------------------------------
// Visiting patterns
//--------------------------------------------------------------------
template<
	MaybeConst<Pattern> P,
	typename Visitor
>
void pattern_visit_children(P &&pattern, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<P>>;
	if(pattern.provided_type)
		type_visit(FORWARD_CONST_VAL(*pattern.provided_type), visitor);

	if(FORWARD_CONST(Type) *type = pattern.try_get_type())
		type_visit(*type, visitor);

	return pattern | match
	{
		[&](FORWARD_CONST(VarPatternUnresolved)&) {},
		[&](FORWARD_CONST(VarPattern)&) {},
		[&](FORWARD_CONST(DerefPattern) &p)
		{
			pattern_visit(FORWARD_CONST_VAL(*p.sub), visitor);
		},
		[&](FORWARD_CONST(AddressOfPattern) &p)
		{
			pattern_visit(FORWARD_CONST_VAL(*p.sub), visitor);
		},
		[&](FORWARD_CONST(ConstructorPattern) &p)
		{
			type_visit(FORWARD_CONST_VAL(*p.ctor), visitor);
			for(FORWARD_CONST(PatternArgument) &arg: *p.args)
				pattern_visit(arg.pattern, visitor);
		},
		[&](FORWARD_CONST(WildcardPattern)&) {},
	};
}

template<
	MaybeConst<Pattern> P,
	typename Visitor
>
void pattern_visit(P &&pattern, Visitor &&visitor)
{
	if constexpr(std::is_invocable_v<Visitor, Pattern&>)
		visitor(pattern);
	else
		pattern_visit_children(pattern, visitor);
}

//--------------------------------------------------------------------
// Visiting statements
//--------------------------------------------------------------------
template<
	MaybeConst<Stmt> S,
	typename Visitor
>
void stmt_visit_children(S &&stmt, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<S>>;
	return stmt | match
	{
		[&](FORWARD_CONST(LetStmt) &s)
		{
			pattern_visit(FORWARD_CONST_VAL(*s.lhs), visitor);
			if(s.init_expr)
				expr_visit(FORWARD_CONST_VAL(*s.init_expr), visitor);
		},
		[&](FORWARD_CONST(ExprStmt) &s)
		{
			expr_visit(FORWARD_CONST_VAL(*s.expr), visitor);
		},
		[&](FORWARD_CONST(BlockStmt) &s)
		{
			for(FORWARD_CONST(Stmt) &child_stmt: *s.stmts)
				stmt_visit(child_stmt, visitor);
		},
		[&](FORWARD_CONST(ReturnStmt) &s)
		{
			if(s.ret_expr)
				expr_visit(FORWARD_CONST_VAL(*s.ret_expr), visitor);
		},
		[&](FORWARD_CONST(IfStmt) &s)
		{
			expr_visit(FORWARD_CONST_VAL(*s.condition), visitor);
			stmt_visit(FORWARD_CONST_VAL(*s.then), visitor);
			if(s.else_)
				stmt_visit(FORWARD_CONST_VAL(*s.else_), visitor);
		},
		[&](FORWARD_CONST(WhileStmt) &s)
		{
			expr_visit(FORWARD_CONST_VAL(*s.condition), visitor);
			stmt_visit(FORWARD_CONST_VAL(*s.body), visitor);
		},
		[&](FORWARD_CONST(MatchStmt) &s)
		{
			expr_visit(FORWARD_CONST_VAL(*s.expr), visitor);
			for(FORWARD_CONST(MatchArm) &arm: *s.arms)
				stmt_visit(arm.stmt, visitor);
		},
		[&](FORWARD_CONST(DeclStmt)&) {},
	};
}

template<
	MaybeConst<Stmt> S,
	typename Visitor
>
void stmt_visit(S &&stmt, Visitor &&visitor)
{
	if constexpr(std::is_invocable_v<Visitor, Stmt&>)
		visitor(stmt);
	else
		stmt_visit_children(stmt, visitor);
}

#undef FORWARD_CONST
#undef FORWARD_CONST_VAL
