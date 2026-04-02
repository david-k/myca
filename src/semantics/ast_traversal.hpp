#pragma once

#include "utils.hpp"
#include "syntax/ast.hpp"
#include "semantics/instance_registry.hpp"

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
using AddConstIf_v = AddConstIf<Target, Condition>::type;

template<typename T, typename BaseType>
concept MaybeConst = std::is_same_v<std::remove_cvref_t<T>, BaseType>;

#define CONST(T) AddConstIf_v<T, is_const>

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
	visitor(*t.pointee);
}

template<
	MaybeConst<ArrayType> T,
	typename Visitor
>
void visit_child_types(T &&t, Visitor &&visitor)
{
	visitor(*t.element);
}

template<typename Visitor>
void visit_child_types(ProcType const &t, Visitor &&visitor)
{
	for(Type const &p: *t.inst->params)
		visitor(p);

	visitor(*t.inst->ret);
}

template<
	MaybeConst<ProcTypeUnresolved> T,
	typename Visitor
>
void visit_child_types(T &&t, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;

	for(CONST(Type) &p: *t.params)
		visitor(p);

	visitor(*t.ret);
}

template<typename Visitor>
void visit_child_types(FixedArray<GenericArg> const *type_args, Visitor &&visitor)
{
	for(GenericArg const &arg: *type_args)
	{
		// TODO Exprs can also contain types, e.g. in as-expressions and when explicitly
		//      specifying the types for generic functions

		if(Type const *type_arg = std::get_if<Type>(&arg))
			visitor(*type_arg);
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
		visitor(*p);
}

template<
	MaybeConst<UnionTypeUnresolved> T,
	typename Visitor
>
void visit_child_types(T &&t, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;

	for(CONST(Type) &p: *t.alternatives)
		visitor(p);
}

template<
	MaybeConst<Path> T,
	typename Visitor
>
void visit_child_types(T &&path, Visitor &&visitor)
{
	visit_child_types(path.type_args, visitor);
	if(path.child)
		visitor(*path.child);
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
template<typename Visitor>
void traverse(Expr const &expr, Visitor &&visitor)
{
	expr | match
	{
		[&](IntLiteralExpr const &e)
		{
			visitor(e);
		},
		[&](BoolLiteralExpr const &e)
		{
			visitor(e);
		},
		[&](StringLiteralExpr const &e)
		{
			visitor(e);
		},
		[&](UnaryExpr const &e)
		{
			visitor(e);
			traverse(*e.sub, visitor);
		},
		[&](BinaryExpr const &e)
		{
			visitor(e);
			traverse(*e.left, visitor);
			traverse(*e.right, visitor);
		},
		[&](AddressOfExpr const &e)
		{
			visitor(e);
			traverse(*e.object, visitor);
		},
		[&](DerefExpr const &e)
		{
			visitor(e);
			traverse(*e.addr, visitor);
		},
		[&](IndexExpr const &e)
		{
			visitor(e);
			traverse(*e.addr, visitor);
			traverse(*e.index, visitor);
		},
		[&](MemberAccessExpr const &e)
		{
			visitor(e);
			traverse(*e.object, visitor);
		},
		[&](AssignmentExpr const &e)
		{
			visitor(e);
			traverse(*e.lhs, visitor);
			traverse(*e.rhs, visitor);
		},
		[&](AsExpr const &e)
		{
			visitor(e);
			traverse(*e.src_expr, visitor);
		},
		[&](ConstructorExpr const &e)
		{
			visitor(e);
		},
		[&](ProcExpr const &e)
		{
			visitor(e);
		},
		[&](CallExpr const &e)
		{
			visitor(e);
			traverse(*e.callable, visitor);
			for(Argument const &arg: *e.args)
				traverse(arg.expr, visitor);
		},
		[&](SizeOfExpr const &e)
		{
			visitor(e);
		},
		[&](MakeExpr const &e)
		{
			visitor(e);
			traverse(*e.addr, visitor);
			traverse(*e.init, visitor);
		},
		[&](Path const &e)
		{
			visitor(e);
		},
		[&](VarExpr const &e)
		{
			visitor(e);
		},
		[&](GenericVarExpr const &e)
		{
			visitor(e);
		},
		[&](UnionInitExpr const &e)
		{
			visitor(e);
			traverse(*e.alt_expr, visitor);
		},
	};
}

//--------------------------------------------------------------------
// Visiting patterns
//--------------------------------------------------------------------
template<typename Visitor>
void traverse(Pattern const &pattern, Visitor &&visitor)
{
	pattern | match
	{
		[&](VarPatternUnresolved const &p)
		{
			visitor(p, pattern.provided_type);
		},
		[&](VarPattern const &p)
		{
			visitor(p, pattern.provided_type);
		},
		[&](DerefPattern const &p)
		{
			visitor(p, pattern.provided_type);
			traverse(*p.sub, visitor);
		},
		[&](AddressOfPattern const &p)
		{
			visitor(p, pattern.provided_type);
			traverse(*p.sub, visitor);
		},
		[&](ConstructorPattern const &p)
		{
			visitor(p, pattern.provided_type);
			for(PatternArgument const &arg: *p.args)
				traverse(arg.pattern, visitor);
		},
		[&](WildcardPattern const &p)
		{
			visitor(p, pattern.provided_type);
		}
	};
}

//--------------------------------------------------------------------
// Visiting statements
//--------------------------------------------------------------------
template<typename Visitor>
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
}

#undef CONST
