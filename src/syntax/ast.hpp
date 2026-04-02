#pragma once

#include <memory>
#include <utility>
#include <variant>
#include "utils.hpp"
#include "syntax/lexer.hpp"
#include "syntax/parser.hpp"

struct Expr;
struct GenericArg;
struct GenericParameter;
struct DeductionVarDef;

struct Var;
struct StructItem;
class Callable;
class StructInstance;
class UnionInstance;
class ProcInstance;
struct ProcTypeInstance;

//--------------------------------------------------------------------
// Types
//--------------------------------------------------------------------
enum class BuiltinTypeDef
{
	NEVER,
	UNIT,
	BOOL,
	I8,
	U8,
	I32,
	U32,
	ISIZE,
	USIZE,
};
struct BuiltinType
{
	TokenRange range;
	BuiltinTypeDef builtin;
};

struct KnownIntType
{
	Int128 low;
	Int128 high;
};

struct Path
{
	TokenRange range;
	FixedArray<GenericArg> *type_args = nullptr;
	Path *NULLABLE child = nullptr;
};

enum class IsMutable
{
	YES,
	NO,
};
struct PointerType
{
	enum Kind
	{
		SINGLE,
		MANY,
	};

	TokenRange range;
	Type *pointee;
	Kind kind;
	IsMutable mutability;
};

struct ArrayType
{
	size_t count() const;

	TokenRange range;
	Type *element;
	Expr *NULLABLE count_arg = nullptr; // If null, the count should be deduced
};

struct UnionTypeUnresolved
{
	TokenRange range;
	FixedArray<Type> *alternatives = nullptr;
};

struct UnionType
{
	TokenRange range;
	UnionInstance *inst;
};

struct ProcTypeUnresolved
{
	TokenRange range;
	Type *ret;
	FixedArray<Type> *params = nullptr;
};

struct ProcType
{
	TokenRange range;
	ProcTypeInstance *inst;

	// Callable provides access to the parameter names and default values of the called object
	// (currently either a procedure or a struct constructor).
	// This is used for typechecking procedure/constructor calls, but is irrelevant for deciding
	// whether two ProcTypes are equal.
	Callable *callable = nullptr;
};

struct StructType
{
	TokenRange range;
	StructInstance *inst;
};

struct InlineStructType
{
	TokenRange range;
	StructItem *struct_;
};

struct GenericParameterVar { GenericParameter const *def; };
struct GenericDeductionVar { DeductionVarDef const *def; };
using GenericVar = variant<GenericParameterVar, GenericDeductionVar>;
struct VarType
{
	TokenRange range;
	GenericVar var;
};

struct GenericTypeParameter {};
struct GenericValueParameter { Type *type; };
struct GenericParameter
{
	TokenRange range;
	string_view name;
	variant<GenericTypeParameter, GenericValueParameter> kind;
};

// Using struct instead of typedef so that Type can be easily forward-declared
struct Type : variant<
	struct BuiltinType,
	struct KnownIntType,
	struct PointerType,
	struct ArrayType,
	struct ProcType,
	struct StructType,
	struct UnionType,
	struct VarType,

	// Unresolved types
	struct Path,
	struct ProcTypeUnresolved,
	struct UnionTypeUnresolved,
	struct InlineStructType
>
{
	using variant::variant;

	TokenRange token_range() const
	{
		return *this | match
		{
			[](KnownIntType const&) { return UNKNOWN_TOKEN_RANGE; },
			[](auto const &t) { return t.range; },
		};
	}

	optional<GenericDeductionVar> try_get_deduction_var() const
	{
		VarType const *var_type = std::get_if<VarType>(this);
		if(not var_type)
			return nullopt;

		GenericDeductionVar const* type_deduction_var = std::get_if<GenericDeductionVar>(&var_type->var);
		if(not type_deduction_var)
			return nullopt;

		return *type_deduction_var;
	}
};

static_assert(sizeof(Type) == 48, "sizeof(Type) is getting larger...");

//--------------------------------------------------------------------
// Expressions
//--------------------------------------------------------------------
struct IntLiteralExpr
{
	TokenRange range;
	Int128 value;

	Type *NULLABLE type = nullptr;
};

struct BoolLiteralExpr
{
	TokenRange range;
	bool value;

	Type *NULLABLE type = nullptr;
};

enum class StringLiteralKind
{
	C
};
struct StringLiteralExpr
{
	TokenRange range;
	StringLiteralKind kind;
	string_view value;

	Type *NULLABLE type = nullptr;
};

enum class UnaryOp
{
	NOT = int(Lexeme::NOT),
	NEG = int(Lexeme::MINUS),
};
struct UnaryExpr
{
	TokenRange range;
	Expr *sub;
	UnaryOp op;

	Type *NULLABLE type = nullptr;
};

enum class BinaryOp
{
	ADD = int(Lexeme::PLUS),
	SUB = int(Lexeme::MINUS),
	MUL = int(Lexeme::STAR),
	DIV = int(Lexeme::SLASH),

	EQ = int(Lexeme::DOUBLE_EQ),
	LT = int(Lexeme::LT),
	LE = int(Lexeme::LE),
	GT = int(Lexeme::GT),
	GE = int(Lexeme::GE),
};
struct BinaryExpr
{
	TokenRange range;
	Expr *left;
	Expr *right;
	BinaryOp op;

	Type *NULLABLE type = nullptr;
};

// The current syntax for taking a mutable address is `&mut var`, which is quite long.
// Potential alternatives:
// - Ditch `mut`: If `var` is mutable, then `&var` results in a mutable pointer
// - Replace `&mut` with `!`: `&var` results in a constant pointer, while `!var` results in a
//   mutable pointer
struct AddressOfExpr
{
	TokenRange range;
	Expr *object;
	IsMutable mutability;

	Type *NULLABLE type = nullptr;
};

struct DerefExpr
{
	TokenRange range;
	Expr *addr;

	Type *NULLABLE type = nullptr;
};

struct IndexExpr
{
	TokenRange range;
	Expr *addr;
	Expr *index;

	Type *NULLABLE type = nullptr;
};

struct MemberAccessExpr
{
	TokenRange range;
	Expr *object;
	string_view member;

	Type *NULLABLE type = nullptr;
};

struct AssignmentExpr
{
	TokenRange range;
	Expr *lhs;
	Expr *rhs;

	Type *NULLABLE type = nullptr;
};

struct AsExpr
{
	TokenRange range;
	Expr *src_expr;
	Type *target_type;

	Type *NULLABLE type = nullptr;
};

struct ConstructorExpr
{
	TokenRange range;
	Type *ctor;

	Type *NULLABLE type = nullptr;
};

struct ProcExpr
{
	TokenRange range;
	ProcInstance *inst;

	Type *NULLABLE type = nullptr;
};

struct CallExpr
{
	TokenRange range;
	Expr *callable;
	FixedArray<struct Argument> *args;

	Type *NULLABLE type = nullptr;
};

struct SizeOfExpr
{
	TokenRange range;
	Type *subject;

	Type *NULLABLE type = nullptr;
};

struct MakeExpr
{
	TokenRange range;
	Expr *init;
	Expr *addr;

	Type *NULLABLE type = nullptr;
};

struct VarExpr
{
	TokenRange range;
	Var const *NULLABLE var = nullptr;

	Type *NULLABLE type = nullptr;
};

struct GenericVarExpr
{
	TokenRange range;
	GenericVar var;

	Type *NULLABLE type = nullptr;
};


struct UnionInitExpr
{
	TokenRange range;
	Expr *alt_expr;

	// We have to store the alt_type instead of the alt_idx here because the order of the
	// alternatives may change during substitution. Additinally, some alternatives may be removed
	// if it turns out they are duplicates.
	Type *alt_type;

	Type *NULLABLE type = nullptr;
};

struct Expr : variant<
	IntLiteralExpr,
	BoolLiteralExpr,
	StringLiteralExpr,
	UnaryExpr,
	BinaryExpr,
	AddressOfExpr,
	DerefExpr,
	IndexExpr,
	MemberAccessExpr,
	AssignmentExpr,
	AsExpr,
	ConstructorExpr,
	ProcExpr,
	CallExpr,
	SizeOfExpr,
	MakeExpr,
	VarExpr,
	GenericVarExpr,
	UnionInitExpr,
	Path
>
{
	using variant::variant;

	Type const& type() const
	{
		Type const *t = try_get_type();
		assert(t);
		return *t;
	}

	Type& type()
	{
		return const_cast<Type&>(std::as_const(*this).type());
	}

	TokenRange token_range() const
	{
		return std::visit([](auto const &p) { return p.range; }, *this);
	}

	Type const* try_get_type() const
	{
		return *this | match
		{
			[](Path const&) -> Type* { assert(!"Expr::try_get_type(): Path"); },
			[](auto const &e) -> Type* { return e.type; },
		};
	}

	Type* try_get_type()
	{
		return const_cast<Type*>(std::as_const(*this).try_get_type());
	}

	optional<GenericDeductionVar> try_get_deduction_var() const
	{
		GenericVarExpr const *var = std::get_if<GenericVarExpr>(this);
		if(not var)
			return nullopt;

		GenericDeductionVar const* type_deduction_var = std::get_if<GenericDeductionVar>(&var->var);
		if(not type_deduction_var)
			return nullopt;

		return *type_deduction_var;
	}
};

struct Argument
{
	TokenRange range;
	Expr expr;
	string_view name{}; // May be empty
	int param_idx = 0; // Available after semantic analysis
};

struct GenericArg : variant<Type, Expr>
{
	using variant::variant;

	// TODO Remove
	Type& as_type() { return std::get<Type>(*this); }
	Type const& as_type() const { return std::get<Type>(*this); }
	Expr& as_expr() { return std::get<Expr>(*this); }
	Expr const& as_expr() const { return std::get<Expr>(*this); }

	TokenRange token_range() const
	{
		return std::visit([](auto const &t) { return t.token_range(); }, *this);
	}

	optional<GenericDeductionVar> try_get_deduction_var() const
	{
		return std::visit([](auto const &t) { return t.try_get_deduction_var(); }, *this);
	}
};

//--------------------------------------------------------------------
// Patterns
//--------------------------------------------------------------------
struct Pattern;
struct PatternArgument;

struct DerefPattern
{
	TokenRange range;
	Pattern *sub;

	Type *NULLABLE type = nullptr;
};

struct AddressOfPattern
{
	TokenRange range;
	Pattern *sub;
	IsMutable mutability;

	Type *NULLABLE type = nullptr;
};

struct VarPatternUnresolved
{
	TokenRange range;
	string_view name;
	IsMutable mutability;
};

struct VarPattern
{
	TokenRange range;
	Var *var = nullptr;

	Type *NULLABLE type = nullptr;
};

struct WildcardPattern
{
	TokenRange range;
	Type *NULLABLE type = nullptr;
};

struct ConstructorPattern
{
	TokenRange range;
	Type *ctor;
	FixedArray<PatternArgument> *args;
	bool has_parens;

	Type *NULLABLE type = nullptr;
};


struct Pattern : variant
<
	VarPatternUnresolved,
	VarPattern,
	DerefPattern,
	AddressOfPattern,
	ConstructorPattern,
	WildcardPattern
>
{
	template<typename T>
	Pattern(T &&t, Type *NULLABLE provided_type) :
		variant(std::forward<T>(t)),
		provided_type(provided_type) {}

	Type const& type() const
	{
		Type const *t = try_get_type();
		assert(t);
		return *t;
	}

	Type& type()
	{
		return const_cast<Type&>(std::as_const(*this).type());
	}

	Type const* try_get_type() const
	{
		return *this | match
		{
			[](VarPatternUnresolved const&) -> Type* { assert(!"Pattern::try_get_type(): VarPatternUnresolved"); },
			[](auto const &e) -> Type* { return e.type; },
		};
	}

	Type* try_get_type()
	{
		return const_cast<Type*>(std::as_const(*this).try_get_type());
	}

	TokenRange token_range() const
	{
		return std::visit([](auto const &p) { return p.range; }, *this);
	}

	Type *NULLABLE provided_type;
};

struct PatternArgument
{
	Pattern pattern;
	string_view param_name; // May be empty
	int param_idx = 0; // Available after semantic analysis
};

static_assert(sizeof(Pattern) == 56, "sizeof(Pattern) is getting larger...");

//--------------------------------------------------------------------
// Statements
//--------------------------------------------------------------------
struct Scope;

struct LetStmt
{
	TokenRange range;
	Pattern *lhs;
	Expr *init_expr;
};

struct ExprStmt
{
	TokenRange range;
	Expr *expr;
};

struct BlockStmt
{
	TokenRange range;
	FixedArray<Stmt> *stmts;
	Scope *scope = nullptr;
};

struct ReturnStmt
{
	TokenRange range;
	Expr *NULLABLE ret_expr = nullptr;
};

struct IfStmt
{
	TokenRange range;
	Expr *condition;
	Stmt *then;
	Stmt *NULLABLE else_ = nullptr;
};

struct WhileStmt
{
	TokenRange range;
	Expr *condition;
	Stmt *body;
};

struct DeclStmt
{
	TokenRange range;
	StructItem *item; // Only structs for the moment
};

struct MatchStmt
{
	TokenRange range;
	Expr *expr;
	FixedArray<struct MatchArm> *arms;
};

// Using struct instead of typedef so that Stmt can be easily forward-declared
struct Stmt : variant
<
	struct LetStmt,
	struct ExprStmt,
	struct BlockStmt,
	struct ReturnStmt,
	struct IfStmt,
	struct WhileStmt,
	struct MatchStmt,
	struct DeclStmt
>
{
	using variant::variant;
};

struct MatchArm
{
	Pattern capture;
	Stmt stmt;

	// Available after semantic analysis
	int discr = -1;
	Scope *scope = nullptr;
};

static_assert(sizeof(Stmt) == 40, "sizeof(Stmt) is getting larger...");

//--------------------------------------------------------------------
// Top-level items
//--------------------------------------------------------------------
struct SemaModule;
struct Struct;
struct Proc;
struct Alias;

struct NoDefaultValue {};
struct ExprPending {};
struct DefaultValueExpr : variant<NoDefaultValue, ExprPending, Expr*>
{
	using variant::variant;

	Expr* try_get_expr() const
	{
		return *this | match
		{
			[](Expr *expr) { return expr; },
			[](auto) { return (Expr*)nullptr; },
		};
	}

	NO_DANGLING Expr& get_expr() const
	{
		Expr *expr = try_get_expr();
		assert(expr && "DefaultValueExpr has no expr");
		return *expr;
	}

	explicit operator bool () const
	{
		return *this | match
		{
			[](NoDefaultValue) { return false; },
			[](auto) { return true; },
		};
	}
};

struct Parameter
{
	TokenRange range;
	Type *NULLABLE type = nullptr;
	DefaultValueExpr default_value;
};

struct ProcItem
{
	TokenRange range;
	string_view name;
	string_view receiver_name; // May be empty
	FixedArray<GenericParameter> *type_params = nullptr;
	FixedArray<Parameter> *params = nullptr;
	Type *NULLABLE ret_type = nullptr;
	Stmt *NULLABLE body = nullptr;
	bool is_extern = false;

	Proc *NULLABLE sema = nullptr; // Available after semantic analysis
};

struct StructMember { struct StructItem *struct_; };
struct CaseMember
{
	struct StructItem *struct_;
	optional<int> next_case_member_idx;
};
struct VarMember
{
	Parameter var;
	optional<int> next_var_member_idx;
};
using Member = variant<VarMember, StructMember, CaseMember>;

struct StructItem
{
	bool has_constructor() const { return num_case_members == 0; }

	TokenRange range;
	string_view name;
	FixedArray<GenericParameter> *type_params = nullptr;
	FixedArray<Member> *members = nullptr;
	bool is_implicit = false;
	bool ctor_without_parens = false;
	bool is_extern = false;
	bool is_case_member = false;

	optional<int> first_initial_var_member_idx = nullopt;
	optional<int> first_trailing_var_member_idx = nullopt;
	optional<int> first_case_member_idx = nullopt;
	int num_var_members = 0;
	int num_case_members = 0;

	Struct *NULLABLE sema = nullptr; // Available after semantic analysis
};

struct AliasItem
{
	TokenRange range;
	string_view name;
	FixedArray<GenericParameter> *type_params = nullptr;
	Type *aliased_type;

	Alias *NULLABLE sema = nullptr; // Available after semantic analysis
};

// Using struct instead of typedef so that TopLevelItem can be easily forward-declared
struct TopLevelItem : variant<
	ProcItem,
	StructItem,
	AliasItem
>
{
	using variant::variant;
};

//--------------------------------------------------------------------
// Top-level items
//--------------------------------------------------------------------
struct Module;
class TypeEnv;
class ConstraintSolver;

struct Module
{
	string_view name_of(Path const &path) const
	{
		Token const &tok = parser.token_at(path.range.first);
		return tok.text;
	}

	string_view name_of(Parameter const &param) const
	{
		Token const &tok = parser.token_at(param.range.first);
		assert(tok.kind == Lexeme::IDENTIFIER);
		return tok.text;
	}

	Parser parser;
	ListBuilder<TopLevelItem> items;

	std::unique_ptr<SemaModule> NULLABLE sema{}; // Available after semantic analysis
};
