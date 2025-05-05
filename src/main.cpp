#include <cstdint>
#include <cassert>
#include <utility>
#include <iostream>
#include <fstream>
#include <ostream>
#include <sstream>
#include <charconv>
#include <memory>
#include <type_traits>
#include <string>
#include <string_view>
#include <variant>
#include <vector>
#include <optional>
#include <unordered_map>
#include <unordered_set>

#include "utils.hpp"


using std::vector;
using std::string;
using std::string_view;
using std::optional;
using std::nullopt;
using std::variant;
using std::unordered_map;
using std::unordered_set;
using namespace std::string_literals;
using namespace std::string_view_literals;


// `type_mut` (applicable to parameters and struct members of type T where T is a struct with case members)
//   - Only if a parameter/member is declared as `type_mut` is its assigned case member allowed to
//     change after initialization
//
// `^any` (any pointer)
//   - Only pointers annotated with the `any` modifier are allowed to point to an unrelated type
//
// The above two approaches help make flow-sensitive typing more useful because we limit what
// function calls can do.


//==============================================================================
// Lexer
//==============================================================================
class LexingError : public std::runtime_error
{
public:
	explicit LexingError(string const &msg) :
		std::runtime_error{msg} {}
};


struct SourceLocation
{
	size_t pos = 0;
	int line = 0;
	int col = 0;
};

struct SourceSpan
{
	SourceLocation begin;
	SourceLocation end;
};


class Lexer
{
public:
	static constexpr int END = 0xffffffff;

	explicit Lexer(string_view source) :
		m_source{source} {}

	char peek(size_t offset = 0)
	{
		size_t p = m_pos + offset;
		assert(p < m_source.length());

		return m_source[p];
	}

	optional<char> try_peek(size_t offset = 0)
	{
		size_t p = m_pos + offset;
		if(p < m_source.length())
			return m_source[p];

		return nullopt;
	}

	void advance(size_t n = 1)
	{
		while(n-- && has_more())
		{
			if(m_source[m_pos] == '\n')
			{
				m_line += 1;
				m_column = 1;
			}
			else
				m_column += 1;

			m_pos += 1;
		}
	}

	char const* cur() const { return m_source.begin() + m_pos; }
	char const* end() const { return m_source.end(); }

	size_t remaining() const { return m_source.length() - m_pos; }
	bool has_more(size_t how_many = 1) const { return m_pos + how_many <= m_source.length(); }

	SourceLocation location() const
	{
		return SourceLocation{
			.pos = m_pos,
			.line = m_line,
			.col = m_column,
		};
	}

private:
	string_view m_source;
	size_t m_pos = 0;
	int m_line = 1;
	int m_column = 1;
};


inline bool try_consume(Lexer &lex, string_view str)
{
	if(lex.remaining() < str.length())
		return false;

	for(size_t i = 0; i < str.length(); ++i)
	{
		if(lex.peek(i) != str[i])
			return false;
	}

	lex.advance(str.length());
	return true;
}


inline bool is_alphabetic(char ch)
{
	return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
}

inline bool is_digit(char ch)
{
	return ch >= '0' && ch <= '9';
}


inline bool is_whitespace(char ch)
{
	return ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r';
}

inline void skip_whitespace(Lexer &lex)
{
	while(lex.has_more() && is_whitespace(lex.peek()))
		lex.advance();
}

inline bool skip_comment(Lexer &lex)
{
	if(try_consume(lex, "/*"))
	{
		while(lex.has_more())
		{
			if(try_consume(lex, "*/"))
				return true;

			lex.advance();
		}

		throw LexingError{"Unclosed multi-line comment"};
	}

	if(try_consume(lex, "//"))
	{
		while(lex.has_more())
		{
			if(try_consume(lex, "\n"))
				return true;

			lex.advance();
		}
	}

	return false;
}

inline void skip_whitespace_and_comments(Lexer &lex)
{
	do skip_whitespace(lex);
	while(skip_comment(lex));
}

inline optional<uint64_t> try_read_number(Lexer &lex)
{
	char const *start_pos = lex.cur();
	while(lex.has_more() && is_digit(lex.peek()))
		lex.advance();

	if(lex.cur() == start_pos)
		return {};

	uint64_t integer;
	std::from_chars_result result = std::from_chars(start_pos, lex.cur(), integer);
	assert(result.ec == std::errc());
	assert(result.ptr == lex.cur());

	return integer;
}

inline optional<string_view> try_read_identifier(Lexer &lex)
{
	if(!lex.has_more())
		return nullopt;

	char ch = lex.peek();
	if(ch == '_' || is_alphabetic(ch))
	{
		char const *id_start = lex.cur();
		do
			lex.advance();
		while(lex.has_more() && (lex.peek() == '_' || is_alphabetic(lex.peek()) || is_digit(lex.peek())));

		return string_view{id_start, lex.cur()};
	}

	return nullopt;
}


//--------------------------------------------------------------------
// Language-specific stuff
//--------------------------------------------------------------------
enum class Lexeme
{
	IDENTIFIER,
	INT_LITERAL,
	C_STRING_LITERAL,
	TRUE,
	FALSE,

	DOT,
	COMMA,
	COLON,
	SEMICOLON,
	CIRCUMFLEX,
	AMPERSAND,
	QUESTIONMARK,
	SINGLE_QUOTE,
	AT,
	LEFT_PAREN,
	RIGHT_PAREN,
	LEFT_BRACE,
	RIGHT_BRACE,
	LEFT_BRACKET,
	RIGHT_BRACKET,

	THIN_ARROW,

	PLUS,
	MINUS,
	SLASH,
	STAR,
	EQ,

	NOT,

	LET,
	PROC,
	STRUCT,
	CASE,

	IF,
	ELSE,
	WHILE,
	MATCH,
	RETURN,

	AS,
	MUT,

	TYPEALIAS,
	EXTERN,

	SIZE_OF,
	MAKE,
};

string_view str(Lexeme tok)
{
	switch(tok)
	{
		case Lexeme::IDENTIFIER: return "IDENTIFIER";
		case Lexeme::INT_LITERAL: return "INT_LITERAL";
		case Lexeme::C_STRING_LITERAL: return "C_STRING_LITERAL";
		case Lexeme::TRUE: return "TRUE";
		case Lexeme::FALSE: return "FALSE";

		case Lexeme::DOT: return "DOT";
		case Lexeme::COMMA: return "COMMA";
		case Lexeme::COLON: return "COLON";
		case Lexeme::SEMICOLON: return "SEMICOLON";
		case Lexeme::CIRCUMFLEX: return "CIRCUMFLEX";
		case Lexeme::AMPERSAND: return "AMPERSAND";
		case Lexeme::QUESTIONMARK: return "QUESTIONMARK";
		case Lexeme::SINGLE_QUOTE: return "SINGLE_QUOTE";
		case Lexeme::AT: return "AT";
		case Lexeme::LEFT_PAREN: return "LEFT_PAREN";
		case Lexeme::RIGHT_PAREN: return "RIGHT_PAREN";
		case Lexeme::LEFT_BRACE: return "LEFT_BRACE";
		case Lexeme::RIGHT_BRACE: return "RIGHT_BRACE";
		case Lexeme::LEFT_BRACKET: return "LEFT_BRACKET";
		case Lexeme::RIGHT_BRACKET: return "RIGHT_BRACKET";

		case Lexeme::THIN_ARROW: return "THIN_ARROW";

		case Lexeme::PLUS: return "PLUS";
		case Lexeme::MINUS: return "MINUS";
		case Lexeme::SLASH: return "SLASH";
		case Lexeme::STAR: return "STAR";
		case Lexeme::EQ: return "EQ";

		case Lexeme::NOT: return "NOT";

		case Lexeme::LET: return "LET";
		case Lexeme::PROC: return "PROC";
		case Lexeme::STRUCT: return "STRUCT";
		case Lexeme::CASE: return "CASE";

		case Lexeme::IF: return "IF";
		case Lexeme::ELSE: return "ELSE";
		case Lexeme::WHILE: return "WHILE";
		case Lexeme::MATCH: return "MATCH";
		case Lexeme::RETURN: return "RETURN";

		case Lexeme::AS: return "AS";
		case Lexeme::MUT: return "MUT";

		case Lexeme::TYPEALIAS: return "TYPEALIAS";
		case Lexeme::EXTERN: return "EXTERN";

		case Lexeme::SIZE_OF: return "SIZE_OF";
		case Lexeme::MAKE: return "MAKE";
	}

	UNREACHABLE;
}

struct Token
{
	Lexeme kind;

	std::string_view text{};
	uint64_t int_val{};

	SourceSpan span;
};


optional<Lexeme> try_read_punctuation(Lexer &lex)
{
	switch(lex.peek())
	{
		case '.': lex.advance(); return Lexeme::DOT;
		case ',': lex.advance(); return Lexeme::COMMA;
		case ':': lex.advance(); return Lexeme::COLON;
		case ';': lex.advance(); return Lexeme::SEMICOLON;
		case '^': lex.advance(); return Lexeme::CIRCUMFLEX;
		case '&': lex.advance(); return Lexeme::AMPERSAND;
		case '?': lex.advance(); return Lexeme::QUESTIONMARK;
		case '\'': lex.advance(); return Lexeme::SINGLE_QUOTE;
		case '@': lex.advance(); return Lexeme::AT;
		case '(': lex.advance(); return Lexeme::LEFT_PAREN;
		case ')': lex.advance(); return Lexeme::RIGHT_PAREN;
		case '{': lex.advance(); return Lexeme::LEFT_BRACE;
		case '}': lex.advance(); return Lexeme::RIGHT_BRACE;
		case '[': lex.advance(); return Lexeme::LEFT_BRACKET;
		case ']': lex.advance(); return Lexeme::RIGHT_BRACKET;
		case '+': lex.advance(); return Lexeme::PLUS;
		case '-':
		{
			lex.advance();
			if(lex.try_peek() == '>')
			{
				lex.advance();
				return Lexeme::THIN_ARROW;
			}

			return Lexeme::MINUS;
		}
		case '*': lex.advance(); return Lexeme::STAR;
		case '/': lex.advance(); return Lexeme::SLASH;
		case '=': lex.advance(); return Lexeme::EQ;

		default: return nullopt;
	}
}

unordered_map<string_view, Lexeme> const KEYWORDS = {
	{"true", Lexeme::TRUE},
	{"false", Lexeme::FALSE},
	{"let", Lexeme::LET},
	{"proc", Lexeme::PROC},
	{"struct", Lexeme::STRUCT},
	{"case", Lexeme::CASE},
	{"if", Lexeme::IF},
	{"else", Lexeme::ELSE},
	{"while", Lexeme::WHILE},
	{"match", Lexeme::MATCH},
	{"return", Lexeme::RETURN},
	{"not", Lexeme::NOT},
	{"as", Lexeme::AS},
	{"mut", Lexeme::MUT},
	{"typealias", Lexeme::TYPEALIAS},
	{"extern", Lexeme::EXTERN},
	{"size_of", Lexeme::SIZE_OF},
	{"make", Lexeme::MAKE},
};


inline optional<Token> next_token(Lexer &lex)
{
	skip_whitespace_and_comments(lex);
	if(!lex.has_more())
		return nullopt;

	SourceLocation loc_begin = lex.location();
	if(optional<uint64_t> int_val = try_read_number(lex))
	{
		return Token{
			.kind = Lexeme::INT_LITERAL,
			.text = {},
			.int_val = *int_val,
			.span = {loc_begin, lex.location()},
		};
	}

	if(optional<Lexeme> tok_kind = try_read_punctuation(lex))
	{
		return Token{
			.kind = *tok_kind,
			.span = {loc_begin, lex.location()},
		};
	}

	// C string literals
	if(try_consume(lex, "c\""))
	{
		char const *string_start = lex.cur();
		while(lex.has_more())
		{
			if(lex.peek() == '"')
				break;

			lex.advance();
		}
		string_view str_literal(string_start, lex.cur());

		if(!try_consume(lex, "\""))
			throw LexingError("Missing closing quote");

		return Token{
			.kind = Lexeme::C_STRING_LITERAL,
			.text = str_literal,
			.span = {loc_begin, lex.location()},
		};
	}

	if(optional<string_view> id = try_read_identifier(lex))
	{
		if(auto keyword_it = KEYWORDS.find(*id); keyword_it != KEYWORDS.end())
		{
			return Token{
				.kind = keyword_it->second,
				.text = *id,
				.span = {loc_begin, lex.location()},
			};
		}

		return Token{
			.kind = Lexeme::IDENTIFIER,
			.text = *id,
			.span = {loc_begin, lex.location()},
		};
	}

	throw LexingError("Invalid character: "s + lex.peek());
}

inline vector<Token> tokenize(string_view source)
{
	Lexer lex{source};

	vector<Token> tokens;
	while(auto tok = next_token(lex))
		tokens.push_back(*tok);

	return tokens;
}


//==============================================================================
// AST
//==============================================================================
class CBackend;

struct TypeVarDef;
struct VarDef;
struct ProcDef;
struct ProcTypeDef;
struct StructDef;
struct AliasDef;


//--------------------------------------------------------------------
// Types
//--------------------------------------------------------------------
using QName = vector<string>;

enum BuiltinType
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

using Type = variant<
	BuiltinType,
	struct UnresolvedId,
	struct VarType,
	struct PointerType,
	struct ManyPointerType,
	struct StructType,
	struct ProcType,
	struct AliasType
>;

struct UnresolvedId
{
	QName qname;
};

struct VarType
{
	TypeVarDef *var;
};

enum class Mutability
{
	MUTABLE,
	CONST,
};

struct PointerType
{
	explicit PointerType(Type &&target_type, Mutability mutability) :
		target_type(std::make_unique<Type>(std::move(target_type))),
		mutability(mutability) {}

	bool is_mutable() const { return mutability == Mutability::MUTABLE; }
	bool is_const() const { return mutability == Mutability::CONST; }

	OwnPtr<Type> target_type;
	Mutability mutability;
};

struct ManyPointerType
{
	explicit ManyPointerType(Type &&element_type, Mutability mutability) :
		element_type(std::make_unique<Type>(std::move(element_type))),
		mutability(mutability) {}

	bool is_mutable() const { return mutability == Mutability::MUTABLE; }
	bool is_const() const { return mutability == Mutability::CONST; }

	OwnPtr<Type> element_type;
	Mutability mutability;
};

struct StructType
{
	StructDef *def;
};

struct ProcType
{
	ProcTypeDef *def;

	// Available after resolve_identifiers()
	ProcTypeDef const *canonical_def = nullptr;
};

struct AliasType
{
	AliasDef *def;
};


Type clone(Type const &type)
{
	return type | match
	{
		[&](BuiltinType const &t) -> Type { return t; },
		[&](VarType const &t) -> Type { return t; },
		[&](UnresolvedId const &t) -> Type { return t; },
		[&](PointerType const &t) -> Type { return PointerType(clone(*t.target_type), t.mutability); },
		[&](ManyPointerType const &t) -> Type { return ManyPointerType(clone(*t.element_type), t.mutability); },
		[&](StructType const &t) -> Type { return t; },
		[&](ProcType const &t) -> Type { return t; },
		[&](AliasType const &t) -> Type { return AliasType(t.def); },
	};
}


//--------------------------------------------------------------------
// Expressions
//--------------------------------------------------------------------
class Scope;
class Expr;


struct IntLiteralExpr { XInt64 value; };

struct BoolLiteralExpr { bool value; };

enum class StringLiteralType
{
	C
};

struct StringLiteralExpr
{
	StringLiteralType type;
	string_view value;
};


struct VarExpr { VarDef *var; };
struct StructExpr { StructDef *def; };
struct ProcExpr { ProcDef *def; };


enum class UnaryOp
{
	NOT = int(Lexeme::NOT),
};

struct UnaryExpr
{
	OwnPtr<Expr> sub;
	UnaryOp op;

	UnaryExpr(Expr &&sub, UnaryOp op) :
		sub(std::make_unique<Expr>(std::move(sub))),
		op(op) {}
};


enum class BinaryOp
{
	ADD = int(Lexeme::PLUS),
	SUB = int(Lexeme::MINUS),
	MUL = int(Lexeme::STAR),
	DIV = int(Lexeme::SLASH),
};

struct BinaryExpr
{
	OwnPtr<Expr> left;
	OwnPtr<Expr> right;
	BinaryOp op;

	BinaryExpr(Expr &&left, Expr &&right, BinaryOp op) :
		left(std::make_unique<Expr>(std::move(left))),
		right(std::make_unique<Expr>(std::move(right))),
		op(op) {}
};


struct AddressOfExpr
{
	OwnPtr<Expr> object_expr;
	Mutability mutability;

	AddressOfExpr(Expr &&object_expr, Mutability mutability) :
		object_expr(std::make_unique<Expr>(std::move(object_expr))),
		mutability(mutability) {}
};

struct DerefExpr
{
	OwnPtr<Expr> ptr_expr;

	DerefExpr(Expr &&ptr_expr) :
		ptr_expr(std::make_unique<Expr>(std::move(ptr_expr))) {}
};

struct IndexExpr
{
	OwnPtr<Expr> ptr_expr;
	OwnPtr<Expr> idx_expr;

	IndexExpr(Expr &&ptr_expr, Expr &&idx_expr) :
		ptr_expr(std::make_unique<Expr>(std::move(ptr_expr))),
		idx_expr(std::make_unique<Expr>(std::move(idx_expr))) {}
};

struct MemberAccessExpr
{
	OwnPtr<Expr> object;
	string member;

	MemberAccessExpr(Expr &&object, string const &member) :
		object(std::make_unique<Expr>(std::move(object))),
		member(member) {}
};

struct AssignmentExpr
{
	OwnPtr<Expr> lhs;
	OwnPtr<Expr> rhs;

	AssignmentExpr(Expr &&lhs, Expr &&rhs) :
		lhs(std::make_unique<Expr>(std::move(lhs))),
		rhs(std::make_unique<Expr>(std::move(rhs))) {}
};

struct AsExpr
{
	OwnPtr<Expr> src_expr;
	Type target_type;

	AsExpr(Expr &&src_expr, Type &&target_type) :
		src_expr(std::make_unique<Expr>(std::move(src_expr))),
		target_type(std::move(target_type)) {}
};


using Callable = variant<ProcDef*, StructDef*, UnresolvedId>;

struct Parameter
{
	string name;
	Type type;
	Expr *default_value;
	// REVISIT: This is a bit hacky. I think it would be cleaner to use an arena allocator for
	//          expressions so we don't need to care about lifetimes.
	bool owned;

	Parameter() :
		default_value(nullptr),
		owned(false) {}

	Parameter(string const &name, Type &&type, NullableOwnPtr<Expr> default_value) :
		name(name),
		type(std::move(type)),
		default_value(default_value.release()),
		owned(true) {}

	Parameter(string const &name, Type &&type, Expr *default_value) :
		name(name),
		type(std::move(type)),
		default_value(default_value),
		owned(false) {}

	Parameter(Parameter &&rhs) :
		name(std::move(rhs.name)),
		type(std::move(rhs.type)),
		default_value(rhs.default_value),
		owned(rhs.owned)
	{
		rhs.default_value = nullptr;
		rhs.owned = false;
	}

	Parameter(Parameter const &rhs) :
		name(rhs.name),
		type(clone(rhs.type)),
		default_value(rhs.default_value),
		owned(false) {}

	~Parameter();
};

struct Argument
{
	OwnPtr<Expr> expr;
	optional<string> name;

	// Available after type checking
	optional<size_t> param_idx = nullopt;
};

struct CallExpr
{
	OwnPtr<Expr> callable;
	vector<Argument> args;

	CallExpr(Expr &&callable, vector<Argument> args) :
		callable(std::make_unique<Expr>(std::move(callable))),
		args(std::move(args)) {}

	vector<Parameter> *callable_params = nullptr;
};

struct SizeOfExpr
{
	Type type;
};

struct MakeExpr
{
	OwnPtr<Expr> init;
	OwnPtr<Expr> addr;
};


class Expr : public variant
<
	IntLiteralExpr,
	BoolLiteralExpr,
	StringLiteralExpr,
	UnresolvedId,
	VarExpr,
	StructExpr,
	ProcExpr,
	UnaryExpr,
	BinaryExpr,
	AddressOfExpr,
	DerefExpr,
	IndexExpr,
	MemberAccessExpr,
	AssignmentExpr,
	AsExpr,
	CallExpr,
	SizeOfExpr,
	MakeExpr
> {
public:
	template<typename T>
	Expr(T &&t, SourceSpan span) :
		variant(std::forward<T>(t)),
		span(span) {}

	SourceSpan span;
	optional<Type> type;
};


Parameter::~Parameter()
{
	if(owned)
		delete default_value;
}


//--------------------------------------------------------------------
// Patterns
//--------------------------------------------------------------------
struct NoPatternOp {};
struct DerefPatternOp {};

struct AddressOfPatternOp
{
	Mutability mutability;
};

using PatternOp = variant<NoPatternOp, DerefPatternOp, AddressOfPatternOp>;


struct VarPattern
{
	VarDef *var;
};


struct Pattern : variant<VarPattern>
{
	template<typename T>
	Pattern(T &&t, PatternOp op) :
		variant(std::forward<T>(t)),
		op(op) {}

	PatternOp op;
};


//--------------------------------------------------------------------
// Statements
//--------------------------------------------------------------------
using Stmt = variant<
	struct LetStmt,
	struct ExprStmt,
	struct BlockStmt,
	struct ReturnStmt,
	struct IfStmt,
	struct WhileStmt,
	struct MatchStmt
>;

struct LetStmt
{
	Pattern lhs;
	Expr init_expr;
};

struct ExprStmt
{
	Expr expr;
};

struct BlockStmt
{
	Scope *scope;
	vector<OwnPtr<Stmt>> stmts;
};

struct ReturnStmt
{
	optional<Expr> ret_expr;
};

struct IfStmt
{
	Expr condition;
	OwnPtr<Stmt> then;
	NullableOwnPtr<Stmt> else_;

	IfStmt(Expr &&cond, Stmt &&then, optional<Stmt> &&else_);
};

struct WhileStmt
{
	Expr condition;
	OwnPtr<Stmt> body;

	WhileStmt(Expr &&cond, Stmt &&body) :
		condition(std::move(cond)),
		body(std::make_unique<Stmt>(std::move(body))) {}
};


struct MatchArm
{
	Type type;
	OwnPtr<Stmt> stmt;
	optional<Pattern> capture;

	MatchArm(Type &&type, Stmt &&stmt, optional<Pattern> &&capture) :
		type(std::move(type)),
		stmt(std::make_unique<Stmt>(std::move(stmt))),
		capture(std::move(capture)) {}

	// Available after semantic analysis
	StructDef *struct_ = nullptr;
};

struct MatchStmt
{
	Expr expr;
	vector<MatchArm> arms;

	// Available after semantic analysis
	StructDef *subject = nullptr;
};


IfStmt::IfStmt(Expr &&cond, Stmt &&then, optional<Stmt> &&else_) :
	condition(std::move(cond)),
	then(std::make_unique<Stmt>(std::move(then))),
	else_(else_ ? std::make_unique<Stmt>(std::move(*else_)) : nullptr) {}


//--------------------------------------------------------------------
// Top-level items
//--------------------------------------------------------------------
using TopLevelItem = variant<ProcDef*, StructDef*, struct AliasDef*>;

void print(TopLevelItem const &item, std::ostream &os, int indent = 0);


//--------------------------------------------------------------------
// Scope/Module
//--------------------------------------------------------------------
class ParseError : public std::runtime_error
{
public:
	ParseError(std::string const &msg) :
		std::runtime_error{msg} {}
};


struct TypeVarDef
{
	string name;
};

enum class VarKind
{
	LOCAL,
	PARAM,
	FIELD,
};

struct VarDef
{
	string name;
	Mutability mutability;
	VarKind kind;
	optional<Type> type;

	bool is_mutable() const { return mutability == Mutability::MUTABLE; }
};


struct ProcTypeDef
{
	vector<Parameter> params;
	Type ret;
};

ProcTypeDef clone(ProcTypeDef const &proc)
{
	return ProcTypeDef{
		.params = proc.params,
		.ret = clone(proc.ret),
	};
}

Type strip_alias_rec(Type const &type);

namespace std
{
	template<>
	struct hash<::ProcTypeDef>
	{
		size_t operator () (::ProcTypeDef const &proc) const
		{
			size_t h = 0;
			for(::Parameter const &param: proc.params)
			{
				::combine_hashes(h, ::compute_hash(param.type));
				::combine_hashes(h, ::compute_hash(param.name));
				::combine_hashes(h, ::compute_hash((bool)param.default_value));
			}

			::combine_hashes(h, ::compute_hash(proc.ret));
			return h;
		}
	};
}

bool equiv(Type const &a, Type const &b);

struct ProcTypeEquiv
{
	bool operator () (ProcTypeDef const &a, ProcTypeDef const &b) const
	{
		if(a.params.size() != b.params.size())
			return false;

		for(size_t i = 0; i < a.params.size(); ++i)
		{
			Parameter const pa = a.params[i];
			Parameter const pb = b.params[i];

			if(pa.name != pb.name)
				return false;

			if(not equiv(pa.type, pb.type))
				return false;

			if((bool)pa.default_value != (bool)pb.default_value)
				return false;
		}

		if(not equiv(a.ret, b.ret))
			return false;

		return true;
	}
};


struct ProcDef
{
	string name;
	ProcTypeDef type;
	Scope *scope;
	optional<Stmt> body;
};


// A struct member can either be...
class Member : public variant
<
	// a variable member or
	Parameter,
	// a case member
	StructDef*
> {
public:
	using variant::variant;
};

struct Constructor
{
	vector<Parameter> params;
};

struct MemoryLayout
{
	size_t size;
	size_t alignment;

	size_t extend(MemoryLayout other)
	{
		// Offset to ensure proper alignment
		size_t offset = 0;
		if(other.alignment > 0 && size % other.alignment)
			offset = other.alignment - (size % other.alignment); 

		size += offset + other.size;
		alignment = std::max(alignment, other.alignment);

		return size - other.size;
	}
};

struct CasesLayout
{
	// Offset from the start of the struct to which the case members reside in memory.
	// The start is directly after the last initial var member (with no padding).
	size_t start;
	// The total number of bytes reserved for the case members, starting from `start`.
	// This may by larger than the largest case member due to incorporating alignment (in case a
	// case member cannot directly be located at `start` due to alignment requirements).
	size_t size;

	size_t end() const { return start + size; }
};

struct CaseMemberOf { StructDef *struct_; size_t case_idx; };
struct ExtensionOf { StructDef *struct_; };
struct NoParent {};
using ParentRelation = variant<NoParent, CaseMemberOf, ExtensionOf>;

struct StructDef
{
	string name;
	vector<TypeVarDef*> type_params;
	vector<Member> members;
	Scope *scope;

	ParentRelation parent = {};

	size_t num_initial_var_members = 0;
	size_t num_cases = 0;

	Range<Member const*> initial_var_members() const { return {members.data(), members.data() + num_initial_var_members}; }
	Range<Member const*> case_members() const { return {members.data() + num_initial_var_members, members.data() + num_initial_var_members + num_cases}; }
	Range<Member const*> trailing_var_members() const { return {members.data() + num_initial_var_members + num_cases, members.data() + members.size()}; }

	size_t get_case_idx() const
	{
		return parent | match
		{
			[](CaseMemberOf parent) { return parent.case_idx; },
			[](ExtensionOf) -> size_t { assert(!"get_case_idx: ExtensionOf"); },
			[](NoParent) -> size_t { assert(!"get_case_idx: NoParent"); },
		};
	}

	bool is_case_member_of(StructDef const *other) const
	{
		return parent | match
		{
			[&](CaseMemberOf parent)
			{
				if(other == parent.struct_)
					return true;

				return parent.struct_->is_case_member_of(other);
			},
			[](ExtensionOf) { return false; },
			[](NoParent) { return false; },
		};
	}

	StructDef* try_get_parent() const
	{
		return parent | match
		{
			[&](CaseMemberOf parent) { return parent.struct_; },
			[](ExtensionOf) -> StructDef* { return nullptr; },
			[](NoParent) -> StructDef* { return nullptr; },
		};
	}

	string get_qname() const
	{
		string qname;
		parent | match
		{
			[&](CaseMemberOf parent) { qname = parent.struct_->get_qname() + "."; },
			[](ExtensionOf) {},
			[](NoParent) {},
		};

		return qname + name;
	}

	// Available iff the struct does not contain any `case`s
	optional<Constructor> constructor;

	bool sema_done = false;
	// Available after semantic analysis (i.e., when `sema_done == true`)
	bool contains_never = false;
	optional<MemoryLayout> own_layout = nullopt; // does not include `parent`
	optional<BuiltinType> discriminator_type = nullopt; // if `num_cases > 0`
	optional<CasesLayout> cases_layout = nullopt; // if `num_cases > 0`
};


struct AliasDef
{
	string name;
	Type type;
};


using ItemDef = variant<ProcDef, StructDef, AliasDef>;
struct NotFound {};

Type const* strip_alias(Type const &type);

class Module;

class Scope
{
public:
	explicit Scope(Module *mod, Scope *parent = nullptr) :
		m_mod(mod),
		m_parent(parent) {}

	Scope* new_child()
	{
		m_children.push_back(std::make_unique<Scope>(m_mod, this));
		return m_children.back().get();
	}

	Scope* parent() { return m_parent; }

	TypeVarDef* declare_type_var(string name)
	{
		if(m_type_vars.contains(name))
			throw ParseError("Type variable already declared: " + name);

		auto res = m_type_vars.insert({name, TypeVarDef{
			.name = name,
		}});

		return &res.first->second;
	}

	TypeVarDef* try_lookup_type_var(string const &name)
	{
		auto it = m_type_vars.find(name);
		if(it != m_type_vars.end())
			return &it->second;

		if(m_parent)
			return m_parent->try_lookup_type_var(name);

		return nullptr;
	}

	VarDef* declare_var(string name, Mutability mutability, VarKind kind)
	{
		if(m_vars.contains(name))
			throw ParseError("Variable already declared: " + name);

		auto res = m_vars.insert({name, VarDef{
			.name = name,
			.mutability = mutability,
			.kind = kind,
			.type = nullopt,
		}});

		return &res.first->second;
	}

	VarDef* try_lookup_var(string const &name)
	{
		auto it = m_vars.find(name);
		if(it != m_vars.end())
			return &it->second;

		if(m_parent)
			return m_parent->try_lookup_var(name);

		return nullptr;
	}

	VarDef* lookup_var(string const &name)
	{
		if(VarDef *var = try_lookup_var(name))
			return var;

		throw ParseError("Variable has not been declared: " + name);
	}

	ProcDef* add_proc(string const &name, vector<Parameter> &&params, Type &&ret_type, Scope *scope, optional<Stmt> &&body)
	{
		if(m_defs.contains(name))
			throw ParseError("There already exists a definition with this name: " + name);

		auto res = m_defs.insert({name, ProcDef{
			.name = name,
			.type = ProcTypeDef(std::move(params), std::move(ret_type)),
			.scope = scope,
			.body = std::move(body),
		}});

		ProcDef *proc = &std::get<ProcDef>(res.first->second);

		return proc;
	}

	StructDef* add_struct(string const &name, Scope *scope)
	{
		if(m_defs.contains(name))
			throw ParseError("There already exists a definition with this name: " + name);

		auto res = m_defs.insert({name, StructDef{
			.name = name,
			.type_params = {},
			.members = {},
			.scope = scope,
			.constructor = {},
		}});

		StructDef *struct_ = &std::get<StructDef>(res.first->second);

		return struct_;
	}

	AliasDef* add_alias(string const &name, Type &&type)
	{
		if(m_defs.contains(name))
			throw ParseError("There already exists a definition with this name: " + name);

		auto res = m_defs.insert({name, AliasDef{
			.name = name,
			.type = std::move(type),
		}});

		AliasDef *alias = &std::get<AliasDef>(res.first->second);

		return alias;
	}

	ItemDef* try_lookup_item(QName const &qname)
	{
		return try_lookup_item(qname.begin(), qname.end());
	}

	ItemDef* try_lookup_item(QName::const_iterator begin, QName::const_iterator end, bool traverse_upwards = true)
	{
		assert(begin != end);

		auto it = m_defs.find(*begin);
		if(it == m_defs.end())
		{
			if(traverse_upwards && m_parent)
				return m_parent->try_lookup_item(begin, end, traverse_upwards);

			return nullptr;
		}

		ItemDef &def = it->second;
		if(++begin == end)
			return &def;

		return def | match
		{
			[&](ProcDef const&) -> ItemDef* { return nullptr; },
			[&](StructDef const &struct_) -> ItemDef* { return struct_.scope->try_lookup_item(begin, end, false); },
			[&](AliasDef const &alias) -> ItemDef*
			{
				return alias.type | match
				{
					[&](StructType t) { return t.def->scope->try_lookup_item(begin, end, false); },
					[](auto const&) -> ItemDef* { return nullptr; },
				};
			},
		};
	}

	variant<NotFound, ProcDef*, StructDef*> resolve_qname(QName const &qname)
	{
		ItemDef *item = try_lookup_item(qname);
		if(!item)
			return NotFound();

		return *item | match
		{
			[&](ProcDef &def) -> variant<NotFound, ProcDef*, StructDef*> { return &def; },
			[&](StructDef &def) -> variant<NotFound, ProcDef*, StructDef*> { return &def; },
			[&](AliasDef &alias) -> variant<NotFound, ProcDef*, StructDef*>
			{
				Type alias_type = AliasType(&alias);
				Type const *stripped_type = strip_alias(alias_type);
				return *stripped_type | match
				{
					[&](StructType t) -> variant<NotFound, ProcDef*, StructDef*>{ return t.def; },
					[](auto const&) -> variant<NotFound, ProcDef*, StructDef*> { return NotFound(); },
				};
			},
		};
	}

	template<typename DefT>
	DefT* try_lookup_def(QName const &qname)
		requires
			std::is_same_v<DefT, StructDef> ||
			std::is_same_v<DefT, ProcDef> ||
			std::is_same_v<DefT, AliasDef>
	{
		ItemDef *item = try_lookup_item(qname);
		if(!item)
			return nullptr;

		DefT *def = std::get_if<DefT>(item);
		if(!def)
			return nullptr;

		return def;
	}

	template<typename DefT>
	DefT* lookup_def(QName const &qname)
		requires
			std::is_same_v<DefT, StructDef> ||
			std::is_same_v<DefT, ProcDef> ||
			std::is_same_v<DefT, AliasDef>
	{
		ItemDef *item = try_lookup_item(qname);
		if(!item)
			throw ParseError("Definition not found");

		DefT *def = std::get_if<DefT>(item);
		if(!def)
			throw ParseError("Definition not found");

		return def;
	}

	using ItemDefIterator = PairValueIterator<unordered_map<string, ItemDef>::iterator>;
	Range<ItemDefIterator> item_defs() { return {m_defs.begin(), m_defs.end()}; }

	using VarDefIterator = PairValueIterator<unordered_map<string, VarDef>::iterator>;
	Range<VarDefIterator> var_defs() { return {m_vars.begin(), m_vars.end()}; }

	vector<OwnPtr<Scope>>& children() { return m_children; }

	Module* mod() { return m_mod; }

private:
	Module *m_mod;
	vector<OwnPtr<Scope>> m_children;
	Scope *m_parent;

	unordered_map<string, TypeVarDef> m_type_vars;
	unordered_map<string, VarDef> m_vars;
	unordered_map<string, ItemDef> m_defs;
};


class Module
{
public:
	Module() :
		m_global_scope(std::make_unique<Scope>(this)),
		m_cur_scope(m_global_scope.get()) {}

	Scope* scope() { return m_cur_scope; }
	Scope* global() { return m_global_scope.get(); }

	Scope* open_scope()
	{
		return m_cur_scope = m_cur_scope->new_child();
	}

	void close_scope()
	{
		assert(m_cur_scope != m_global_scope.get() && "Cannot close global scope");
		m_cur_scope = m_cur_scope->parent();
	}

	void add_item(TopLevelItem item)
	{
		m_items.push_back(item);
	}

	ProcTypeDef* add_proc_type(ProcTypeDef &&proc)
	{
		m_proc_types.push_back(std::make_unique<ProcTypeDef>(std::move(proc)));
		return m_proc_types.back().get();
	}

	ProcTypeDef const* canonicalize(ProcTypeDef &&proc)
	{
		for(Parameter &p: proc.params)
			p.type = strip_alias_rec(p.type);

		proc.ret = strip_alias_rec(proc.ret);

		auto res = m_canonical_proc_types.insert(std::move(proc));
		return &*res.first;
	}

	vector<TopLevelItem>& items() { return m_items; }
	vector<TopLevelItem> const& items() const { return m_items; }

	using ProcTypeIterator = vector<OwnPtr<ProcTypeDef>>::iterator;
	Range<ProcTypeIterator> proc_types() { return {m_proc_types.begin(), m_proc_types.end()}; }

private:
	OwnPtr<Scope> m_global_scope;
	Scope *m_cur_scope;
	vector<TopLevelItem> m_items;

	vector<OwnPtr<ProcTypeDef>> m_proc_types;
	unordered_set<ProcTypeDef, std::hash<ProcTypeDef>, ProcTypeEquiv> m_canonical_proc_types;
};


//==============================================================================
// Printing
//==============================================================================
constexpr int INDENT_WIDTH = 4;

std::ostream& operator << (std::ostream &os, SourceLocation loc)
{
	return os << loc.line << ":" << loc.col;
}

constexpr string_view TYPE_NEVER_NAME = "Never";
constexpr string_view TYPE_UNIT_NAME = "Unit";
constexpr string_view TYPE_ISIZE_NAME = "isize";
constexpr string_view TYPE_USIZE_NAME = "usize";

string_view str(BuiltinType t)
{
	switch(t)
	{
		case BuiltinType::NEVER: return TYPE_NEVER_NAME;
		case BuiltinType::UNIT: return TYPE_UNIT_NAME;
		case BuiltinType::BOOL: return "bool";
		case BuiltinType::I8: return "i8";
		case BuiltinType::U8: return "u8";
		case BuiltinType::I32: return "i32";
		case BuiltinType::U32: return "u32";
		case BuiltinType::ISIZE: return TYPE_ISIZE_NAME;
		case BuiltinType::USIZE: return TYPE_USIZE_NAME;
	}

	UNREACHABLE;
}

std::ostream& operator << (std::ostream &os, Type const &type)
{
	type | match
	{
		[&](BuiltinType const &t) { os << str(t); },
		[&](VarType const &t) { os << t.var->name; },
		[&](UnresolvedId const &t) { os << RangeFmt(t.qname, "."); },
		[&](PointerType const &t)
		{
			os << "^";
			if(t.is_mutable())
				os << "mut ";
			os << *t.target_type;
		},
		[&](ManyPointerType const &t) { os << "[^]" << *t.element_type; },
		[&](StructType const &t) { os << t.def->get_qname(); },
		[&](ProcType const &t)
		{
			os << "proc(" << RangeFmt(t.def->params, ", ", [&](Parameter const  &p)
			{
				os << p.type;
			});
			os << ") -> " << t.def->ret;
		},
		[&](AliasType const &t) { os << t.def->name; },
	};

	return os;
}


std::ostream& operator << (std::ostream &os, XInt64 xint)
{
	if(xint.is_signed())
		os << xint.as_signed();
	else
		os << xint.as_unsigned();

	return os;
}

class PrintListener
{
public:
	virtual void before_proc(std::ostream&, ProcDef*) {}
	virtual void after_proc(std::ostream&, ProcDef*) {}

	virtual void after_expr(std::ostream&, Expr const&) {}

	virtual void after_stmt(std::ostream &os, Stmt const &stmt, int indent)
	{
		(void)os;
		(void)stmt;
		(void)indent;
	}
};

std::ostream& print(std::ostream &os, Expr const &expr, PrintListener *listener = nullptr)
{
	expr | match
	{
		[&](IntLiteralExpr const &e)
		{
			os << e.value;
		},
		[&](BoolLiteralExpr const &e)
		{
			os << (e.value ? "true" : "false");
		},
		[&](StringLiteralExpr const &e)
		{
			switch(e.type)
			{
				case StringLiteralType::C: os << '"' << e.value << '"'; break;
			}
		},
		[&](UnresolvedId const &e)
		{
			os << RangeFmt(e.qname, ".");
		},
		[&](VarExpr const &e)
		{
			os << e.var->name;
		},
		[&](StructExpr const &e)
		{
			os << e.def->name;
		},
		[&](ProcExpr const &e)
		{
			os << e.def->name;
		},
		[&](UnaryExpr const &e)
		{
			os << "(";
			switch(e.op)
			{
				case UnaryOp::NOT: os << "not "; break;
			}
			print(os, *e.sub, listener) << ")";
		},
		[&](BinaryExpr const &e)
		{
			os << "(";
			print(os, *e.left, listener);
			switch(e.op)
			{
				case BinaryOp::ADD: os << " + "; break;
				case BinaryOp::SUB: os << " - "; break;
				case BinaryOp::MUL: os << " * "; break;
				case BinaryOp::DIV: os << " / "; break;
			}
			print(os, *e.right, listener) << ")";
		},
		[&](AddressOfExpr const &e)
		{
			os << "&";
			if(e.mutability == Mutability::MUTABLE)
				os << "mut ";

			os << "(";
			print(os, *e.object_expr, listener) << ")";
		},
		[&](DerefExpr const &e)
		{
			os << "(";
			print(os, *e.ptr_expr, listener) << ")^";
		},
		[&](IndexExpr const &e)
		{
			os << "(";
			print(os, *e.ptr_expr, listener) << ")[";
			print(os, *e.idx_expr, listener) << "]";
		},
		[&](MemberAccessExpr const &e)
		{
			print(os, *e.object, listener) << "." << e.member;
		},
		[&](AssignmentExpr const &e)
		{
			os << "(";
			print(os, *e.lhs, listener) << " = ";
			print(os, *e.rhs, listener) << ")";
		},
		[&](AsExpr const &e)
		{
			os << "(";
			print(os, *e.src_expr, listener) << " as " << e.target_type << ")";
		},
		[&](CallExpr const &e)
		{
			print(os, *e.callable, listener);
			os << "(";
			os << RangeFmt(e.args, ", ", [&](Argument const &arg)
			{
				if(arg.name)
					os << *arg.name << ": ";

				print(os, *arg.expr, listener);
			});
			os << ")";
		},
		[&](SizeOfExpr const &e)
		{
			os << "size_of(" << e.type << ")";
		},
		[&](MakeExpr const &e)
		{
			os << "make ";
			print(os, *e.init, listener);
			os << " @ ";
			print(os, *e.addr, listener);
		},
	};

	if(listener)
		listener->after_expr(os, expr);

	return os;
}

std::ostream& operator << (std::ostream &os, Expr const &expr)
{
	return print(os, expr);
}


std::ostream& print(std::ostream &os, PatternOp const &op)
{
	op | match
	{
		[&](NoPatternOp) {},
		[&](DerefPatternOp)
		{
			os << " ^";
		},
		[&](AddressOfPatternOp const &o)
		{
			os << " &";
			if(o.mutability == Mutability::MUTABLE)
				os << "mut";
		},
	};

	return os << " ";
}

std::ostream& print(std::ostream &os, Pattern const &pattern)
{
	pattern | match
	{
		[&](VarPattern const &p)
		{
			if(p.var->is_mutable())
				os << "mut ";
			os << p.var->name;

			print(os, pattern.op);

			if(p.var->type)
				os << ": " << *p.var->type;
		}
	};

	return os;
}


void print(std::ostream &os, Stmt const &stmt, int indent = 0, PrintListener *listener = nullptr)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ');
			os << "let ";
			print(os, s.lhs);
			os << " = ";
			print(os, s.init_expr, listener) << ";";
		},
		[&](ExprStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ');
			print(os, s.expr, listener) << ";";
		},
		[&](BlockStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ') << "{\n";
			for(OwnPtr<Stmt> const &stmt: s.stmts)
			{
				print(os, *stmt, indent+1, listener);
				os << "\n";
			}
			os << string(indent*INDENT_WIDTH, ' ') << "}";
		},
		[&](ReturnStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ');
			os << "return";
			if(s.ret_expr)
			{
				os << " ";
				print(os, *s.ret_expr, listener);
			}
			os << ";";
		},
		[&](IfStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ') << "if ";
			print(os, s.condition, listener) << "\n";
			print(os, *s.then, indent, listener);
			if(s.else_)
			{
				os << "\n" << string(indent*INDENT_WIDTH, ' ') << "else\n";
				print(os, *s.else_, indent, listener);
			}
		},
		[&](WhileStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ') << "while ";
			print(os, s.condition, listener) << "\n";
			print(os, *s.body, indent, listener);
		},
		[&](MatchStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ') << "match ";
			print(os, s.expr, listener) << "\n";
			os << string(indent*INDENT_WIDTH, ' ') << "{\n";
			for(MatchArm const &arm: s.arms)
			{
				os << string((indent+1)*INDENT_WIDTH, ' ') << "case " << arm.type << "\n";
				print(os, *arm.stmt, indent+1, listener);
				os << "\n";
			}
			os << string(indent*INDENT_WIDTH, ' ') << "}";
		},
	};

	if(listener)
		listener->after_stmt(os, stmt, indent);
}

std::ostream& operator << (std::ostream& os, Stmt const &stmt)
{
	print(os, stmt);
	return os;
}


void print(TopLevelItem const &item, std::ostream &os, int indent, PrintListener *listener = nullptr)
{
	item | match
	{
		[&](ProcDef *def)
		{
			if(listener) listener->before_proc(os, def);

			os << "proc " << def->name << "(";
			os << RangeFmt(def->type.params, ", ", [&](Parameter const &p)
			{
				os << p.name << ": " << p.type;
			});
			os << ") -> " << def->type.ret;

			if(def->body)
			{
				os << "\n";
				print(os, *def->body, 0, listener);
			}
			else
				os << ";";

			if(listener) listener->after_proc(os, def);
		},
		[&](StructDef *def)
		{
			def->parent | match
			{
				[&](CaseMemberOf)
				{
					os << string(indent*INDENT_WIDTH, ' ') << "case " << def->name;
				},
				[&](ExtensionOf parent)
				{
					os << string(indent*INDENT_WIDTH, ' ') << "struct " << def->name << " extends " << parent.struct_->name;
				},
				[&](NoParent)
				{
					os << string(indent*INDENT_WIDTH, ' ') << "struct " << def->name;
				},
			};

			if(def->members.empty())
				os << " {}";
			else
			{
				os << " \n";
				os << string(indent*INDENT_WIDTH, ' ') << "{\n";
				os << RangeFmt(def->members, "\n", [&](Member const &member)
				{
					member | match
					{
						[&](Parameter const &var_member)
						{
							os << string((indent+1)*INDENT_WIDTH, ' ') << var_member.name << ": " << var_member.type << ",";
						},
						[&](StructDef *case_member)
						{
							print(case_member, os, indent+1, listener);
							os << ",";
						},
					};
				});
				os << "\n" << string(indent*INDENT_WIDTH, ' ') << "}";
			}
		},
		[&](AliasDef *def)
		{
			os << "typealias " << def->name << " " << def->type;
		},
	};
}


std::ostream& print(std::ostream &os, Module const &mod, PrintListener *listener = nullptr)
{
	for(TopLevelItem item: mod.items())
	{
		print(item, os, 0, listener);
		os << "\n";
	}

	return os;
}

std::ostream& operator << (std::ostream &os, Module const &mod)
{
	return print(os, mod);
}


//==============================================================================
// Parser
//==============================================================================
class Parser
{
public:
	explicit Parser(std::vector<Token> tokens) :
		m_tokens{std::move(tokens)} {}

	Token const& tok() const
	{
		assert(m_pos < m_tokens.size());
		return m_tokens[m_pos];
	}

	optional<Lexeme> peek(size_t i = 0)
	{
		if(m_pos + i < m_tokens.size())
			return m_tokens[m_pos + i].kind;

		return nullopt;
	}

	optional<Lexeme> tok_kind() const
	{
		if(m_pos < m_tokens.size())
			return m_tokens[m_pos].kind;

		return nullopt;
	}

	Token const& next()
	{
		assert(m_pos < m_tokens.size());
		return m_tokens[m_pos++];
	}

	bool done() const { return m_pos == m_tokens.size(); }

	SourceLocation prev_loc() const
	{
		assert(m_pos > 0 && m_pos <= m_tokens.size());
		return m_tokens[m_pos - 1].span.end;
	}

	SourceLocation location() const
	{
		assert(m_pos < m_tokens.size());
		return m_tokens[m_pos].span.begin;
	}

private:
	std::vector<Token> m_tokens;
	size_t m_pos = 0;
};


inline void require_not_done(Parser &parser, std::string const &expected)
{
	if(parser.done())
		throw ParseError("Expected " + expected + " but reached end of file");
}

inline Token consume(Parser &parser, Lexeme kind)
{
	require_not_done(parser, std::string(str(kind)));
	if(parser.tok().kind != kind)
		throw ParseError(str(parser.location()) + ": Expected "s + str(kind) + ", got " + str(parser.tok().kind));

	return parser.next();
}

inline optional<Token> try_consume(Parser &parser, Lexeme kind)
{
	if(parser.done() || parser.tok().kind != kind)
		return std::nullopt;

	return parser.next();
}


//--------------------------------------------------------------------
// Types
//--------------------------------------------------------------------
Type parse_type(Parser &parser, Module &mod)
{
	require_not_done(parser, "type");
	Token const &tok = parser.next();
	switch(tok.kind)
	{
		case Lexeme::IDENTIFIER:
		{
			QName qname{string(tok.text)};
			while(try_consume(parser, Lexeme::DOT))
				qname.push_back(string(consume(parser, Lexeme::IDENTIFIER).text));

			if(qname.size() == 1)
			{
				if(qname.front() == TYPE_NEVER_NAME)
					return BuiltinType::NEVER;
				else if(qname.front() == TYPE_UNIT_NAME)
					return BuiltinType::UNIT;
				else if(qname.front() == "bool")
					return BuiltinType::BOOL;
				else if(qname.front() == "i8")
					return BuiltinType::I8;
				else if(qname.front() == "u8")
					return BuiltinType::U8;
				else if(qname.front() == "i32")
					return BuiltinType::I32;
				else if(qname.front() == "u32")
					return BuiltinType::U32;
				else if(qname.front() == TYPE_ISIZE_NAME)
					return BuiltinType::ISIZE;
				else if(qname.front() == TYPE_USIZE_NAME)
					return BuiltinType::USIZE;

				if(TypeVarDef *tvar = mod.scope()->try_lookup_type_var(qname.front()))
					return VarType(tvar);
			}

			if(StructDef *def = mod.scope()->try_lookup_def<StructDef>(qname))
				return StructType(def);

			if(AliasDef *def = mod.scope()->try_lookup_def<AliasDef>(qname))
				return AliasType(def);

			return UnresolvedId(qname);
		}

		case Lexeme::CIRCUMFLEX:
		{
			Mutability mutability = try_consume(parser, Lexeme::MUT) ? Mutability::MUTABLE : Mutability::CONST;
			Type target_type = parse_type(parser, mod);
			return PointerType(std::move(target_type), mutability);
		}

		case Lexeme::LEFT_BRACKET:
		{
			consume(parser, Lexeme::CIRCUMFLEX);
			consume(parser, Lexeme::RIGHT_BRACKET);
			Mutability mutability = try_consume(parser, Lexeme::MUT) ? Mutability::MUTABLE : Mutability::CONST;
			Type element_type = parse_type(parser, mod);
			return ManyPointerType(std::move(element_type), mutability);
		}

		case Lexeme::PROC:
		{
			ProcTypeDef proc_type;
			consume(parser, Lexeme::LEFT_PAREN);
			while(parser.peek() != Lexeme::RIGHT_PAREN)
			{
				proc_type.params.push_back(Parameter("", parse_type(parser, mod), nullptr));
				if(parser.peek() != Lexeme::RIGHT_PAREN)
					consume(parser, Lexeme::COMMA);
			}
			consume(parser, Lexeme::RIGHT_PAREN);
			consume(parser, Lexeme::THIN_ARROW);
			proc_type.ret = parse_type(parser, mod);

			return ProcType(mod.add_proc_type(std::move(proc_type)));
		}

		default: throw ParseError("Invalid token while parsing type: "s + str(tok.kind));
	}
}


//--------------------------------------------------------------------
// Expressions
//--------------------------------------------------------------------
Expr parse_prefix_expr(Parser &parser, Module &mod);
Expr parse_infix_expr(Parser &parser, Module &mod, Expr &&left);

BinaryOp to_binary_op(Lexeme tok)
{
	switch(tok)
	{
		case Lexeme::PLUS: return BinaryOp::ADD;
		case Lexeme::MINUS: return BinaryOp::SUB;
		case Lexeme::STAR: return BinaryOp::MUL;
		case Lexeme::SLASH: return BinaryOp::DIV;
		default: UNREACHABLE;
	}
}


enum class Associativity
{
	LEFT,
	RIGHT,
};

struct OperatorInfo
{
	int precedence;
	Associativity assoc;

	bool tighter_than(OperatorInfo other) { return precedence < other.precedence; }
};

constexpr OperatorInfo NO_PREVIOUS_OP = {.precedence = INT_MAX, .assoc = Associativity::LEFT};

optional<OperatorInfo> get_operator_info(Lexeme tok)
{
	switch(tok)
	{
		// Member access
		case Lexeme::DOT:
			return OperatorInfo{.precedence = 0, .assoc = Associativity::LEFT};

		// Procedure/constructor calls
		case Lexeme::LEFT_PAREN:
			return OperatorInfo{.precedence = 1, .assoc = Associativity::LEFT};

		// Pointer dereference
		case Lexeme::CIRCUMFLEX:
			return OperatorInfo{.precedence = 2, .assoc = Associativity::LEFT};

		// Indexing
		case Lexeme::LEFT_BRACKET:
			return OperatorInfo{.precedence = 2, .assoc = Associativity::LEFT};

		// Address of
		case Lexeme::AMPERSAND:
			return OperatorInfo{.precedence = 3, .assoc = Associativity::LEFT};

		// Logical NOT
		case Lexeme::NOT:
			return OperatorInfo{.precedence = 4, .assoc = Associativity::LEFT};

		// Multiplication/division
		case Lexeme::STAR:
		case Lexeme::SLASH:
			return OperatorInfo{.precedence = 5, .assoc = Associativity::LEFT};

		// Addition/subtraction
		case Lexeme::PLUS:
		case Lexeme::MINUS:
			return OperatorInfo{.precedence = 6, .assoc = Associativity::LEFT};

		// Casting
		case Lexeme::AS:
			return OperatorInfo{.precedence = 7, .assoc = Associativity::LEFT};

		// Assignment
		case Lexeme::EQ:
			return OperatorInfo{.precedence = 8, .assoc = Associativity::RIGHT};

		default:
			return nullopt;
	}
}


Expr parse_expr(Parser &parser, Module &mod, OperatorInfo prev_op = NO_PREVIOUS_OP)
{
	Expr expr = parse_prefix_expr(parser, mod);
	while(!parser.done())
	{
		optional<OperatorInfo> next_op = get_operator_info(parser.tok().kind);
		if(!next_op)
			break;

		if(
			prev_op.tighter_than(*next_op) ||
			(prev_op.precedence == next_op->precedence && next_op->assoc == Associativity::LEFT)
		)
			break;

		expr = parse_infix_expr(parser, mod, std::move(expr));
	}

	return expr;
}


#define with_location(E) Expr(E, {tok.span.begin, parser.prev_loc()})

Expr parse_prefix_expr(Parser &parser, Module &mod)
{
	require_not_done(parser, "expression");
	Token const &tok = parser.next();
	switch(tok.kind)
	{
		case Lexeme::IDENTIFIER:
		{
			QName qname{string(tok.text)};
			while(try_consume(parser, Lexeme::DOT))
				qname.push_back(string(consume(parser, Lexeme::IDENTIFIER).text));

			// Try parse variable / member access
			if(VarDef *var = mod.scope()->try_lookup_var(qname.front()))
			{
				Expr expr = with_location(VarExpr(var));
				for(auto member_it = qname.begin() + 1; member_it != qname.end(); ++member_it)
					expr = with_location(MemberAccessExpr(std::move(expr), *member_it));

				return expr;
			}

			return with_location(UnresolvedId(std::move(qname)));
		}

		case Lexeme::LEFT_PAREN:
		{
			Expr sub_expr = parse_expr(parser, mod);
			consume(parser, Lexeme::RIGHT_PAREN);
			return sub_expr;
		}

		case Lexeme::INT_LITERAL:
			return with_location(IntLiteralExpr(XInt64(tok.int_val)));

		case Lexeme::TRUE:
			return with_location(BoolLiteralExpr(true));

		case Lexeme::FALSE:
			return with_location(BoolLiteralExpr(false));

		case Lexeme::C_STRING_LITERAL:
			return with_location(StringLiteralExpr(StringLiteralType::C, tok.text));

		case Lexeme::NOT:
		{
			Expr sub_expr = parse_expr(parser, mod, *get_operator_info(tok.kind));
			return with_location(UnaryExpr(std::move(sub_expr), UnaryOp::NOT));
		}

		case Lexeme::AMPERSAND:
		{
			Mutability mutability = try_consume(parser, Lexeme::MUT) ? Mutability::MUTABLE : Mutability::CONST;
			Expr sub_expr = parse_expr(parser, mod, *get_operator_info(tok.kind));
			return with_location(AddressOfExpr(std::move(sub_expr), mutability));
		}

		case Lexeme::SIZE_OF:
		{
			consume(parser, Lexeme::LEFT_PAREN);
			Type type = parse_type(parser, mod);
			consume(parser, Lexeme::RIGHT_PAREN);

			return with_location(SizeOfExpr(std::move(type)));
		}

		case Lexeme::MAKE:
		{
			Expr init = parse_expr(parser, mod);
			consume(parser, Lexeme::AT);
			Expr addr = parse_expr(parser, mod);

			return with_location(MakeExpr(
				std::make_unique<Expr>(std::move(init)),
				std::make_unique<Expr>(std::move(addr))
			));
		}

		default:
			throw ParseError("Invalid token while parsing expression: "s + str(tok.kind));
	}
}

Expr parse_infix_expr(Parser &parser, Module &mod, Expr &&left)
{
	Token const &tok = parser.next();
	switch(tok.kind)
	{
		case Lexeme::DOT:
		{
			string_view member_name = consume(parser, Lexeme::IDENTIFIER).text;
			return with_location(MemberAccessExpr(std::move(left), string(member_name)));
		}

		case Lexeme::PLUS:
		case Lexeme::MINUS:
		case Lexeme::STAR:
		case Lexeme::SLASH:
		{
			return with_location(BinaryExpr(
				std::move(left),
				parse_expr(parser, mod, *get_operator_info(tok.kind)),
				to_binary_op(tok.kind)
			));
		}

		case Lexeme::EQ:
		{
			return with_location(AssignmentExpr(
				std::move(left),
				parse_expr(parser, mod, *get_operator_info(tok.kind))
			));
		}

		case Lexeme::LEFT_PAREN:
		{
			vector<Argument> args;
			while(parser.peek() != Lexeme::RIGHT_PAREN)
			{
				if(try_consume(parser, Lexeme::DOT))
				{
					string_view param_name = consume(parser, Lexeme::IDENTIFIER).text;
					consume(parser, Lexeme::EQ);
					Expr arg_expr = parse_expr(parser, mod);
					args.push_back(Argument{.expr = std::make_unique<Expr>(std::move(arg_expr)), .name = string(param_name)});
				}
				else
					args.push_back(Argument{.expr = std::make_unique<Expr>(parse_expr(parser, mod)), .name = nullopt});

				if(parser.peek() != Lexeme::RIGHT_PAREN)
					consume(parser, Lexeme::COMMA);
			}
			consume(parser, Lexeme::RIGHT_PAREN);

			return with_location(CallExpr(std::move(left), std::move(args)));
		}

		case Lexeme::CIRCUMFLEX: return with_location(DerefExpr(std::move(left)));

		case Lexeme::LEFT_BRACKET:
		{
			Expr idx_expr = parse_expr(parser, mod);
			consume(parser, Lexeme::RIGHT_BRACKET);
			return with_location(IndexExpr(std::move(left), std::move(idx_expr)));
		}

		case Lexeme::AS: return with_location(AsExpr(std::move(left), parse_type(parser, mod)));

		default: UNREACHABLE;
	}
}

#undef with_location


//--------------------------------------------------------------------
// Patterns
//--------------------------------------------------------------------
PatternOp parse_pattern_op(Parser &parser)
{
	if(try_consume(parser, Lexeme::CIRCUMFLEX))
		return DerefPatternOp();

	if(try_consume(parser, Lexeme::AMPERSAND))
	{
		Mutability mutability = Mutability::CONST;
		if(try_consume(parser, Lexeme::MUT))
			mutability = Mutability::MUTABLE;

		return AddressOfPatternOp(mutability);
	}

	return NoPatternOp();
}

Pattern parse_pattern(Parser &parser, Module &mod)
{
	require_not_done(parser, "pattern");
	switch(parser.tok().kind)
	{
		case Lexeme::MUT:
		case Lexeme::IDENTIFIER:
		{
			Mutability mutability = Mutability::CONST;
			if(try_consume(parser, Lexeme::MUT))
				mutability = Mutability::MUTABLE;

			string_view ident = consume(parser, Lexeme::IDENTIFIER).text;
			VarDef *var = mod.scope()->declare_var(string(ident), mutability, VarKind::LOCAL);
			PatternOp pattern_op = parse_pattern_op(parser);

			if(try_consume(parser, Lexeme::COLON))
				var->type = parse_type(parser, mod);

			return Pattern(VarPattern(var), pattern_op);
		}

		default:
			throw ParseError("Invalid lexeme while parsing pattern");
	}
}


//--------------------------------------------------------------------
// Statements
//--------------------------------------------------------------------
enum class ScopePolicy
{
	REUSE_SCOPE,
	NEW_SCOPE,
};

BlockStmt parse_block_stmt(Parser &parser, Module &mod, ScopePolicy policy = ScopePolicy::NEW_SCOPE);

Stmt parse_stmt(Parser &parser, Module &mod)
{
	require_not_done(parser, "statement");
	switch(parser.tok().kind)
	{
		case Lexeme::LET:
		{
			parser.next();
			Pattern lhs = parse_pattern(parser, mod);
			consume(parser, Lexeme::EQ);
			Expr init_expr = parse_expr(parser, mod);
			consume(parser, Lexeme::SEMICOLON);

			return LetStmt(std::move(lhs), std::move(init_expr));
		}

		case Lexeme::RETURN:
		{
			parser.next();
			optional<Expr> ret_expr;
			if(!try_consume(parser, Lexeme::SEMICOLON))
			{
				ret_expr = parse_expr(parser, mod);
				consume(parser, Lexeme::SEMICOLON);
			}

			return ReturnStmt(std::move(ret_expr));
		}

		case Lexeme::IF:
		{
			parser.next();
			Expr cond = parse_expr(parser, mod);
			Stmt then = parse_block_stmt(parser, mod);
			optional<Stmt> else_;
			if(try_consume(parser, Lexeme::ELSE))
				else_ = parse_block_stmt(parser, mod);

			return IfStmt(std::move(cond), std::move(then), std::move(else_));
		}

		case Lexeme::WHILE:
		{
			parser.next();
			Expr cond = parse_expr(parser, mod);
			Stmt body = parse_block_stmt(parser, mod);

			return WhileStmt(std::move(cond), std::move(body));
		}

		case Lexeme::MATCH:
		{
			parser.next();
			Expr subject = parse_expr(parser, mod);
			consume(parser, Lexeme::LEFT_BRACE);
			vector<MatchArm> arms;
			while(parser.peek() != Lexeme::RIGHT_BRACE)
			{
				consume(parser, Lexeme::CASE);
				Type type = parse_type(parser, mod);

				mod.open_scope();
				optional<Pattern> capture;
				if(try_consume(parser, Lexeme::THIN_ARROW))
					capture = parse_pattern(parser, mod);

				Stmt body = parse_block_stmt(parser, mod, ScopePolicy::REUSE_SCOPE);
				arms.emplace_back(std::move(type), std::move(body), std::move(capture));
				mod.close_scope();
			}
			consume(parser, Lexeme::RIGHT_BRACE);

			return MatchStmt(std::move(subject), std::move(arms));
		}

		default:
		{
			Expr expr = parse_expr(parser, mod);
			consume(parser, Lexeme::SEMICOLON);
			return ExprStmt(std::move(expr));
		}
	}
}

BlockStmt parse_block_stmt(Parser &parser, Module &mod, ScopePolicy policy)
{
	vector<OwnPtr<Stmt>> stmts;

	consume(parser, Lexeme::LEFT_BRACE);
	Scope *scope = mod.scope();
	if(policy == ScopePolicy::NEW_SCOPE) scope = mod.open_scope();
	while(!parser.done())
	{
		if(parser.peek() == Lexeme::RIGHT_BRACE)
			break;

		stmts.push_back(std::make_unique<Stmt>(parse_stmt(parser, mod)));
	}
	if(policy == ScopePolicy::NEW_SCOPE) mod.close_scope();
	consume(parser, Lexeme::RIGHT_BRACE);

	return BlockStmt(scope, std::move(stmts));
}


//--------------------------------------------------------------------
// Top-level items
//--------------------------------------------------------------------
ProcDef* parse_proc(Parser &parser, Module &mod)
{
	string_view name = consume(parser, Lexeme::IDENTIFIER).text;
	Scope *proc_scope = mod.open_scope();

	// Parse parameters
	consume(parser, Lexeme::LEFT_PAREN);
	vector<Parameter> params;
	while(parser.peek() != Lexeme::RIGHT_PAREN)
	{
		string_view param_name = consume(parser, Lexeme::IDENTIFIER).text;
		consume(parser, Lexeme::COLON);
		Type param_type = parse_type(parser, mod);
		VarDef *param = mod.scope()->declare_var(string(param_name), Mutability::CONST, VarKind::PARAM);
		param->type = clone(param_type);

		NullableOwnPtr<Expr> default_value;
		if(try_consume(parser, Lexeme::EQ))
			default_value = std::make_unique<Expr>(parse_expr(parser, mod));

		params.push_back(Parameter(string(param_name), std::move(param_type), std::move(default_value)));

		if(parser.peek() != Lexeme::RIGHT_PAREN)
			consume(parser, Lexeme::COMMA);
	}
	consume(parser, Lexeme::RIGHT_PAREN);

	// Parse return type
	Type ret_type = BuiltinType::UNIT;
	if(try_consume(parser, Lexeme::THIN_ARROW))
		ret_type = parse_type(parser, mod);

	// Parse body
	optional<Stmt> body;
	if(!try_consume(parser, Lexeme::SEMICOLON))
		body = parse_block_stmt(parser, mod, ScopePolicy::REUSE_SCOPE);
	mod.close_scope();

	return mod.scope()->add_proc(string(name), std::move(params), std::move(ret_type), proc_scope, std::move(body));
}

void gather_initial_params(StructDef *struct_, vector<Parameter> &result)
{
	struct_->parent | match
	{
		[&](CaseMemberOf parent) { gather_initial_params(parent.struct_, result); },
		[&](ExtensionOf) { assert(!"TODO: gather_initial_params: ExtensionOf"); },
		[&](NoParent) {},
	};

	for(Member const &m: struct_->initial_var_members())
		result.push_back(std::get<Parameter>(m));
}

void gather_trailing_params(StructDef *struct_, vector<Parameter> &result)
{
	for(Member const &m: struct_->trailing_var_members())
		result.push_back(std::get<Parameter>(m));

	struct_->parent | match
	{
		[&](CaseMemberOf parent) { gather_trailing_params(parent.struct_, result); },
		[&](ExtensionOf) { assert(!"TODO: gather_trailing_params: ExtensionOf"); },
		[&](NoParent) {},
	};
}

void init_constructor_params(StructDef *struct_)
{
	if(struct_->num_cases == 0)
	{
		struct_->constructor = Constructor();
		gather_initial_params(struct_, struct_->constructor->params);
		gather_trailing_params(struct_, struct_->constructor->params);
	}
	else
	{
		for(Member const &m: struct_->case_members())
			init_constructor_params(std::get<StructDef*>(m));
	}
}

StructDef* parse_struct(Parser &parser, Module &mod, ParentRelation parent = NoParent())
{
	string_view name = consume(parser, Lexeme::IDENTIFIER).text;
	StructDef *struct_ = mod.scope()->add_struct(string(name), mod.open_scope());
	struct_->parent = parent;

	if(try_consume(parser, Lexeme::SINGLE_QUOTE))
	{
		string_view type_var_name = consume(parser, Lexeme::IDENTIFIER).text;
		TypeVarDef *type_var = struct_->scope->declare_type_var(string(type_var_name));
		struct_->type_params.push_back(type_var);
	}

	// Parse members
	consume(parser, Lexeme::LEFT_BRACE);
	while(parser.peek() != Lexeme::RIGHT_BRACE)
	{
		if(try_consume(parser, Lexeme::CASE))
		{
			StructDef *case_member = parse_struct(parser, mod, CaseMemberOf(struct_, struct_->num_cases));
			struct_->members.push_back(case_member);
			struct_->num_cases += 1;
		}
		else
		{
			string_view member_name = consume(parser, Lexeme::IDENTIFIER).text;
			consume(parser, Lexeme::COLON);
			VarDef *member_var = mod.scope()->declare_var(string(member_name), Mutability::MUTABLE, VarKind::FIELD);
			member_var->type = parse_type(parser, mod);

			NullableOwnPtr<Expr> init_expr;
			if(try_consume(parser, Lexeme::EQ))
				init_expr = std::make_unique<Expr>(parse_expr(parser, mod));
			struct_->members.push_back(Parameter(member_var->name, clone(*member_var->type), std::move(init_expr)));

			if(struct_->num_cases == 0)
				struct_->num_initial_var_members += 1;
		}

		if(parser.peek() != Lexeme::RIGHT_BRACE)
			consume(parser, Lexeme::COMMA);
	}
	consume(parser, Lexeme::RIGHT_BRACE);
	mod.close_scope();

	if(is<NoParent>(parent))
		init_constructor_params(struct_);

	return struct_;
}


void parse_top_level_item(Parser &parser, Module &mod)
{
	require_not_done(parser, "top-level item");
	Token tok = parser.next();
	switch(tok.kind)
	{
		case Lexeme::PROC: mod.add_item(parse_proc(parser, mod)); break;
		case Lexeme::STRUCT: mod.add_item(parse_struct(parser, mod)); break;

		case Lexeme::TYPEALIAS:
		{
			string_view alias_name = consume(parser, Lexeme::IDENTIFIER).text;
			consume(parser, Lexeme::EQ);
			Type type = parse_type(parser, mod);
			consume(parser, Lexeme::SEMICOLON);
			AliasDef *def = mod.scope()->add_alias(string(alias_name), std::move(type));
			mod.add_item(def);
		} break;

		case Lexeme::EXTERN:
		{
			consume(parser, Lexeme::PROC);
			ProcDef *proc = parse_proc(parser, mod);
			if(proc->body)
				throw ParseError("Extern procedures must not have a body");

			mod.add_item(proc);
		} break;

		default: throw ParseError("Invalid token while parsing top-level item: "s + str(parser.tok().kind));
	}
}


Module parse_module(Parser &parser)
{
	Module mod;
	while(!parser.done())
		parse_top_level_item(parser, mod);

	return mod;
}


//==============================================================================
// Semantic analysis
//==============================================================================
void resolve_identifiers(Module &mod);
void typecheck(Module &mod);
void compute_type_layouts(Module &mod);

void sema(Module &mod)
{
	resolve_identifiers(mod);
	compute_type_layouts(mod);
	typecheck(mod);
}


//--------------------------------------------------------------------
// Type relations
//--------------------------------------------------------------------
BuiltinType smallest_int_type_for(XInt64 val)
{
	if(val.is_negative())
	{
		if(-128 <= val.as_signed())
			return BuiltinType::I8;

		if(-2147483648 <= val.as_signed())
			return BuiltinType::I32;

		return BuiltinType::ISIZE;
	}
	else
	{
		if(val.as_unsigned() <= 127)
			return BuiltinType::I8;

		if(val.as_unsigned() <= 255)
			return BuiltinType::U8;

		if(val.as_unsigned() <= 2147483647)
			return BuiltinType::U32;

		return BuiltinType::USIZE;
	}
}

bool is_integral_type(BuiltinType type)
{
	switch(type)
	{
		case BuiltinType::NEVER: return false;
		case BuiltinType::UNIT:  return false;
		case BuiltinType::BOOL:  return false;
		case BuiltinType::I8:   return true;
		case BuiltinType::U8:   return true;
		case BuiltinType::I32:   return true;
		case BuiltinType::U32:   return true;
		case BuiltinType::ISIZE: return true;
		case BuiltinType::USIZE: return true;
	}

	UNREACHABLE;
}

bool is_signed(BuiltinType type)
{
	assert(is_integral_type(type));

	switch(type)
	{
		case BuiltinType::NEVER: assert(!"is_signed: NEVER");
		case BuiltinType::UNIT:  assert(!"is_signed: UNIT");
		case BuiltinType::BOOL:  assert(!"is_signed: BOOL");
		case BuiltinType::I8:  return false;
		case BuiltinType::U8:  return false;
		case BuiltinType::I32:   return true;
		case BuiltinType::U32:   return true;
		case BuiltinType::ISIZE: return true;
		case BuiltinType::USIZE: return false;
	}

	UNREACHABLE;
}

MemoryLayout get_layout(BuiltinType type)
{
	switch(type)
	{
		case BuiltinType::NEVER: return {.size = 0, .alignment = 0};
		case BuiltinType::UNIT:  return {.size = 1, .alignment = 1};
		case BuiltinType::BOOL:  return {.size = 1, .alignment = 1};
		case BuiltinType::I8:    return {.size = 1, .alignment = 1};
		case BuiltinType::U8:    return {.size = 1, .alignment = 1};
		case BuiltinType::I32:   return {.size = 4, .alignment = 4};
		case BuiltinType::U32:   return {.size = 4, .alignment = 4};
		case BuiltinType::ISIZE: return {.size = 8, .alignment = 8};
		case BuiltinType::USIZE: return {.size = 8, .alignment = 8};
	}

	UNREACHABLE;
}

size_t size_of(BuiltinType type)
{
	return get_layout(type).size;
}

bool builtin_losslessly_convertible(BuiltinType dest, BuiltinType src)
{
	if(!is_integral_type(dest) || !is_integral_type(src))
		return dest == src;

	bool dest_is_signed = is_signed(dest);
	bool src_is_signed = is_signed(src);
	if(dest_is_signed)
	{
		if(src_is_signed)
			// signed <- signed
			return size_of(dest) >= size_of(src);

		// signed <- unsigned
		return size_of(dest) >= size_of(src) + 1;
	}
	else
	{
		if(src_is_signed)
			// unsigned <- signed
			return false;

		// unsigned <- unsigned
		return size_of(dest) >= size_of(src);
	}
}


Type const* strip_alias(Type const &type)
{
	Type const *stripped = &type;
	while(AliasType const *alias = std::get_if<AliasType>(stripped))
		stripped = &alias->def->type;

	return stripped;
}

Type strip_alias_rec(Type const &type)
{
	return *strip_alias(type) | match
	{
		[&](BuiltinType const&) { return clone(type); },
		[&](VarType const&) { return clone(type); },
		[&](PointerType const &t) -> Type
		{
			return PointerType(strip_alias_rec(*t.target_type), t.mutability);
		},
		[&](ManyPointerType const &t) -> Type
		{
			return ManyPointerType(strip_alias_rec(*t.element_type), t.mutability);
		},
		[&](StructType const&) { return clone(type); },
		[&](ProcType const&) { return clone(type); },
		[&](AliasType const&) -> Type { UNREACHABLE; },
		[&](UnresolvedId const&) -> Type { assert(!"strip_alias_rec: UnresolvedId"); },
	};
}

bool equiv(Type const &a, Type const &b)
{
	return *strip_alias(a) | match
	{
		[&](BuiltinType const &ta)
		{
			if(BuiltinType const *tb = std::get_if<BuiltinType>(strip_alias(b)))
				return ta == *tb;

			return false;
		},
		[&](VarType const &ta)
		{
			if(VarType const *tb = std::get_if<VarType>(strip_alias(b)))
				return ta.var == tb->var;

			return false;
		},
		[&](PointerType const &ta)
		{
			if(PointerType const *tb = std::get_if<PointerType>(strip_alias(b)))
				return ta.mutability == tb->mutability && equiv(*ta.target_type, *tb->target_type);

			return false;
		},
		[&](ManyPointerType const &ta)
		{
			if(ManyPointerType const *tb = std::get_if<ManyPointerType>(strip_alias(b)))
				return ta.mutability == tb->mutability && equiv(*ta.element_type, *tb->element_type);

			return false;
		},
		[&](StructType const &ta)
		{
			if(StructType const *tb = std::get_if<StructType>(strip_alias(b)))
				return ta.def == tb->def;

			return false;
		},
		[&](ProcType const &ta)
		{
			if(ProcType const *tb = std::get_if<ProcType>(strip_alias(b)))
				return ta.canonical_def == tb->canonical_def;

			return false;
		},
		[&](AliasType const&) -> bool { UNREACHABLE; },
		[&](UnresolvedId const&) -> bool { assert(!"equiv: UnresolvedId"); },
	};
}

struct TypeEquiv
{
	bool operator () (Type const &a, Type const &b) const
	{
		return equiv(a, b);
	}
};

namespace std
{
	template<>
	struct hash<::Type>
	{
		size_t operator () (::Type const &type) const
		{
			size_t h = ::compute_hash(type.index());
			*::strip_alias(type) | match
			{
				[&](::BuiltinType const &t)
				{
					::combine_hashes(h, ::compute_hash((int)t));
				},
				[&](::VarType const &t)
				{
					::combine_hashes(h, ::compute_hash(t.var));
				},
				[&](::PointerType const &t)
				{
					::combine_hashes(h, ::compute_hash((int)t.mutability));
					::combine_hashes(h, ::compute_hash(*t.target_type));
				},
				[&](::ManyPointerType const &t)
				{
					::combine_hashes(h, ::compute_hash((int)t.mutability));
					::combine_hashes(h, ::compute_hash(*t.element_type));
				},
				[&](::StructType const &t)
				{
					::combine_hashes(h, ::compute_hash(t.def));
				},
				[&](::ProcType const &t)
				{
					::combine_hashes(h, ::compute_hash(*t.def));
				},
				[&](::AliasType const&) { UNREACHABLE; },
				[&](::UnresolvedId const&) { assert(!"hash<Type>: UnresolvedId"); },
			};

			return h;
		}
	};
}

bool is_struct_assignable(StructDef *dest, StructDef *src)
{
	if(src == dest)
		return true;

	while(src)
	{
		if(src->is_case_member_of(dest))
			return true;

		src = src->try_get_parent();
	}

	return false;
}

bool is_type_assignable(Type const &dest, Type const &src)
{
	return *strip_alias(src) | match
	{
		[&](BuiltinType const &src_t)
		{
			if(BuiltinType const *dest_int_type = std::get_if<BuiltinType>(strip_alias(dest)))
				return builtin_losslessly_convertible(*dest_int_type, src_t);

			return false;
		},
		[&](VarType const &src_t)
		{
			if(VarType const *dest_t = std::get_if<VarType>(strip_alias(dest)))
				return src_t.var == dest_t->var;

			return false;
		},
		[&](PointerType const &src_t)
		{
			if(PointerType const *dest_ptr_type = std::get_if<PointerType>(strip_alias(dest)))
			{
				bool mut_compatible = src_t.is_mutable() || dest_ptr_type->is_const();
				return mut_compatible && equiv(*src_t.target_type, *dest_ptr_type->target_type);
			}

			return false;
		},
		[&](ManyPointerType const &src_t)
		{
			if(ManyPointerType const *dest_ptr_type = std::get_if<ManyPointerType>(strip_alias(dest)))
			{
				bool mut_compatible = src_t.is_mutable() || dest_ptr_type->is_const();
				return mut_compatible && equiv(*src_t.element_type, *dest_ptr_type->element_type);
			}

			return false;
		},
		[&](StructType const &src_t)
		{
			if(StructType const *dest_struct_type = std::get_if<StructType>(strip_alias(dest)))
				return is_struct_assignable(dest_struct_type->def, src_t.def);

			return false;
		},
		[&](ProcType const&) -> bool { assert(!"is_type_assignable: ProcType: TODO"); },
		[&](AliasType const&) -> bool { UNREACHABLE; },
		[&](UnresolvedId const&) -> bool { assert(!"is_type_assignable: UnresolvedId"); },
	};
}


bool is_expr_assignable(Type const &dest, Expr const &src)
{
	if(IntLiteralExpr const *src_val = std::get_if<IntLiteralExpr>(&src))
	{
		if(BuiltinType const *dest_int_type = std::get_if<BuiltinType>(strip_alias(dest)))
			return builtin_losslessly_convertible(*dest_int_type, smallest_int_type_for(src_val->value));
	}

	assert(src.type);
	return is_type_assignable(dest, *src.type);
}


bool is_instantiable(Type const &type)
{
	return *strip_alias(type) | match
	{
		[&](BuiltinType const &t)
		{
			return t != BuiltinType::NEVER;
		},
		[&](VarType const&)
		{
			return true;
		},
		[&](PointerType const&)
		{
			return true;
		},
		[&](ManyPointerType const&)
		{
			return true;
		},
		[&](StructType const &t)
		{
			return !t.def->contains_never;
		},
		[&](ProcType const&) { return false; },
		[&](AliasType const&) -> bool { UNREACHABLE; },
		[&](UnresolvedId const&) -> bool { assert(!"is_instantiable: UnresolvedId"); },
	};
}

MemoryLayout compute_own_layout(StructDef *struct_, unordered_set<StructDef*> &seen);

MemoryLayout compute_layout(Type const &type, unordered_set<StructDef*> &seen)
{
	return *strip_alias(type) | match
	{
		[&](BuiltinType const &t)
		{
			return get_layout(t);
		},
		[&](VarType const&) -> MemoryLayout { assert(!"compute_layout: VarType"); },
		[&](PointerType const&)
		{
			return MemoryLayout{.size = 8, .alignment = 8};
		},
		[&](ManyPointerType const&)
		{
			return MemoryLayout{.size = 8, .alignment = 8};
		},
		[&](StructType const &t)
		{
			return t.def->parent | match
			{
				[&](NoParent)            { return compute_own_layout(t.def, seen); },
				[&](CaseMemberOf parent) { return compute_layout(StructType(parent.struct_), seen); },

				[&](ExtensionOf) -> MemoryLayout { assert(!"TODO: compute_layout(StructType): ExtensionOf"); },
			};
		},
		[&](ProcType const&) -> MemoryLayout { assert(!"compute_layout: ProcType"); },
		[&](AliasType const&) -> MemoryLayout { UNREACHABLE; },
		[&](UnresolvedId const&) -> MemoryLayout { assert(!"compute_layout: UnresolvedId"); },
	};
}

MemoryLayout compute_own_layout(StructDef *struct_, unordered_set<StructDef*> &seen)
{
	if(struct_->sema_done)
		return *struct_->own_layout;

	if(!seen.insert(struct_).second)
		throw ParseError("Cyclic type definition");

	struct_->own_layout = MemoryLayout{};

	// Reserve space for the discriminator
	if(struct_->num_cases > 0)
	{
		struct_->discriminator_type = smallest_int_type_for(XInt64(struct_->num_cases));
		struct_->own_layout->extend(get_layout(*struct_->discriminator_type));
	}

	// Reserve space for initial variable members
	size_t member_idx = 0;
	for(;member_idx < struct_->members.size(); ++member_idx)
	{
		Member &member = struct_->members[member_idx];
		if(Parameter const *var_member = std::get_if<Parameter>(&member))
		{
			MemoryLayout var_layout = compute_layout(var_member->type, seen);
			struct_->own_layout->extend(var_layout);
		}
		else
			break;
	}

	// Reserve space for case members
	if(struct_->num_cases > 0)
	{
		struct_->cases_layout = CasesLayout{};
		struct_->cases_layout->start = struct_->own_layout->size;
		MemoryLayout case_members_layout{};
		for(;member_idx < struct_->members.size(); ++member_idx)
		{
			Member &member = struct_->members[member_idx];
			if(StructDef **case_member = std::get_if<StructDef*>(&member))
			{
				MemoryLayout case_layout = compute_own_layout(*case_member, seen);
				case_members_layout.size = std::max(case_members_layout.size, case_layout.size);
				case_members_layout.alignment = std::max(case_members_layout.alignment, case_layout.alignment);
			}
			else
				break;
		}
		size_t size_before_cases = struct_->own_layout->size;
		struct_->own_layout->extend(case_members_layout);
		struct_->cases_layout->size = struct_->own_layout->size - size_before_cases;
	}

	// Reserve space for trailing variable members (same as for initial variable members above)
	for(;member_idx < struct_->members.size(); ++member_idx)
	{
		Member &member = struct_->members[member_idx];
		if(Parameter const *var_member = std::get_if<Parameter>(&member))
		{
			MemoryLayout var_layout = compute_layout(var_member->type, seen);
			struct_->own_layout->extend(var_layout);
		}
		else {
			UNREACHABLE;
		}
	}

	struct_->sema_done = true;
	return *struct_->own_layout;
}


MemoryLayout compute_layout(Type const &type)
{
	unordered_set<StructDef*> seen;
	return compute_layout(type, seen);
}

void compute_type_layouts(Scope *scope)
{
	for(ItemDef &item: scope->item_defs())
	{
		if(StructDef *struct_ = std::get_if<StructDef>(&item))
		{
			unordered_set<StructDef*> seen;
			compute_own_layout(struct_, seen);
		}
	}
}

void compute_type_layouts(Module &mod)
{
	compute_type_layouts(mod.global());
}

//--------------------------------------------------------------------
// Resolve identifiers
//--------------------------------------------------------------------
void resolve_identifiers(Type &type, Scope *scope)
{
	type | match
	{
		[&](BuiltinType&) {},
		[&](VarType&) {},
		[&](PointerType &t)
		{
			resolve_identifiers(*t.target_type, scope);
		},
		[&](ManyPointerType &t)
		{
			resolve_identifiers(*t.element_type, scope);
		},
		[&](StructType&) {},
		[&](ProcType &t)
		{
			resolve_identifiers(t.def->ret, scope);
			for(Parameter &p: t.def->params)
				resolve_identifiers(p.type, scope);

			t.canonical_def = scope->mod()->canonicalize(clone(*t.def));
		},
		[&](AliasType&) {},
		[&](UnresolvedId &t)
		{
			if(StructDef *def = scope->try_lookup_def<StructDef>(t.qname))
				type = StructType(def);
			else if(AliasDef *def = scope->try_lookup_def<AliasDef>(t.qname))
				type = AliasType(def);
			else
				throw ParseError("Undeclared type: "s + str(RangeFmt(t.qname, ".")));
		},
	};
}

void resolve_identifiers(Expr &expr, Scope *scope)
{
	expr | match
	{
		[&](IntLiteralExpr&) {},
		[&](BoolLiteralExpr&) {},
		[&](StringLiteralExpr&) {},
		[&](UnresolvedId &e)
		{
			if(StructDef *def = scope->try_lookup_def<StructDef>(e.qname))
				expr = Expr(StructExpr(def), expr.span);
			else if(ProcDef *def = scope->try_lookup_def<ProcDef>(e.qname))
				expr = Expr(ProcExpr(def), expr.span);
			else
				throw ParseError("Undeclared identifier: "s + str(RangeFmt(e.qname, ".")));
		},
		[&](VarExpr&) {},
		[&](StructExpr&) {},
		[&](ProcExpr&) {},
		[&](UnaryExpr &e)
		{
			resolve_identifiers(*e.sub, scope);
		},
		[&](BinaryExpr &e)
		{
			resolve_identifiers(*e.left, scope);
			resolve_identifiers(*e.right, scope);
		},
		[&](AddressOfExpr &e)
		{
			resolve_identifiers(*e.object_expr, scope);
		},
		[&](DerefExpr &e)
		{
			resolve_identifiers(*e.ptr_expr, scope);
		},
		[&](IndexExpr &e)
		{
			resolve_identifiers(*e.ptr_expr, scope);
			resolve_identifiers(*e.idx_expr, scope);
		},
		[&](MemberAccessExpr &e)
		{
			resolve_identifiers(*e.object, scope);
		},
		[&](AssignmentExpr &e)
		{
			resolve_identifiers(*e.lhs, scope);
			resolve_identifiers(*e.rhs, scope);
		},
		[&](AsExpr &e)
		{
			resolve_identifiers(*e.src_expr, scope);
			resolve_identifiers(e.target_type, scope);
		},
		[&](CallExpr &e)
		{
			resolve_identifiers(*e.callable, scope);
			for(Argument &arg: e.args)
				resolve_identifiers(*arg.expr, scope);
		},
		[&](SizeOfExpr &e)
		{
			resolve_identifiers(e.type, scope);
		},
		[&](MakeExpr &e)
		{
			resolve_identifiers(*e.init, scope);
			resolve_identifiers(*e.addr, scope);
		},
	};
}

void resolve_identifiers(Stmt &stmt, Scope *scope)
{
	stmt | match
	{
		[&](LetStmt &s)
		{
			resolve_identifiers(s.init_expr, scope);
		},
		[&](ExprStmt &s)
		{
			resolve_identifiers(s.expr, scope);
		},
		[&](BlockStmt &s)
		{
			for(OwnPtr<Stmt> &stmt: s.stmts)
				resolve_identifiers(*stmt, s.scope);
		},
		[&](ReturnStmt &s)
		{
			if(s.ret_expr)
				resolve_identifiers(*s.ret_expr, scope);
		},
		[&](IfStmt &s)
		{
			resolve_identifiers(s.condition, scope);
			resolve_identifiers(*s.then, scope);
			if(s.else_)
				resolve_identifiers(*s.else_, scope);
		},
		[&](WhileStmt &s)
		{
			resolve_identifiers(s.condition, scope);
			resolve_identifiers(*s.body, scope);
		},
		[&](MatchStmt &s)
		{
			resolve_identifiers(s.expr, scope);
			for(MatchArm &arm: s.arms)
			{
				resolve_identifiers(arm.type, scope);
				resolve_identifiers(*arm.stmt, scope);
			}
		},
	};
}

void resolve_identifiers(vector<Parameter> &params, Scope *scope)
{
	for(Parameter &param: params)
	{
		resolve_identifiers(param.type, scope);
		if(param.default_value)
			resolve_identifiers(*param.default_value, scope);
	}
}

void resolve_identifiers(TopLevelItem item, Module &mod)
{
	item | match
	{
		[&](ProcDef *def)
		{
			resolve_identifiers(def->type.params, def->scope->parent());
			if(def->body)
				resolve_identifiers(*def->body, def->scope);

			resolve_identifiers(def->type.ret, def->scope->parent());
		},
		[&](StructDef *def)
		{
			for(Member &member: def->members)
			{
				member | match
				{
					[&](Parameter &var_member)
					{
						resolve_identifiers(var_member.type, def->scope);
						if(var_member.default_value)
							resolve_identifiers(*var_member.default_value, def->scope);
					},
					[&](StructDef *case_member)
					{
						resolve_identifiers(case_member, mod);
					},
				};
			}

			if(def->constructor)
				resolve_identifiers(def->constructor->params, def->scope);
		},
		[&](AliasDef *def)
		{
			resolve_identifiers(def->type, mod.global());
		},
	};
}

void resolve_identifiers(Scope *scope)
{
	for(VarDef &var: scope->var_defs())
	{
		if(var.type)
			resolve_identifiers(*var.type, scope);
	}

	for(OwnPtr<Scope> &child: scope->children())
		resolve_identifiers(child.get());
}

void resolve_identifiers(Module &mod)
{
	for(TopLevelItem item: mod.items())
		resolve_identifiers(item, mod);

	resolve_identifiers(mod.global());
}


//--------------------------------------------------------------------
// Type checking
//--------------------------------------------------------------------
optional<size_t> find_by_name(vector<Parameter> &params, string const &name)
{
	for(size_t i = 0; i < params.size(); ++i)
	{
		if(params[i].name == name)
			return i;
	}

	return nullopt;
}

optional<Mutability> is_lvalue_expr(Expr const &expr)
{
	return expr | match
	{
		[](VarExpr const &e) -> optional<Mutability>
		{
			return e.var->mutability;
		},
		[](DerefExpr const &e) -> optional<Mutability>
		{
			return std::get<PointerType>(*e.ptr_expr->type).mutability;
		},
		[](IndexExpr const &e) -> optional<Mutability>
		{
			return std::get<ManyPointerType>(*e.ptr_expr->type).mutability;
		},
		[](MemberAccessExpr const &e) -> optional<Mutability>
		{
			return is_lvalue_expr(*e.object);
		},

		[](auto const&) -> optional<Mutability> { return nullopt; },
	};
}

bool is_cast_ok(Type const &dest_type, Type const &src_type)
{
	if(equiv(dest_type, src_type))
		return true;

	return *strip_alias(src_type) | match
	{
		[&](BuiltinType const &src_t)
		{
			if(BuiltinType const *dest_t = std::get_if<BuiltinType>(strip_alias(dest_type)))
				return is_integral_type(src_t) && is_integral_type(*dest_t);

			return false;
		},
		[&](VarType const&)
		{
			return false;
		},
		[&](PointerType const &src_ptr_type)
		{
			if(PointerType const *dest_ptr_type = std::get_if<PointerType>(strip_alias(dest_type)))
				return src_ptr_type.is_mutable() || dest_ptr_type->is_const();

			if(ManyPointerType const *dest_ptr_type = std::get_if<ManyPointerType>(strip_alias(dest_type)))
				return src_ptr_type.is_mutable() || dest_ptr_type->is_const();

			return false;
		},
		[&](ManyPointerType const &src_ptr_type)
		{
			if(PointerType const *dest_ptr_type = std::get_if<PointerType>(strip_alias(dest_type)))
				return src_ptr_type.is_mutable() || dest_ptr_type->is_const();

			if(ManyPointerType const *dest_ptr_type = std::get_if<ManyPointerType>(strip_alias(dest_type)))
				return src_ptr_type.is_mutable() || dest_ptr_type->is_const();

			return false;
		},
		[&](StructType const &src_t)
		{
			if(StructType const *dest_t = std::get_if<StructType>(strip_alias(dest_type)))
				return src_t.def->is_case_member_of(dest_t->def);

			return false;
		},
		[&](ProcType const&) -> bool { assert(!"is_cast_ok: ProcType: TODO"); },
		[&](AliasType const&) -> bool { UNREACHABLE; },
		[&](UnresolvedId const&) -> bool { assert(!"is_cast_ok: UnresolvedId"); },
	};
}

bool is_integral_type(Type const &type)
{
	return *strip_alias(type) | match
	{
		[&](BuiltinType const &t) { return is_integral_type(t); },
		[&](auto const&) { return false; },
	};
}

Parameter* find_var_member(StructDef *struct_, string const &field)
{
	for(Member &m: struct_->members)
	{
		Parameter *param = std::get_if<Parameter>(&m);
		if(param && param->name == field)
			return param;
	}

	return struct_->parent | match
	{
		[&](CaseMemberOf parent) { return find_var_member(parent.struct_, field); },
		[&](ExtensionOf parent) { return find_var_member(parent.struct_, field); },
		[&](NoParent) -> Parameter* { return nullptr; },
	};
}

void add_size_to_alloc_call(Expr &addr, Expr &&size_expr);

void typecheck(Expr &expr, Module &mod)
{
	expr | match
	{
		[&](IntLiteralExpr&)
		{
			expr.type = BuiltinType::I32;
		},
		[&](BoolLiteralExpr&)
		{
			expr.type = BuiltinType::BOOL;
		},
		[&](StringLiteralExpr &e)
		{
			switch(e.type)
			{
				case StringLiteralType::C:
					expr.type = ManyPointerType(AliasType(mod.global()->lookup_def<AliasDef>(QName{"c_char"})), Mutability::CONST);
					break;
			}
		},
		[&](UnresolvedId&) { assert(!"typecheck: UnresolvedId"); },
		[&](VarExpr &e)
		{
			assert(e.var->type && "typecheck(VarExpr): VarDef::type is null");
			expr.type = clone(*e.var->type);
		},
		[&](StructExpr &e)
		{
			expr.type = StructType(e.def);
		},
		[&](ProcExpr &e)
		{
			expr.type = ProcType(&e.def->type, mod.canonicalize(clone(e.def->type)));
		},
		[&](UnaryExpr &e)
		{
			typecheck(*e.sub, mod);
			switch(e.op)
			{
				case UnaryOp::NOT:
				{
					if(!equiv(*e.sub->type, BuiltinType::BOOL))
						throw ParseError("NOT expected type bool");

					expr.type = clone(*e.sub->type);
				} break;
			}
		},
		[&](BinaryExpr &e)
		{
			typecheck(*e.left, mod);
			typecheck(*e.right, mod);
			switch(e.op)
			{
				case BinaryOp::ADD:
				case BinaryOp::SUB:
				case BinaryOp::MUL:
				case BinaryOp::DIV:
				{
					if(!is_integral_type(*e.left->type) || !equiv(*e.left->type, *e.right->type))
						throw ParseError("Binary op: expected type equivalent integral types, got " + str(*e.left->type) + " and " + str(*e.right->type));

					expr.type = clone(*e.left->type);
				} break;
			}
		},
		[&](AddressOfExpr &e)
		{
			typecheck(*e.object_expr, mod);
			if(optional<Mutability> mutability = is_lvalue_expr(*e.object_expr))
			{
				if(*mutability == Mutability::MUTABLE || e.mutability == Mutability::CONST)
				{
					expr.type = PointerType(clone(*e.object_expr->type), e.mutability);
					return;
				}

				throw ParseError("Cannot make mutable reference to const object");
			}

			throw ParseError("Can only take address of lvalue expression");
		},
		[&](DerefExpr &e)
		{
			typecheck(*e.ptr_expr, mod);
			if(PointerType const *ptr_type = std::get_if<PointerType>(strip_alias(*e.ptr_expr->type)))
			{
				if(!is_instantiable(*ptr_type->target_type))
					throw ParseError("Cannot deref pointer to a type that contains Never");

				expr.type = clone(*ptr_type->target_type);
				return;
			}

			throw ParseError("Pointer dereference expr requires pointer type");
		},
		[&](IndexExpr &e)
		{
			typecheck(*e.ptr_expr, mod);
			typecheck(*e.idx_expr, mod);
			if(ManyPointerType const *ptr_type = std::get_if<ManyPointerType>(strip_alias(*e.ptr_expr->type)))
			{
				if(is_integral_type(*e.idx_expr->type))
				{
					if(!is_instantiable(*ptr_type->element_type))
						throw ParseError("Cannot index pointer with an element type that contains Never");

					expr.type = clone(*ptr_type->element_type);
					return;
				}

				throw ParseError("Index expr must be of integral type");
			}

			throw ParseError("Indexing expr requires many pointer type");
		},
		[&](MemberAccessExpr &e)
		{
			typecheck(*e.object, mod);
			if(StructType const *struct_type = std::get_if<StructType>(strip_alias(*e.object->type)))
			{
				if(Parameter *param = find_var_member(struct_type->def, e.member))
				{
					expr.type = clone(param->type);
					return;
				}

				throw ParseError("Member access: unknown field: " + e.member);
			}

			throw ParseError("Member access requires object of struct type");
		},
		[&](AssignmentExpr &e)
		{
			typecheck(*e.lhs, mod);
			typecheck(*e.rhs, mod);
			if(!is_expr_assignable(*e.lhs->type, *e.rhs))
				throw ParseError("LHS and RHS have incompatible types in assignment");

			if(is_lvalue_expr(*e.lhs) == Mutability::MUTABLE)
				expr.type = clone(*e.lhs->type);
			else
				throw ParseError("LHS does not denote a mutable lvalue in assignment");
		},
		[&](AsExpr &e)
		{
			typecheck(*e.src_expr, mod);
			if(!is_cast_ok(e.target_type, *e.src_expr->type))
				throw ParseError("Invalid cast");

			expr.type = clone(e.target_type);
		},
		[&](CallExpr &e)
		{
			typecheck(*e.callable, mod);

			*e.callable->type | match
			{
				[&](ProcType &t)
				{
					e.callable_params = &t.def->params;
					expr.type = clone(t.def->ret);
				},
				[&](auto&)
				{
					*e.callable | match
					{
						[&](StructExpr &struct_expr)
						{
							if(!struct_expr.def->constructor)
								throw ParseError("Struct does not provide a constructor: " + struct_expr.def->name);

							e.callable_params = &struct_expr.def->constructor->params;
							expr.type = StructType(struct_expr.def);
						},
						[&](auto&)
						{
							throw ParseError("Expression is not callable");
						},
					};
				},
			};

			if(e.args.size() > e.callable_params->size())
				throw ParseError("Too many arguments");

			unordered_set<size_t> assigned_params;
			bool has_ooo_named_args = false;
			for(size_t i = 0; i < e.args.size(); ++i)
			{
				Argument &arg = e.args[i];
				typecheck(*arg.expr, mod);

				// Find the corresponding parameter depending on whether the argument is named or not
				if(arg.name)
				{
					if(optional<size_t> param_idx_opt = find_by_name(*e.callable_params, *arg.name))
					{
						arg.param_idx = *param_idx_opt;
						if(*param_idx_opt != i)
							has_ooo_named_args = true;
					}
					else
						throw ParseError("Invalid parameter name");
				}
				else
				{
					if(has_ooo_named_args)
						throw ParseError("Positional arguments must come before named ones");

					arg.param_idx = i;
				}

				auto res = assigned_params.insert(*arg.param_idx);
				if(!res.second)
					throw ParseError("Multiple arguments for same parameter");

				Parameter &param = e.callable_params->at(*arg.param_idx);
				if(!is_expr_assignable(param.type, *arg.expr))
					throw ParseError("Procedure/struct argument has invalid type");
			}

			for(size_t param_idx = 0; param_idx < e.callable_params->size(); ++param_idx)
			{
				if(!assigned_params.contains(param_idx))
				{
					if(!e.callable_params->at(param_idx).default_value)
						throw ParseError("Missing value for procedure/struct argument in call");
				}
			}
		},
		[&](SizeOfExpr&)
		{
			expr.type = BuiltinType::USIZE;
		},
		[&](MakeExpr &e)
		{
			typecheck(*e.init, mod);

			add_size_to_alloc_call(*e.addr, Expr(SizeOfExpr(clone(*e.init->type)), {}));
			typecheck(*e.addr, mod);

			expr.type = PointerType(clone(*e.init->type), Mutability::MUTABLE);
		},
	};
}

void add_size_to_alloc_call(Expr &addr, Expr &&size_expr)
{
	if(CallExpr *addr_call = std::get_if<CallExpr>(&addr))
	{
		if(ProcExpr *proc_expr = std::get_if<ProcExpr>(addr_call->callable.get()))
		{
			vector<Parameter> const &params = proc_expr->def->type.params;
			if(
				addr_call->args.size() == 0 &&
				params.size() == 1 &&
				equiv(params[0].type, Type(BuiltinType::USIZE))
			)
			{
				addr_call->args.push_back(Argument{
					.expr = std::make_unique<Expr>(std::move(size_expr)),
					.name = nullopt,
					.param_idx = 0,
				});
			}
		}
	}
}

Type apply_pattern_op(PatternOp const &op, Expr const &expr, Type const &expr_type)
{
	return op | match
	{
		[&](NoPatternOp) { return clone(expr_type); },
		[&](DerefPatternOp)
		{
			if(PointerType const *pointer = std::get_if<PointerType>(strip_alias(expr_type)))
				return clone(*pointer->target_type);

			throw ParseError("Invalid pattern op: target type not a pointer");
		},
		[&](AddressOfPatternOp const &o)
		{
			if(optional<Mutability> mutability = is_lvalue_expr(expr))
			{
				if(*mutability == Mutability::MUTABLE || o.mutability == Mutability::CONST)
					return Type(PointerType(clone(expr_type), o.mutability));

				throw ParseError("Cannot make mutable reference to const object");
			}

			throw ParseError("Can only take address of lvalue expression");
		},
	};
}

void typecheck_pattern(Pattern &lhs_pattern, Expr const &rhs_expr, Type const &rhs_type)
{
	lhs_pattern | match
	{
		[&](VarPattern const &p)
		{
			Type inferred_var_type = apply_pattern_op(lhs_pattern.op, rhs_expr, rhs_type);
			if(p.var->type)
			{
				if(not equiv(*p.var->type, inferred_var_type))
					throw ParseError("Inferred type does not match specified type in let statement");
			}
			else
				p.var->type = std::move(inferred_var_type);
		}
	};
}

void typecheck(Stmt &stmt, ProcDef const *proc, Module &mod)
{
	stmt | match
	{
		[&](LetStmt &s)
		{
			typecheck(s.init_expr, mod);
			typecheck_pattern(s.lhs, s.init_expr, *s.init_expr.type);
		},
		[&](ExprStmt &s)
		{
			typecheck(s.expr, mod);
		},
		[&](BlockStmt &s)
		{
			for(OwnPtr<Stmt> &stmt: s.stmts)
				typecheck(*stmt, proc, mod);
		},
		[&](ReturnStmt &s)
		{
			if(s.ret_expr)
			{
				typecheck(*s.ret_expr, mod);
				if(!is_expr_assignable(proc->type.ret, *s.ret_expr))
					throw ParseError("Return statement must return value of appropriate type");
			}
			else
			{
				if(!equiv(proc->type.ret, BuiltinType::UNIT))
					throw ParseError("Return statement must return value of appropriate type");
			}
		},
		[&](IfStmt &s)
		{
			typecheck(s.condition, mod);
			if(!equiv(*s.condition.type, BuiltinType::BOOL))
				throw ParseError("If condition must be of type bool");

			typecheck(*s.then, proc, mod);
			if(s.else_) typecheck(*s.else_, proc, mod);
		},
		[&](WhileStmt &s)
		{
			typecheck(s.condition, mod);
			if(!equiv(*s.condition.type, BuiltinType::BOOL))
				throw ParseError("Loop condition must be of type bool");

			typecheck(*s.body, proc, mod);
		},
		[&](MatchStmt &s)
		{
			typecheck(s.expr, mod);
			s.subject = *strip_alias(*s.expr.type) | match
			{
				[&](StructType t) { return t.def; },
				[&](auto const &) -> StructDef* { throw ParseError("Match statements only work on structs"); },
			};

			unordered_set<StructDef const*> matched_cases;
			for(MatchArm &arm: s.arms)
			{
				*strip_alias(arm.type) | match
				{
					[&](StructType t)
					{
						t.def->parent | match
						{
							[&](CaseMemberOf parent)
							{
								if(parent.struct_ != s.subject)
									throw ParseError("Case statements must match against the case members of the match subject");
							},
							[](auto const&)
							{
								throw ParseError("Case statements must match against the case members of the match statement");
							},
						};

						arm.struct_ = t.def;
						if(!matched_cases.insert(arm.struct_).second)
							throw ParseError("Duplicate case value");
					},
					[&](auto const &) { throw ParseError("Match statements only work on structs"); },
				};

				if(arm.capture)
					typecheck_pattern(*arm.capture, s.expr, arm.type);

				typecheck(*arm.stmt, proc, mod);
			}

			if(matched_cases.size() != s.subject->num_cases)
				throw ParseError("Match is not exhaustive");
		},
	};
}


void typecheck(TopLevelItem item, Module &mod)
{
	item | match
	{
		[&](ProcDef *def)
		{
			for(Parameter &param: def->type.params)
			{
				if(!param.default_value)
					continue;

				typecheck(*param.default_value, mod);
				if(!is_expr_assignable(param.type, *param.default_value))
					throw ParseError("Parameter default value has incorrect type");
			}

			if(def->body)
				typecheck(*def->body, def, mod);
		},
		[&](StructDef *def)
		{
			enum MemberListState
			{
				INITIAL_VAR_MEMBERS,
				CASE_MEMBERS,
				TRAILING_VAR_MEMBERS,
			};

			MemberListState state = INITIAL_VAR_MEMBERS;
			for(Member &member: def->members)
			{
				member | match
				{
					[&](Parameter const &var_member)
					{
						if(state == CASE_MEMBERS)
							state = TRAILING_VAR_MEMBERS;

						if(var_member.default_value)
						{
							typecheck(*var_member.default_value, mod);
							if(!is_expr_assignable(var_member.type, *var_member.default_value))
								throw ParseError("Member default value has incorrect type");
						}

						if(!is_instantiable(var_member.type))
							def->contains_never = true;
					},
					[&](StructDef *case_member)
					{
						if(state == INITIAL_VAR_MEMBERS)
							state =  CASE_MEMBERS;
						else if(state == TRAILING_VAR_MEMBERS)
							throw ParseError("Variable members must come before or after all case members");

						typecheck(case_member, mod);
						if(case_member->contains_never)
							def->contains_never = true;
					},
				};
			}
		},
		[&](AliasDef*) {},
	};
}


void typecheck(Module &mod)
{
	for(TopLevelItem item: mod.items())
		typecheck(item, mod);
}


//==============================================================================
// Backend: C
//==============================================================================
void generate_c(Type const &type, CBackend &backend);

struct LineEnd_Tag {} LineEnd;

class CBackend
{
public:
	explicit CBackend(std::ostream &os) :
		os(os)
	{
		*this << "#include <stdint.h>" << LineEnd;
		*this << "#include <stddef.h>" << LineEnd;
		*this << "#include <stdbool.h>" << LineEnd;
		*this << "#include <limits.h>" << LineEnd;
		*this << "#include <assert.h>" << LineEnd;
		*this << LineEnd;

		// In Myca, `c_char` is defined to be an alias for `i8`.
		// Here, we make sure that this matches the type of `char` in C.
		*this << "#if CHAR_BIT != 8" << LineEnd;
		*this << "    #error \"Unexpected CHAR_BIT\"" << LineEnd;
		*this << "#endif" << LineEnd;
		*this << "#if CHAR_MIN != -128" << LineEnd;
		*this << "    #error \"Unexpected CHAR_MIN\"" << LineEnd;
		*this << "#endif" << LineEnd;
		*this << "#if CHAR_MAX != 127" << LineEnd;
		*this << "    #error \"Unexpected CHAR_MAX\"" << LineEnd;
		*this << "#endif" << LineEnd;

		// This disables warnings generated by GCC when forward-declaring builtin C functions with
		// non-conforming type signatures.
		// This is needed because we define `c_char` as either `signed char` or `unsigned char`,
		// both of which are distinct from `char`.
		// For example, this declaration in Myca
		//
		//     extern proc puts(str: [^]c_char) -> i32;
		//
		// generates the following C code:
		//
		//     typedef int8_t c_char;
		//     int32_t puts(c_char const* str);
		//
		// This differs from the standard definition of `puts(char const *str)`.
		*this << LineEnd;
		*this << "#if defined(__clang__)" << LineEnd;
		*this << "   // Nothing yet" << LineEnd;
		*this << "#elif defined(__GNUC__)" << LineEnd;
		*this << "    #pragma GCC diagnostic ignored \"-Wbuiltin-declaration-mismatch\"" << LineEnd;
		*this << "#endif" << LineEnd;
		*this << LineEnd;

		*this << LineEnd;
		*this << "typedef struct Never { char _; } Never;" << LineEnd;
		*this << LineEnd;
	}

	template<typename T>
	CBackend& operator << (T const &v) requires std::is_integral_v<T> || std::is_same_v<T, std::string> || std::is_array_v<T>
	{
		if(insert_indent)
		{
			os << string(indent_level*4, ' ');
			insert_indent = false;
		}

		os << v;
		return *this;
	}

	// By using a special overload for RangeFmt (instead of using the templated one above) we ensure
	// that we only insert the indention if the range is not empty.
	template<typename RangeT, typename FuncT>
	CBackend& operator << (RangeFmt<RangeT, FuncT> const &r)
	{
		os << r;
		return *this;
	}

	CBackend& operator << (LineEnd_Tag)
	{
		os << "\n";
		insert_indent = true;
		return *this;
	}

	CBackend& operator << (Type const &type)
	{
		generate_c(type, *this);
		return *this;
	}

	void increase_indent() { indent_level += 1; }
	void decrease_indent() { indent_level -= 1; }

	string new_tmp_var()
	{
		return "__myca__tmp" + std::to_string(tmp_var_counter++);
	}

private:
	std::ostream &os;
	int indent_level = 0;
	bool insert_indent = true;
	int tmp_var_counter = 0;
};


//--------------------------------------------------------------------
// Types
//--------------------------------------------------------------------
string get_fq_name(StructDef const *struct_)
{
	return struct_->parent | match
	{
		[&](NoParent)            { return struct_->name; },
		[&](CaseMemberOf parent) { return get_fq_name(parent.struct_) + "__" + struct_->name; },
		[&](ExtensionOf)         { return struct_->name; },
	};
}

string generate_c_to_str(BuiltinType const &type)
{
	switch(type)
	{
		case BuiltinType::NEVER: return "Never";
		case BuiltinType::UNIT: TODO("generate_c(Type): UNIT");
		case BuiltinType::BOOL: return "bool";
		case BuiltinType::I8: return "int8_t";
		case BuiltinType::U8: return "uint8_t";
		case BuiltinType::I32: return "int32_t";
		case BuiltinType::U32: return "int32_t";
		case BuiltinType::USIZE: return "size_t";
		case BuiltinType::ISIZE: return "ptrdiff_t";
	}

	UNREACHABLE;
}

string generate_c_to_str(Type const &type)
{
	return type | match
	{
		[&](BuiltinType const &t) { return generate_c_to_str(t); },
		[&](VarType const&) -> string { assert(!"generate_c_to_str: VarType"); },
		[&](PointerType const &t)
		{
			string type_str = generate_c_to_str(*t.target_type);
			if(t.is_const())
				type_str += " const";

			return type_str + "*";
		},
		[&](ManyPointerType const &t)
		{
			string type_str = generate_c_to_str(*t.element_type);
			if(t.is_const())
				type_str += " const";

			return type_str + "*";
		},
		[&](StructType const &t)
		{
			if(t.def->name == "c_void")
				return "void"s;

			return "struct " + get_fq_name(t.def);
		},
		[&](ProcType const&) -> string { assert(!"generate_c_to_str: ProcType: TODO"); },
		[&](AliasType const &t)
		{
			return t.def->name;
		},
		[&](UnresolvedId const&) -> string { assert(!"generate_c_to_str: UnresolvedId"); },
	};
}

void generate_c(Type const &type, CBackend &backend)
{
	backend << generate_c_to_str(type);
}


//--------------------------------------------------------------------
// Expressions
//--------------------------------------------------------------------
string callee_name(Callable const &callable)
{
	return callable | match
	{
		[&](ProcDef *def) { return def->name; },
		[&](StructDef *struct_) { return "__myca__" + get_fq_name(struct_) + "_create"; },
		[](UnresolvedId) -> string { assert(!"callee_name: UnresolvedId"); },
	};
}

string generate_c(Expr const &expr, CBackend &backend, bool need_result = true);

string generate_c_cast(Type const &target_type, string const &expr, Type const &expr_type)
{
	if(equiv(target_type, expr_type))
		return expr;

	string type_val = generate_c_to_str(target_type);
	return *strip_alias(target_type) | match
	{
		[&](StructType)
		{
			return "*(" + type_val + "*)&(" + expr + ")";
		},
		[&](auto const&)
		{
			return "(" + type_val + ")(" + expr + ")";
		},
	};
}

string generate_c_cast(Type const &target_type, Expr const &expr, CBackend &backend)
{
	return generate_c_cast(target_type, generate_c(expr, backend), *expr.type);
}

string generate_c(Expr const &expr, CBackend &backend, bool need_result)
{
	return expr | match
	{
		[&](IntLiteralExpr const &e)
		{
			return str(e.value);
		},
		[&](BoolLiteralExpr const &e)
		{
			return string(e.value ? "true" : "false");
		},
		[&](StringLiteralExpr const &e)
		{
			switch(e.type)
			{
				case StringLiteralType::C:
					// In C, `char` is a type distinct from `signed char` and `unsigned char`.
					// However, in Myca, we define `c_char` to be equal to either `i8` (aka `signed
					// char`) or `u8` (aka `unsigned char`).
					// Thus, we need to insert a cast to keep the C compiler happy.
					return "(c_char const*)\""s + e.value + '"';
			}

			UNREACHABLE;
		},
		[&](UnresolvedId const&) -> string { assert(!"generate_c: UnresolvedId"); },
		[&](VarExpr const &e)
		{
			return e.var->name;
		},
		[&](StructExpr const &e)
		{
			return "__myca__" + get_fq_name(e.def) + "_create";
		},
		[&](ProcExpr const &e)
		{
			return e.def->name;
		},
		[&](UnaryExpr const &e)
		{
			string sub_val = generate_c(*e.sub, backend);
			switch(e.op)
			{
				case UnaryOp::NOT: return "(!" + sub_val + ")";
			}

			UNREACHABLE;
		},
		[&](BinaryExpr const &e)
		{
			string left_val = generate_c(*e.left, backend);
			string right_val = generate_c(*e.right, backend);
			switch(e.op)
			{
				case BinaryOp::ADD: return "(" + left_val + " + " + right_val + ")";
				case BinaryOp::SUB: return "(" + left_val + " - " + right_val + ")";
				case BinaryOp::MUL: return "(" + left_val + " * " + right_val + ")";
				case BinaryOp::DIV: return "(" + left_val + " / " + right_val + ")";
			}

			UNREACHABLE;
		},
		[&](AddressOfExpr const &e)
		{
			string object_val = generate_c(*e.object_expr, backend);
			return "&(" + object_val + ")";
		},
		[&](DerefExpr const &e)
		{
			string ptr_val = generate_c(*e.ptr_expr, backend);
			return "(*(" + ptr_val + "))";
		},
		[&](IndexExpr const &e)
		{
			string ptr_val = generate_c(*e.ptr_expr, backend);
			string idx_val = generate_c(*e.idx_expr, backend);
			return "(" + ptr_val + ")[" + idx_val + "]";
		},
		[&](MemberAccessExpr const &e)
		{
			string object_val = generate_c(*e.object, backend);
			return object_val + "." + e.member;
		},
		[&](AssignmentExpr const &e)
		{
			string rhs_val = generate_c_cast(*e.lhs->type, *e.rhs, backend);
			string lhs_val = generate_c(*e.lhs, backend);
			return "(" + lhs_val + " = " + rhs_val + ")";
		},
		[&](AsExpr const &e)
		{
			return generate_c_cast(e.target_type, *e.src_expr, backend);
		},
		[&](CallExpr const &e)
		{
			vector<Parameter> const &params = *e.callable_params;

			// Evaluate arguments in the order they were provided
			vector<string> arg_vals(params.size());
			for(Argument const &arg: e.args)
			{
				Type const &param_type = params[*arg.param_idx].type;
				arg_vals[*arg.param_idx] = generate_c_cast(param_type, *arg.expr, backend);
			}

			// Evaluate a parameter's default value if an argument is missing
			for(size_t i = 0; i < params.size(); ++i)
			{
				if(arg_vals[i].empty())
				{
					assert(params[i].default_value);
					arg_vals[i] = generate_c(*params[i].default_value, backend);
				}
			}

			// Create a variable to store the procedure's return value
			Type const &ret_type = *expr.type;
			string result_var;
			if(!equiv(ret_type, BuiltinType::UNIT) && need_result)
			{
				result_var = backend.new_tmp_var();
				backend << ret_type << " " << result_var << " = ";
			}

			// Make the call
			string callable_val = generate_c(*e.callable, backend);
			backend << callable_val << "(";
			backend << RangeFmt(arg_vals, ", ", [&](auto &arg_val) { backend << arg_val; });
			backend << ");" << LineEnd;

			return result_var;
		},
		[&](SizeOfExpr const &e)
		{
			return "sizeof(" + generate_c_to_str(e.type) + ")";
		},
		[&](MakeExpr const &e)
		{
			string addr_val = generate_c(*e.addr, backend);

			string result_var = backend.new_tmp_var();
			backend << *expr.type << " " << result_var << " = " << generate_c_cast(*expr.type, addr_val, *e.addr->type) << ";" << LineEnd;

			string init_val = generate_c(*e.init, backend);
			backend << "*" << result_var << " = " << init_val << ";" << LineEnd;

			return result_var;
		},
	};
}


//--------------------------------------------------------------------
// Statements
//--------------------------------------------------------------------
string apply_pattern_op_to_c_expr(PatternOp const &op, string const &expr)
{
	return op | match
	{
		[&](NoPatternOp) { return expr; },
		[&](DerefPatternOp) { return "*(" + expr + ")"; },
		[&](AddressOfPatternOp const&) { return "&(" + expr + ")"; },
	};
}

void generate_c(Pattern const &lhs_pattern, string const &rhs_expr, Type const &rhs_type, CBackend &backend)
{
	lhs_pattern | match
	{
		[&](VarPattern const &p)
		{
			backend << *p.var->type << " " << p.var->name << " = ";
			string rhs = apply_pattern_op_to_c_expr(lhs_pattern.op, rhs_expr);
			backend	<< generate_c_cast(*p.var->type, rhs, rhs_type) << ";" << LineEnd;
		}
	};
}

void generate_c(Stmt const &stmt, ProcDef const *proc, CBackend &backend)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			string init_expr_var = generate_c(s.init_expr, backend);
			generate_c(s.lhs, init_expr_var, *s.init_expr.type, backend);
		},
		[&](ExprStmt const &s)
		{
			string expr_str = generate_c(s.expr, backend, false);
			if(expr_str.length())
				backend << expr_str << ";" << LineEnd;
		},
		[&](BlockStmt const &s)
		{
			backend << "{" << LineEnd;
			backend.increase_indent();
			for(OwnPtr<Stmt> const &stmt: s.stmts)
				generate_c(*stmt, proc, backend);
			backend.decrease_indent();
			backend << "}" << LineEnd;
		},
		[&](ReturnStmt const &s)
		{
			if(s.ret_expr)
			{
				string ret_val = generate_c_cast(proc->type.ret, *s.ret_expr, backend);
				backend << "return " << ret_val << ";" << LineEnd;
			}
			else
				backend << "return;" << LineEnd;
		},
		[&](IfStmt const &s)
		{
			string cond_str = generate_c(s.condition, backend);
			backend << "if(" << cond_str << ")" << LineEnd;
			generate_c(*s.then, proc, backend);
			if(s.else_)
			{
				backend << "else" << LineEnd;
				generate_c(*s.else_, proc, backend);
				backend << LineEnd;
			}
		},
		[&](WhileStmt const &s)
		{
			backend << "while(true)" << LineEnd;
			backend << "{" << LineEnd;
			backend.increase_indent();
				string cond_str = generate_c(s.condition, backend);
				backend << "if(!" << cond_str << ") break;" << LineEnd;
				generate_c(*s.body, proc, backend);
			backend.decrease_indent();
			backend << "}" << LineEnd;
		},
		[&](MatchStmt const &s)
		{
			string subject_str = generate_c(s.expr, backend);
			backend << "switch(" << subject_str << ".__myca__discr)" << LineEnd;
			backend << "{" << LineEnd;
			backend.increase_indent();
				for(MatchArm const &arm: s.arms)
				{
					backend << "case " << arm.struct_->get_case_idx() << ":" << LineEnd;
					backend << "{" << LineEnd;
					backend.increase_indent();

					if(arm.capture)
						generate_c(*arm.capture, subject_str, *s.expr.type, backend);

					generate_c(*arm.stmt, proc, backend);

					backend.decrease_indent();
					backend << "}" << LineEnd;

					backend << "break;" << LineEnd;
				}
				backend << "default: assert(0);" << LineEnd;
			backend.decrease_indent();
			backend << "}" << LineEnd;
		},
	};
}


//--------------------------------------------------------------------
// Top-level elements
//--------------------------------------------------------------------
struct CMemberNormal { string name; };
struct CMemberConst { string name; size_t value; };
struct CMemberPadding {};

using CMemberKind = variant<CMemberNormal, CMemberConst, CMemberPadding>;

struct CMember
{
	CMemberKind kind;
	Type type;

	CMember(string const &name, Type &&type) :
		kind(CMemberNormal(name)), type(std::move(type)) {}

	CMember(string name, optional<size_t> value, Type &&type) :
		kind(value ?
			CMemberKind(CMemberConst(name, *value)) :
			CMemberKind(CMemberNormal(name))
		),
		type(std::move(type)) {}

	CMember(Type &&type) :
		kind(CMemberPadding()), type(std::move(type)) {}


	CMember(CMember const &rhs) :
		kind(rhs.kind),
		type(clone(rhs.type)) {}
};

struct CStruct
{
	string name;
	MemoryLayout layout{};
	vector<CMember> members;

	explicit CStruct(string const &name) :
		name(name) {}

	size_t add(CMember &&m)
	{
		layout.extend(compute_layout(m.type));
		members.push_back(std::move(m));
		return members.size() - 1;
	}
};


// Generate C struct fields for all the variable members of `struct_` that come before the first
// case member
void create_c_struct_initial_vars(StructDef const *struct_, optional<size_t> child_case_idx, CStruct *result)
{
	struct_->parent | match
	{
		[&](NoParent) {},
		[&](CaseMemberOf parent) { create_c_struct_initial_vars(parent.struct_, parent.case_idx, result); },
		[&](ExtensionOf parent)  { create_c_struct_initial_vars(parent.struct_, nullopt, result); },
	};

	if(struct_->discriminator_type)
		// REVISIT: When the struct contains multiple nested case members then the name is not
		// unique anymore
		result->add(CMember("__myca__discr", child_case_idx, Type(*struct_->discriminator_type)));

	for(Member const &member: struct_->initial_var_members())
	{
		Parameter const &param = std::get<Parameter>(member);
		result->add(CMember(param.name, clone(param.type)));
	}
}

// Generate C struct fields for all the variable members of `struct_` that come after its case members
void create_c_struct_trailing_vars(StructDef const *struct_, CStruct *result)
{
	for(Member const &member: struct_->trailing_var_members())
	{
		Parameter const &param = std::get<Parameter>(member);
		result->add(CMember(param.name, clone(param.type)));
	}

	struct_->parent | match
	{
		[&](NoParent) {},
		[&](CaseMemberOf parent)
		{
			size_t additional_padding = parent.struct_->cases_layout->end() - result->layout.size;
			if(additional_padding)
			{
				for(size_t i = 0; i < additional_padding; ++i)
					result->add(CMember(BuiltinType::U8));
			}

			create_c_struct_trailing_vars(parent.struct_, result);
		},
		[&](ExtensionOf) {},
	};
}

CStruct create_c_struct(StructDef const *struct_)
{
	CStruct result(get_fq_name(struct_));

	create_c_struct_initial_vars(struct_, nullopt, &result);

	if(struct_->cases_layout)
	{
		for(size_t i = 0; i < struct_->cases_layout->size; ++i)
			result.add(CMember(BuiltinType::U8));
	}

	create_c_struct_trailing_vars(struct_, &result);

	return result;
}



void generate_c_struct_def(CStruct const &cstruct, CBackend &backend)
{
	// In Myca, types can have zero alignment (e.g., Never), but this is not allowed in C.
	size_t alignment = std::max(cstruct.layout.alignment, size_t(1));
	backend << "struct " << "__attribute__((aligned(" << alignment << "))) " << cstruct.name << LineEnd << "{" << LineEnd;
	backend.increase_indent();

	for(CMember const &member: cstruct.members)
	{
		backend << member.type << " ";
		member.kind | match
		{
			[&](CMemberNormal const &m) { backend << m.name; },
			[&](CMemberConst const &m) { backend << m.name; },
			[&](CMemberPadding) { backend << backend.new_tmp_var(); },
		};
		backend << ";" << LineEnd;
	}

	backend.decrease_indent();
	backend << "};" << LineEnd;
}

void generate_c_struct_methods(CStruct const &cstruct, CBackend &backend)
{
	// Generate constructor
	{
		// Function header
		backend << "struct " << cstruct.name << " " << "__myca__" << cstruct.name << "_create(";
		bool first = true;
		for(CMember const &member: cstruct.members)
		{
			member.kind | match
			{
				[&](CMemberNormal const &m)
				{
					if(first) first = false;
					else backend << ", ";

					backend << member.type << " " << m.name;
				},
				[&](CMemberConst const&) {},
				[&](CMemberPadding) {},
			};
		}
		backend << ")" << LineEnd;
		
		// Function body
		backend << "{" << LineEnd;
		backend.increase_indent();
			backend << "struct " << cstruct.name << " __myca__object = {";
			backend.increase_indent();
				backend << RangeFmt(cstruct.members, ", ", [&](CMember const &member)
				{
					member.kind | match
					{
						[&](CMemberNormal const &m) { backend << m.name; },
						[&](CMemberConst const &m) { backend << m.value; },
						[&](CMemberPadding) { backend << "0"; },
					};
				});
			backend.decrease_indent();
			backend << "};" << LineEnd;
			backend << "return __myca__object;" << LineEnd;
		backend.decrease_indent();
		backend << "}" << LineEnd;
	}
}


void generate_c_proc_sig(ProcDef const *proc, CBackend &backend)
{
	if(equiv(proc->type.ret, BuiltinType::UNIT))
		backend << "void";
	else
		backend << proc->type.ret;

	backend << " " << proc->name << "(";
	backend << RangeFmt(proc->type.params, ", ", [&](Parameter const &p)
	{
		backend << p.type << " " << p.name;
	});
	backend << ")";
}


//--------------------------------------------------------------------
// Whole module
//--------------------------------------------------------------------

// Dependency computation for structs
//------------------------------------------------
void compute_struct_deps(Type const &type, unordered_set<StructDef*> &deps)
{
	type | match
	{
		[&](BuiltinType const&) {},
		[&](VarType const&) {},
		[&](PointerType const&) {},
		[&](ManyPointerType const&) {},
		[&](StructType const &t)
		{
			for(Member const &member: t.def->members)
			{
				member | match
				{
					[&](Parameter const &var_member)
					{
						compute_struct_deps(var_member.type, deps);
					},
					[&](StructDef *case_member)
					{
						compute_struct_deps(StructType(case_member), deps);
					},
				};
			}

			deps.insert(t.def);
		},
		[&](ProcType const &t)
		{
			for(Parameter const &p: t.def->params)
				compute_struct_deps(p.type, deps);

			compute_struct_deps(t.def->ret, deps);
		},
		[&](AliasType const &t)
		{
			compute_struct_deps(t.def->type, deps);
		},
		[&](UnresolvedId const&) { assert(!"compute_struct_deps: UnresolvedId"); },
	};
}

using StructDepMap = unordered_map<StructDef*, unordered_set<StructDef*>>;

StructDepMap compute_struct_deps_mapping(vector<StructDef*> const &structs)
{
	StructDepMap deps_by_struct;
	for(StructDef *struct_: structs)
	{
		unordered_set<StructDef*> deps;
		compute_struct_deps(StructType(struct_), deps);
		deps.erase(struct_);

		deps_by_struct.emplace(struct_, std::move(deps));
	}

	return deps_by_struct;
}

void _sort_structs_by_deps(
	StructDef *struct_,
	StructDepMap const &deps_by_struct,
	vector<StructDef*> &result,
	unordered_set<StructDef*> &visited
)
{
	// We assume the semantic analysis would already have detected any dependency cycles
	auto res = visited.insert(struct_);
	if(!res.second)
		return;

	auto deps = deps_by_struct.find(struct_);
	if(deps != deps_by_struct.end())
	{
		for(StructDef *dep: deps->second)
			_sort_structs_by_deps(dep, deps_by_struct, result, visited);
	}

	result.push_back(struct_);
}

vector<StructDef*> sort_structs_by_deps(vector<StructDef*> const &structs)
{
	StructDepMap deps_by_struct = compute_struct_deps_mapping(structs);
	vector<StructDef*> result;
	unordered_set<StructDef*> visited;
	for(StructDef *struct_: structs)
		_sort_structs_by_deps(struct_, deps_by_struct, result, visited);

	return result;
}


// Dependency computation for aliases
//------------------------------------------------
// CLEANUP: DRY this up, these functions are almost identical to the one for structs
void compute_alias_deps(Type const &type, unordered_set<AliasDef*> &deps)
{
	type | match
	{
		[&](BuiltinType const&) {},
		[&](VarType const&) {},
		[&](PointerType const &t)
		{
			compute_alias_deps(*t.target_type, deps);
		},
		[&](ManyPointerType const &t)
		{
			compute_alias_deps(*t.element_type, deps);
		},
		[&](StructType const &t)
		{
			for(Member const &member: t.def->members)
			{
				member | match
				{
					[&](Parameter const &var_member)
					{
						compute_alias_deps(var_member.type, deps);
					},
					[&](StructDef *case_member)
					{
						compute_alias_deps(StructType(case_member), deps);
					},
				};
			}
		},
		[&](ProcType const &t)
		{
			for(Parameter const &p: t.def->params)
				compute_alias_deps(p.type, deps);

			compute_alias_deps(t.def->ret, deps);
		},
		[&](AliasType const &t)
		{
			compute_alias_deps(t.def->type, deps);
			deps.insert(t.def);
		},
		[&](UnresolvedId const&) { assert(!"compute_alias_deps: UnresolvedId"); },
	};
}

using AliasDepMap = unordered_map<AliasDef*, unordered_set<AliasDef*>>;

AliasDepMap compute_alias_deps_mapping(vector<AliasDef*> const &aliases)
{
	AliasDepMap deps_by_alias;
	for(AliasDef *alias: aliases)
	{
		unordered_set<AliasDef*> deps;
		compute_alias_deps(AliasType(alias), deps);
		deps.erase(alias);

		deps_by_alias.emplace(alias, std::move(deps));
	}

	return deps_by_alias;
}

void _sort_aliases_by_deps(
	AliasDef *alias,
	AliasDepMap const &deps_by_alias,
	vector<AliasDef*> &result,
	unordered_set<AliasDef*> &visited
)
{
	// We assume the semantic analysis would already have detected any dependency cycles
	auto res = visited.insert(alias);
	if(!res.second)
		return;

	auto deps = deps_by_alias.find(alias);
	if(deps != deps_by_alias.end())
	{
		for(AliasDef *dep: deps->second)
			_sort_aliases_by_deps(dep, deps_by_alias, result, visited);
	}

	result.push_back(alias);
}

vector<AliasDef*> sort_alias_by_deps(vector<AliasDef*> const &aliases)
{
	AliasDepMap deps_by_alias = compute_alias_deps_mapping(aliases);
	vector<AliasDef*> result;
	unordered_set<AliasDef*> visited;
	for(AliasDef *alias: aliases)
		_sort_aliases_by_deps(alias, deps_by_alias, result, visited);

	return result;
}


// Finally generate some correctly ordered code
//------------------------------------------------
void generate_c(Module const &mod, CBackend &backend)
{
	vector<StructDef*> structs;
	vector<ProcDef*> procs;
	vector<AliasDef*> aliases;
	for(TopLevelItem item: mod.items())
	{
		item | match
		{
			[&](ProcDef *def) { procs.push_back(def); },
			[&](StructDef *def) { structs.push_back(def); },
			[&](AliasDef *def) { aliases.push_back(def); },
		};
	}


	// Aliases
	vector<AliasDef*> sorted_aliases = sort_alias_by_deps(aliases);
	for(AliasDef *alias: sorted_aliases)
		backend << "typedef " << alias->type << " " << alias->name << ";" << LineEnd;

	// Structures
	vector<StructDef*> sorted_structs = sort_structs_by_deps(structs);
	unordered_map<StructDef*, CStruct> cstructs;
	for(StructDef *struct_: sorted_structs)
	{
		if(struct_->type_params.size())
			continue;

		CStruct cstruct = create_c_struct(struct_);
		generate_c_struct_def(cstruct, backend);
		backend << LineEnd;

		cstructs.emplace(struct_, std::move(cstruct));
	}

	for(StructDef *struct_: sorted_structs)
	{
		if(struct_->type_params.size())
			continue;

		generate_c_struct_methods(cstructs.at(struct_), backend);
		backend << LineEnd;
	}

	// Procedures
	for(ProcDef *proc: procs)
	{
		generate_c_proc_sig(proc, backend);
		backend << ";" << LineEnd;
	}

	for(ProcDef *proc: procs)
	{
		if(proc->body)
		{
			generate_c_proc_sig(proc, backend);
			backend << LineEnd;
			generate_c(*proc->body, proc, backend);
			backend << LineEnd;
		}
	}
}


//==============================================================================
// Main
//==============================================================================
#define NEXT_ARG() (*++argv)


struct JSON_Int
{
	uint8_t discrimitator;
	uint64_t value;
};

struct JSON_Null
{
	uint8_t discrimitator;
	uint8_t padding[8];
};


int main(int argc, char *argv[])
{
	(void)argc;

	optional<string> arg_input_filename;
	optional<string> arg_output_filename;
	while(NEXT_ARG())
	{
		if(*argv == "-o"sv) {
			if(!NEXT_ARG()) {
				std::cerr << "Error: no output file provided" << std::endl;
				return 1;
			}
			arg_output_filename = *argv;
		}
		else {
			if(arg_input_filename) {
				std::cerr << "Error: input file already provided" << std::endl;
				return 1;
			}
			arg_input_filename = *argv;
		}
	}


	if(!arg_input_filename) {
		std::cerr << "Error: no input file provided" << std::endl;
		return 1;
	}
	if(!arg_output_filename) {
		std::cerr << "Error: no output file provided" << std::endl;
		return 1;
	}

	std::ifstream file(*arg_input_filename);
	if(!file) {
		std::cerr << "Error: cannot read file: " << *arg_input_filename << std::endl;
		return 1;
	}

	std::ostringstream ss;
	ss << file.rdbuf();
	std::string source = std::move(ss).str();
	string_view source_view(source.begin(), source.end());

	try
	{
		std::vector<Token> tokens = tokenize(source_view);
		Parser parser(tokens);
		Module mod = parse_module(parser);
		sema(mod);
		print(std::cout, mod) << std::endl;

		std::cout << "--------------------------------------------------" << std::endl;
		std::stringstream c_code;
		CBackend backend(c_code);
		generate_c(mod, backend);

		std::cout << c_code.str();
		std::ofstream c_file(*arg_output_filename);
		c_file << c_code.str();
	}
	catch(LexingError const &e)
	{
		std::cerr << "Lexing failed: " << e.what() << std::endl;
		return 1;
	}
	catch(ParseError const &e)
	{
		std::cerr << "Parsing failed: " << e.what() << std::endl;
		return 1;
	}
}
