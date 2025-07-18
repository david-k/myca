#include <cstdint>
#include <cassert>
#include <limits>
#include <stdexcept>
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
#include <algorithm>

#include "utils.hpp"


using std::vector;
using std::string;
using std::string_view;
using std::optional;
using std::nullopt;
using std::variant;
using std::pair;
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
	BANG,
	SINGLE_QUOTE,
	AT,
	BAR,
	LEFT_PAREN,
	RIGHT_PAREN,
	LEFT_BRACE,
	RIGHT_BRACE,
	LEFT_BRACKET,
	RIGHT_BRACKET,

	THIN_ARROW,

	PLUS,
	MINUS,
	UNARY_MINUS, // Not actually generated by the lexer. Only used for get_operator_info()
	SLASH,
	STAR,
	EQ,
	COLON_EQ,
	DOUBLE_EQ,
	LT,
	LE,
	GT,
	GE,

	NOT,

	LET,
	PROC,
	STRUCT,
	CASE,
	IMPLICIT,

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
		case Lexeme::BANG: return "BANG";
		case Lexeme::SINGLE_QUOTE: return "SINGLE_QUOTE";
		case Lexeme::AT: return "AT";
		case Lexeme::BAR: return "BAR";
		case Lexeme::LEFT_PAREN: return "LEFT_PAREN";
		case Lexeme::RIGHT_PAREN: return "RIGHT_PAREN";
		case Lexeme::LEFT_BRACE: return "LEFT_BRACE";
		case Lexeme::RIGHT_BRACE: return "RIGHT_BRACE";
		case Lexeme::LEFT_BRACKET: return "LEFT_BRACKET";
		case Lexeme::RIGHT_BRACKET: return "RIGHT_BRACKET";

		case Lexeme::THIN_ARROW: return "THIN_ARROW";

		case Lexeme::PLUS: return "PLUS";
		case Lexeme::MINUS: return "MINUS";
		case Lexeme::UNARY_MINUS: return "UNARY_MINUS";
		case Lexeme::SLASH: return "SLASH";
		case Lexeme::STAR: return "STAR";
		case Lexeme::EQ: return "EQ";
		case Lexeme::COLON_EQ: return "COLON_EQ";
		case Lexeme::DOUBLE_EQ: return "DOUBLE_EQ";
		case Lexeme::LT: return "LT";
		case Lexeme::LE: return "LE";
		case Lexeme::GT: return "GT";
		case Lexeme::GE: return "GE";

		case Lexeme::NOT: return "NOT";

		case Lexeme::LET: return "LET";
		case Lexeme::PROC: return "PROC";
		case Lexeme::STRUCT: return "STRUCT";
		case Lexeme::CASE: return "CASE";
		case Lexeme::IMPLICIT: return "IMPLICIT";

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
		case ':':
		{
			lex.advance(); 
			if(try_consume(lex, "="))
				return Lexeme::COLON_EQ;

			return Lexeme::COLON;
		}
		case ';': lex.advance(); return Lexeme::SEMICOLON;
		case '^': lex.advance(); return Lexeme::CIRCUMFLEX;
		case '&': lex.advance(); return Lexeme::AMPERSAND;
		case '?': lex.advance(); return Lexeme::QUESTIONMARK;
		case '!': lex.advance(); return Lexeme::BANG;
		case '\'': lex.advance(); return Lexeme::SINGLE_QUOTE;
		case '@': lex.advance(); return Lexeme::AT;
		case '|': lex.advance(); return Lexeme::BAR;
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
		case '=':
		{
			lex.advance();
			if(try_consume(lex, "="))
				return Lexeme::DOUBLE_EQ;
				
			return Lexeme::EQ;
		}
		case '<':
		{
			lex.advance();
			if(try_consume(lex, "="))
				return Lexeme::LE;
				
			return Lexeme::LT;
		}
		case '>':
		{
			lex.advance();
			if(try_consume(lex, "="))
				return Lexeme::GE;
				
			return Lexeme::GT;
		}

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
	{"implicit", Lexeme::IMPLICIT},
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
struct ProcDefInstance;
struct ProcTypeDef;
struct UnionTypeDef;
struct StructDef;
struct StructDefInstance;
struct AliasDef;
class Scope;


//--------------------------------------------------------------------
// Paths
//--------------------------------------------------------------------
class Type;

struct TypeArgList
{
	TypeArgList() = default;
	explicit TypeArgList(Type &&type);
	TypeArgList(Type &&t1, Type &&t2);

	vector<Type> args;

	size_t size() const;
	vector<Type>::iterator begin();
	vector<Type>::iterator end();

	vector<Type>::const_iterator begin() const;
	vector<Type>::const_iterator end() const;

	void append(TypeArgList const &other);
};

struct UnresolvedPath
{
	string name;
	TypeArgList type_args;
	variant<Scope*, OwnPtr<UnresolvedPath>> context;

	UnresolvedPath(string const &name, TypeArgList &&type_args, Scope *scope);
	UnresolvedPath(string const &name, TypeArgList &&type_args, UnresolvedPath &&context);

	UnresolvedPath* try_get_parent() const ;
};


//--------------------------------------------------------------------
// Types
//--------------------------------------------------------------------
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


enum BuiltinTrait
{
	COMPARABLE, // Comparison operators
};

struct TypeConstraint
{
	void subsume(TypeConstraint const &other)
	{
		traits.insert(other.traits.begin(), other.traits.end());
	}

	bool satisfied_by(Type const &type) const;

	std::unordered_set<BuiltinTrait> traits;
};

struct TypeDeductionVar
{
	uint32_t id;

	friend bool operator == (TypeDeductionVar a, TypeDeductionVar b) = default;
};

// Q: Why have this indirection? Why not store the integer range directly in the type?
// A: This allows TypeEnv to assign types to integer literals by referrering to the KnownIntTypeVar.
struct KnownIntTypeVar
{
	uint32_t id;

	friend bool operator == (KnownIntTypeVar a, KnownIntTypeVar b) = default;
};

struct TypeParameterVar
{
	TypeVarDef *var;

	friend bool operator == (TypeParameterVar a, TypeParameterVar b) = default;
};

using VarType = variant<TypeParameterVar, TypeDeductionVar, KnownIntTypeVar>;

enum class IsMutable
{
	YES,
	NO,
};

struct PointerType
{
	explicit PointerType(Type &&target_type, IsMutable mutability) :
		target_type(std::make_unique<Type>(std::move(target_type))),
		mutability(mutability) {}

	bool is_mutable() const { return mutability == IsMutable::YES; }
	bool is_const() const { return mutability == IsMutable::NO; }

	OwnPtr<Type> target_type;
	IsMutable mutability;
};

struct ManyPointerType
{
	explicit ManyPointerType(Type &&element_type, IsMutable mutability) :
		element_type(std::make_unique<Type>(std::move(element_type))),
		mutability(mutability) {}

	bool is_mutable() const { return mutability == IsMutable::YES; }
	bool is_const() const { return mutability == IsMutable::NO; }

	OwnPtr<Type> element_type;
	IsMutable mutability;
};



class StructType
{
public:
	explicit StructType(StructDefInstance *inst);
	StructType(StructDef *def, TypeArgList &&type_args, NullableOwnPtr<StructType> parent);
	StructType(StructType &&rhs);

	StructType& operator = (StructType &&rhs);

	void instantiate(class TypeEnv const &type_env, struct SemaContext &ctx);

	bool is_instantiated() const { return m_inst != nullptr; }

	string const& name() const;
	StructDef* def() const { return m_def; }
	StructDefInstance* inst() const { return m_inst; }

	StructType clone() const;

	StructType const* parent() const;
	StructType* parent();

	TypeArgList const& type_args() const;
	TypeArgList& type_args();

private:
	StructDef *m_def;
	StructDefInstance *m_inst = nullptr;

	// If m_inst == nullptr
	TypeArgList m_type_args;
	NullableOwnPtr<StructType> m_parent = nullptr;
};


struct ProcType
{
	ProcTypeDef *def;

	// Available after resolve_identifiers()
	ProcTypeDef const *canonical_def = nullptr;
};

struct UnappliedProcType
{
	UnappliedProcType(ProcTypeDef &&type, TypeArgList &&type_args);

	OwnPtr<ProcTypeDef> type;
	TypeArgList type_args;
};

struct UnionType
{
	UnionTypeDef *def;

	// Available after resolve_identifiers()
	UnionTypeDef const *canonical_def = nullptr;
};


class Type : public variant
<
	BuiltinType,
	VarType,
	PointerType,
	ManyPointerType,
	StructType,
	ProcType,
	UnionType,
	UnappliedProcType,
	UnresolvedPath
>
{
public:
	using variant::variant;

	// I tried to rely soley on the constructors inherited from std::variant but unfortunately
	// `clang++ -std=20` doesn't like it. However, it works with -std=17, and g++ is happy even when
	// using -std=20.
	template<typename T>
	Type(T &&t) :
		variant(std::forward<T>(t)) {}
};


template<typename T>
optional<T> clone(optional<T> const &opt)
{
	if(!opt)
		return nullopt;

	return clone(*opt);
}

template<typename T>
NullableOwnPtr<T> clone(NullableOwnPtr<T> const &opt)
{
	if(!opt)
		return nullptr;

	return std::make_unique<T>(clone(*opt));
}


struct Parameter
{
	string name;
	Type type;
	NullableOwnPtr<class Expr> default_value;
};

struct ProcTypeDef
{
	vector<TypeVarDef*> type_params;
	vector<Parameter> params;
	Type ret;
};


bool equiv(Type const &a, Type const &b);

struct UnionTypeDef
{
	// Must always be sorted
	vector<Type> alternatives;

	optional<size_t> contains(Type const &type) const
	{
		for(size_t i = 0; i < alternatives.size(); ++i)
		{
			if(equiv(type, alternatives[i]))
				return i;
		}

		return nullopt;
	}
};


UnappliedProcType::UnappliedProcType(ProcTypeDef &&type, TypeArgList &&type_args) :
	type(std::make_unique<ProcTypeDef>(std::move(type))),
	type_args(std::move(type_args)) {}


TypeArgList clone(TypeArgList const &list);
UnresolvedPath clone(UnresolvedPath const &path);
ProcTypeDef clone(ProcTypeDef const &unapplied);
UnionTypeDef clone(UnionTypeDef const &unapplied);

Type clone(Type const &type)
{
	return type | match
	{
		[&](BuiltinType const &t) -> Type { return t; },
		[&](VarType const &t) -> Type { return t; },
		[&](PointerType const &t) -> Type { return PointerType(clone(*t.target_type), t.mutability); },
		[&](ManyPointerType const &t) -> Type { return ManyPointerType(clone(*t.element_type), t.mutability); },
		[&](StructType const &t) -> Type { return t.clone(); },
		[&](ProcType const &t) -> Type { return t; },
		[&](UnionType const &t) -> Type { return t; },
		[&](UnappliedProcType const &t) -> Type { return UnappliedProcType(clone(*t.type), clone(t.type_args)); },
		[&](UnresolvedPath const &t) -> Type { return clone(t); },
	};
}


UnionTypeDef clone(UnionTypeDef const &union_)
{
	vector<Type> alternatives;
	for(Type const &t: union_.alternatives)
		alternatives.push_back(clone(t));

	return UnionTypeDef{
		.alternatives = std::move(alternatives),
	};
}

UnresolvedPath clone(UnresolvedPath const &path)
{
	return path.context | match
	{
		[&](Scope *s) { return UnresolvedPath(path.name, clone(path.type_args), s); },
		[&](OwnPtr<UnresolvedPath> const &parent_path) { return UnresolvedPath(path.name, clone(path.type_args), clone(*parent_path)); },
	};
}


UnresolvedPath::UnresolvedPath(string const &name, TypeArgList &&type_args, Scope *scope) :
	name(name),
	type_args(std::move(type_args)),
	context(scope) {}

UnresolvedPath::UnresolvedPath(string const &name, TypeArgList &&type_args, UnresolvedPath &&context) :
	name(name),
	type_args(std::move(type_args)),
	context(std::make_unique<UnresolvedPath>(std::move(context))) {}

UnresolvedPath* UnresolvedPath::try_get_parent() const
{
	OwnPtr<UnresolvedPath> const *parent = std::get_if<OwnPtr<UnresolvedPath>>(&context);
	if(not parent)
		return nullptr;

	return parent->get();
}



namespace std
{
	template<>
	struct hash<::VarType>
	{
		size_t operator () (::VarType const &var) const
		{
			size_t h = 0;
			::combine_hashes(h, ::compute_hash(var.index()));
			var | match
			{
				[&](::TypeParameterVar v) { ::combine_hashes(h, ::compute_hash(v.var)); },
				[&](::TypeDeductionVar v) { ::combine_hashes(h, ::compute_hash(v.id)); },
				[&](::KnownIntTypeVar v) { ::combine_hashes(h, ::compute_hash(v.id)); },
			};

			return h;
		}
	};

	template<>
	struct hash<::KnownIntTypeVar>
	{
		size_t operator () (::KnownIntTypeVar const &v) const
		{
			return ::compute_hash(v.id);
		}
	};
}

bool integer_assignable_to(BuiltinType type, XInt64 val);
optional<BuiltinType> smallest_int_type_for(XInt64 low, XInt64 high);
struct SemaContext;

class TypeEnv
{
public:
	auto find(VarType var) const { return m_mapping.find(var); }
	auto begin() const { return m_mapping.begin(); }
	auto end() const { return m_mapping.end(); }
	bool empty() const { return m_mapping.empty(); }
	bool contains(VarType var) const { return m_mapping.contains(var); }

	Type const* try_lookup(VarType var) const
	{
		auto it = m_mapping.find(var);
		if(it == m_mapping.end())
			return nullptr;

		return &it->second;
	}

	Type& lookup(VarType var)
	{
		auto it = m_mapping.find(var);
		assert(it != m_mapping.end());

		return it->second;
	}

	void add_unique(VarType var, Type &&type)
	{
		update_deferred_type_annotations(var, type);

		auto res = m_mapping.emplace(var, std::move(type));
		assert(res.second);
	}

	void replace(VarType var, Type &&type)
	{
		update_deferred_type_annotations(var, type);

		auto it = m_mapping.find(var);
		if(it == m_mapping.end())
			m_mapping.emplace(var, std::move(type));
		else
			it->second = std::move(type);
	}

	void instantiate_int_vars(SemaContext const &ctx);

	void take(TypeEnv &&other)
	{
		for(auto &[var, type]: other.m_mapping)
			add_unique(var, std::move(type));

		for(auto &[var, known_ints]: other.m_deferred_type_instantiations)
			m_deferred_type_instantiations[var].insert(known_ints.begin(), known_ints.end());
	}

private:
	unordered_map<VarType, Type> m_mapping;
	unordered_map<VarType, unordered_set<KnownIntTypeVar>> m_deferred_type_instantiations;

	void update_deferred_type_annotations(VarType var, Type const &type)
	{
		if(VarType const *t = std::get_if<VarType>(&type))
		{
			if(KnownIntTypeVar const *known_int_var = std::get_if<KnownIntTypeVar>(t))
				m_deferred_type_instantiations[var].insert(*known_int_var);
		}
	}
};

TypeEnv clone(TypeEnv const &env)
{
	TypeEnv result;
	for(auto &[var, type]: env)
		result.add_unique(var, clone(type));

	return result;
}


bool equiv(Type const &a, Type const &b);
bool operator == (TypeArgList const &a, TypeArgList const &b)
{
	if(a.args.size() != b.args.size())
		return false;

	for(size_t i = 0; i < a.args.size(); ++i)
	{
		if(not equiv(a.args[i], b.args[i]))
			return false;
	}

	return true;
}

TypeArgList clone(TypeArgList const &list)
{
	TypeArgList result;
	for(Type const &arg: list.args)
		result.args.push_back(clone(arg));

	return result;
}


size_t TypeArgList::size() const { return args.size(); }
vector<Type>::iterator TypeArgList::begin() { return args.begin(); }
vector<Type>::iterator TypeArgList::end() { return args.end(); }
vector<Type>::const_iterator TypeArgList::begin() const { return args.begin(); }
vector<Type>::const_iterator TypeArgList::end() const { return args.end(); }

void TypeArgList::append(TypeArgList const &other)
{
	for(Type const &t: other)
		args.push_back(clone(t));
}

TypeArgList::TypeArgList(Type &&type)
{
	args.push_back(std::move(type));
}

TypeArgList::TypeArgList(Type &&t1, Type &&t2)
{
	args.push_back(std::move(t1));
	args.push_back(std::move(t2));
}


bool is_integer_type(Type const &type);
BuiltinType smallest_int_type_for(XInt64 val);
bool builtin_losslessly_convertible(BuiltinType dest, BuiltinType src);
bool integer_assignable_to(BuiltinType type, XInt64 val);

bool TypeConstraint::satisfied_by(Type const &type) const
{
	for(BuiltinTrait trait: traits)
	{
		switch(trait)
		{
			case BuiltinTrait::COMPARABLE:
			{
				if(not is_integer_type(type))
					return false;
			}
		}
	}

	return true;
}


//--------------------------------------------------------------------
// Expressions
//--------------------------------------------------------------------
class Scope;
class Expr;
struct Pattern;

using Stmt = variant<
	struct LetStmt,
	struct ExprStmt,
	struct BlockStmt,
	struct ReturnStmt,
	struct IfStmt,
	struct WhileStmt,
	struct MatchStmt
>;


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


struct ProcExpr
{
	ProcDefInstance *inst;
};


enum class UnaryOp
{
	NOT = int(Lexeme::NOT),
	NEG = int(Lexeme::MINUS),
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

	EQ = int(Lexeme::DOUBLE_EQ),
	LT = int(Lexeme::LT),
	LE = int(Lexeme::LE),
	GT = int(Lexeme::GT),
	GE = int(Lexeme::GE),
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


// The current syntax for taking a mutable address is `&mut var`, which is quite long.
// Potential alternatives:
// - Ditch `mut`: If `var` is mutable, then `&var` results in a mutable pointer
// - Replace `&mut` with `!`: `&var` results in a constant pointer, while `!var` results in a
//   mutable pointer
struct AddressOfExpr
{
	OwnPtr<Expr> object_expr;
	IsMutable mutability;

	AddressOfExpr(Expr &&object_expr, IsMutable mutability) :
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

struct ConstructorExpr
{
	Type ctor;

	StructType const& struct_() const { return std::get<StructType>(ctor); }
};

struct UnionInitExpr
{
	UnionInitExpr(size_t alt_idx, Expr &&value) :
		alt_idx(alt_idx),
		value(std::make_unique<Expr>(std::move(value))) {}

	size_t alt_idx;
	OwnPtr<Expr> value;
};

struct UnappliedProcExpr
{
	ProcDef *proc;
	TypeArgList type_args;
};

struct TryExpr
{
	TryExpr(
		Expr &&subject,
		optional<Stmt> &&on_failure,
		optional<Pattern> &&error_capture,
		NullableOwnPtr<Stmt> match_stmt = nullptr,
		NullableOwnPtr<Expr> result_expr = nullptr
	);

	OwnPtr<Expr> subject;

	// These are only used for constructing the `match_stmt`, i.e., they are not typechecked etc
	NullableOwnPtr<Stmt> on_failure;
	NullableOwnPtr<Pattern> error_capture;

	// Available after semantic analysis
	NullableOwnPtr<Stmt> match_stmt;
	NullableOwnPtr<Expr> result_expr;
};


class Expr : public variant
<
	IntLiteralExpr,
	BoolLiteralExpr,
	StringLiteralExpr,
	VarExpr,
	ConstructorExpr,
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
	MakeExpr,
	UnionInitExpr,
	TryExpr,
	UnappliedProcExpr,
	UnresolvedPath
> {
public:
	template<typename T>
	Expr(T &&t, SourceSpan span, optional<Type> &&type = nullopt) :
		variant(std::forward<T>(t)),
		span(span),
		type(std::move(type)) {}

	SourceSpan span;
	optional<Type> type;

	template<typename T>
	Expr clone_as(T &&t) const
	{
		optional<Type> cloned_type;
		if(type)
			cloned_type = clone(*type);

		return Expr(std::forward<T>(t), span, std::move(cloned_type));
	}
};


Expr clone(Expr const &expr);

Parameter clone(Parameter const &p)
{
	NullableOwnPtr<Expr> default_val;
	if(p.default_value)
		default_val = std::make_unique<Expr>(clone(*p.default_value));

	return Parameter{
		.name = p.name,
		.type = clone(p.type),
		.default_value = std::move(default_val),
	};
}


Expr clone(Expr const &expr);


//--------------------------------------------------------------------
// Patterns
//--------------------------------------------------------------------
struct Pattern;

struct DerefPattern
{
	OwnPtr<Pattern> sub;
};

struct AddressOfPattern
{
	OwnPtr<Pattern> sub;
	IsMutable mutability;
};

struct VarPattern
{
	VarDef *var;
};

struct WildcardPattern {};


struct PatternArgument
{
	OwnPtr<Pattern> pattern;
	optional<string> param_name;

	// Available after type checking
	optional<size_t> param_idx = nullopt;
};

struct ConstructorPattern
{
	Type ctor;
	vector<PatternArgument> args;
	bool has_parens;
};


struct Pattern : variant
<
	VarPattern,
	DerefPattern,
	AddressOfPattern,
	ConstructorPattern,
	WildcardPattern
>
{
	template<typename T>
	Pattern(T &&t, optional<Type> &&provided_type, optional<Type> &&type = nullopt) :
		variant(std::forward<T>(t)),
		provided_type(std::move(provided_type)),
		type(std::move(type)) {}

	optional<Type> provided_type;

	// Available after semantic analysis
	optional<Type> type;
};


PatternArgument clone(PatternArgument const &arg);

Pattern clone(Pattern const &pattern)
{
	return pattern | match
	{
		[&](VarPattern const &p)
		{
			return Pattern(p, clone(pattern.provided_type), clone(pattern.type));
		},
		[&](DerefPattern const &p)
		{
			return Pattern(
				DerefPattern(std::make_unique<Pattern>(clone(*p.sub))),
				clone(pattern.provided_type),
				clone(pattern.type)
			);
		},
		[&](AddressOfPattern const &p)
		{
			return Pattern(
				AddressOfPattern(std::make_unique<Pattern>(clone(*p.sub)), p.mutability),
				clone(pattern.provided_type),
				clone(pattern.type)
			);
		},
		[&](ConstructorPattern const &p)
		{
			vector<PatternArgument> args;
			for(PatternArgument const &arg: p.args)
				args.push_back(clone(arg));

			return Pattern(
				ConstructorPattern(clone(p.ctor), std::move(args), p.has_parens),
				clone(pattern.provided_type),
				clone(pattern.type)
			);
		},
		[&](WildcardPattern const&)
		{
			return Pattern(WildcardPattern(), clone(pattern.provided_type), clone(pattern.type));
		}
	};
}

PatternArgument clone(PatternArgument const &arg)
{
	return PatternArgument{
		.pattern = std::make_unique<Pattern>(clone(*arg.pattern)),
		.param_name = arg.param_name,
		.param_idx = arg.param_idx,
	};
}


//--------------------------------------------------------------------
// Statements
//--------------------------------------------------------------------
struct LetStmt
{
	Pattern lhs;
	optional<Expr> init_expr;
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
	Pattern capture;
	OwnPtr<Stmt> stmt;

	MatchArm(Pattern &&capture, Stmt &&stmt, optional<size_t> discr = nullopt) :
		capture(std::move(capture)),
		stmt(std::make_unique<Stmt>(std::move(stmt))),
		discr(discr) {}

	// Available after semantic analysis
	optional<size_t> discr{};
};

struct MatchStmt
{
	Expr expr;
	vector<MatchArm> arms;
};


IfStmt::IfStmt(Expr &&cond, Stmt &&then, optional<Stmt> &&else_) :
	condition(std::move(cond)),
	then(std::make_unique<Stmt>(std::move(then))),
	else_(else_ ? std::make_unique<Stmt>(std::move(*else_)) : nullptr) {}


Stmt clone(Stmt const &stmt)
{
	return stmt | match
	{
		[&](LetStmt const &s)
		{
			return Stmt(LetStmt(clone(s.lhs), clone(s.init_expr)));
		},
		[&](ExprStmt const &s)
		{
			return Stmt(ExprStmt(clone(s.expr)));
		},
		[&](BlockStmt const &s)
		{
			vector<OwnPtr<Stmt>> stmts;
			for(OwnPtr<Stmt> const &stmt: s.stmts)
				stmts.push_back(std::make_unique<Stmt>(clone(*stmt)));

			return Stmt(BlockStmt(s.scope, std::move(stmts)));
		},
		[&](ReturnStmt const &s)
		{
			optional<Expr> ret_expr;
			if(s.ret_expr)
				ret_expr = clone(*s.ret_expr);

			return Stmt(ReturnStmt(std::move(ret_expr)));
		},
		[&](IfStmt const &s)
		{
			Expr cond = clone(s.condition);
			Stmt then = clone(*s.then);
			optional<Stmt> else_;
			if(s.else_)
				else_ = clone(*s.else_);

			return Stmt(IfStmt(std::move(cond), std::move(then), std::move(else_)));
		},
		[&](WhileStmt const &s)
		{
			return Stmt(WhileStmt(clone(s.condition), clone(*s.body)));
		},
		[&](MatchStmt const &s)
		{
			Expr expr = clone(s.expr);
			vector<MatchArm> arms;
			for(MatchArm const &arm: s.arms)
			{
				arms.push_back(MatchArm(
					clone(arm.capture),
					clone(*arm.stmt),
					arm.discr
				));
			}

			return Stmt(MatchStmt(std::move(expr), std::move(arms)));
		},
	};
}

TryExpr::TryExpr(
	Expr &&subject,
	optional<Stmt> &&on_failure,
	optional<Pattern> &&error_capture,
	NullableOwnPtr<Stmt> match_stmt,
	NullableOwnPtr<Expr> result_expr
) :
	subject(std::make_unique<Expr>(std::move(subject))),
	on_failure(on_failure ? std::make_unique<Stmt>(std::move(*on_failure)) : nullptr),
	error_capture(error_capture ? std::make_unique<Pattern>(std::move(*error_capture)) : nullptr),
	match_stmt(std::move(match_stmt)),
	result_expr(std::move(result_expr)) {}

Expr clone(Expr const &expr)
{
	return expr | match
	{
		[&](IntLiteralExpr const &e)
		{
			return expr.clone_as(e);
		},
		[&](BoolLiteralExpr const &e)
		{
			return expr.clone_as(e);
		},
		[&](StringLiteralExpr const &e)
		{
			return expr.clone_as(e);
		},
		[&](VarExpr const &e)
		{
			return expr.clone_as(e);
		},
		[&](ConstructorExpr const &e)
		{
			return expr.clone_as(ConstructorExpr(clone(e.ctor)));
		},
		[&](ProcExpr const &e)
		{
			return expr.clone_as(e);
		},
		[&](UnaryExpr const &e)
		{
			return expr.clone_as(UnaryExpr(clone(*e.sub), e.op));
		},
		[&](BinaryExpr const &e)
		{
			return expr.clone_as(BinaryExpr(clone(*e.left), clone(*e.right), e.op));
		},
		[&](AddressOfExpr const &e)
		{
			return expr.clone_as(AddressOfExpr(clone(*e.object_expr), e.mutability));
		},
		[&](DerefExpr const &e)
		{
			return expr.clone_as(DerefExpr(clone(*e.ptr_expr)));
		},
		[&](IndexExpr const &e)
		{
			return expr.clone_as(IndexExpr(clone(*e.ptr_expr), clone(*e.idx_expr)));
		},
		[&](MemberAccessExpr const &e)
		{
			return expr.clone_as(MemberAccessExpr(clone(*e.object), e.member));
		},
		[&](AssignmentExpr const &e)
		{
			return expr.clone_as(AssignmentExpr(clone(*e.lhs), clone(*e.rhs)));
		},
		[&](AsExpr const &e)
		{
			return expr.clone_as(AsExpr(clone(*e.src_expr), clone(e.target_type)));
		},
		[&](CallExpr const &e)
		{
			vector<Argument> args;
			for(Argument const &a: e.args)
			{
				args.push_back(Argument{
					.expr = std::make_unique<Expr>(clone(*a.expr)),
					.name = a.name,
					.param_idx = a.param_idx,
				});
			}

			CallExpr cloned(clone(*e.callable), std::move(args));
			return expr.clone_as(std::move(cloned));
		},
		[&](SizeOfExpr const &e)
		{
			return expr.clone_as(SizeOfExpr(clone(e.type)));
		},
		[&](MakeExpr const &e)
		{
			return expr.clone_as(MakeExpr(std::make_unique<Expr>(clone(*e.init)), std::make_unique<Expr>(clone(*e.addr))));
		},
		[&](UnionInitExpr const &e)
		{
			return expr.clone_as(UnionInitExpr(e.alt_idx, clone(*e.value)));
		},
		[&](TryExpr const &e)
		{
			optional<Stmt> on_failure;
			if(e.on_failure)
				on_failure = clone(*e.on_failure);

			optional<Pattern> error_capture;
			if(e.error_capture)
				error_capture = clone(*e.error_capture);

			return expr.clone_as(TryExpr(clone(*e.subject), std::move(on_failure), std::move(error_capture), clone(e.match_stmt), clone(e.result_expr)));
		},
		[&](UnresolvedPath const &p)
		{
			return expr.clone_as(clone(p));
		},
		[&](UnappliedProcExpr const &p)
		{
			return expr.clone_as(UnappliedProcExpr(p.proc, clone(p.type_args)));
		},
	};
}


//--------------------------------------------------------------------
// Top-level items
//--------------------------------------------------------------------
using TopLevelItem = variant<ProcDef*, StructDef*, struct AliasDef*>;

void print(TopLevelItem const &item, std::ostream &os, int indent = 0);


//==============================================================================
// Item definitions (variables, procedures, structs)
//==============================================================================
class ParseError : public std::runtime_error
{
public:
	ParseError(std::string const &msg) :
		std::runtime_error{msg} {}
};


// A StructDefInstance is uniquely identified by
// - the list of type arguments that are used to instantiate the struct instance, and
// - a reference to the containing struct instance (if it exists)
struct DefInstanceKey
{
	TypeArgList type_args;
	StructDefInstance *outer_struct; // may be null

	friend bool operator == (DefInstanceKey const &a, DefInstanceKey const &b) = default;
};



//--------------------------------------------------------------------
// (Type) Variables
//--------------------------------------------------------------------
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
	IsMutable mutability;
	VarKind kind;
	optional<Type> type;

	bool is_mutable() const { return mutability == IsMutable::YES; }
};


//--------------------------------------------------------------------
// Procedures
//--------------------------------------------------------------------
ProcTypeDef clone(ProcTypeDef const &proc)
{
	vector<Parameter> params;
	for(Parameter const &p: proc.params)
		params.push_back(clone(p));

	return ProcTypeDef{
		.type_params = proc.type_params,
		.params = std::move(params),
		.ret = clone(proc.ret),
	};
}

// For simplicity, we do not consider α-equivlance for type parameters. It's not necessary as
// generic procedures are not first class and cannot be passed around.

namespace std
{
	template<>
	struct hash<::ProcTypeDef>
	{
		size_t operator () (::ProcTypeDef const &proc) const
		{
			size_t h = 0;
			for(::TypeVarDef const *tparam: proc.type_params)
				::combine_hashes(h, ::compute_hash(tparam));

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

	template<>
	struct hash<::UnionTypeDef>
	{
		size_t operator () (::UnionTypeDef const &proc) const
		{
			size_t h = 0;
			for(::Type const &alt: proc.alternatives)
				::combine_hashes(h, ::compute_hash(alt));

			return h;
		}
	};
}


struct ProcTypeEquiv
{
	bool operator () (ProcTypeDef const &a, ProcTypeDef const &b) const
	{
		if(a.type_params.size() != b.type_params.size())
			return false;

		if(a.params.size() != b.params.size())
			return false;

		for(size_t i = 0; i < a.type_params.size(); ++i)
		{
			if(a.type_params[i] != b.type_params[i])
				return false;
		}

		for(size_t i = 0; i < a.params.size(); ++i)
		{
			Parameter const &pa = a.params[i];
			Parameter const &pb = b.params[i];

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

struct UnionTypeEquiv
{
	bool operator () (UnionTypeDef const &a, UnionTypeDef const &b) const
	{
		if(a.alternatives.size() != b.alternatives.size())
			return false;

		for(size_t i = 0; i < a.alternatives.size(); ++i)
		{
			if(not equiv(a.alternatives[i], b.alternatives[i]))
				return false;
		}

		return true;
	}
};

namespace std
{
	template<>
	struct hash<::TypeArgList>
	{
		size_t operator () (::TypeArgList const &list) const
		{
			size_t h = 0;
			for(::Type const &t: list.args)
				::combine_hashes(h, ::compute_hash(t));

			return h;
		}
	};
}

namespace std
{
	template<>
	struct hash<::DefInstanceKey>
	{
		size_t operator () (::DefInstanceKey const &key) const
		{
			size_t h = ::compute_hash(key.type_args);
			::combine_hashes(h, ::compute_hash(key.outer_struct));

			return h;
		}
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
			type | match
			{
				[&](::BuiltinType const &t)
				{
					::combine_hashes(h, ::compute_hash((int)t));
				},
				[&](::VarType const &t)
				{
					::combine_hashes(h, ::compute_hash(t));
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
					assert(t.inst());
					::combine_hashes(h, ::compute_hash(t.inst()));
				},
				[&](::ProcType const &t)
				{
					::combine_hashes(h, ::compute_hash(*t.def));
				},
				[&](::UnionType const &t)
				{
					::combine_hashes(h, ::compute_hash(*t.def));
				},
				[&](::UnresolvedPath const&) { assert(!"hash<Type>: UnresolvedPath"); },
				[&](::UnappliedProcType const&) { assert(!"hash<Type>: UnappliedProcType"); },
			};

			return h;
		}
	};
}


struct ProcDef;

struct ProcDefInstance
{
	ProcDef *def;

	TypeArgList type_args;
	TypeEnv type_env;
	ProcTypeDef type;
	bool is_concrete;

	bool is_partial() const;
};

struct ProcDef
{
	ProcDef(string const &name, Scope *scope) :
		name(name), scope(scope) {}

	string name;
	ProcTypeDef type;
	Scope *scope;
	vector<VarDef*> param_vars;
	optional<Stmt> body;

	unordered_map<TypeArgList, ProcDefInstance> instances;
};


bool ProcDefInstance::is_partial() const
{
	return type_args.size() < def->type.type_params.size();
}


//--------------------------------------------------------------------
// Structs
//--------------------------------------------------------------------
struct StructDef;

struct MemoryLayout
{
	size_t size{};
	size_t alignment{};

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

// Defines the memory region inside a struct that is used to store the struct's case members.
struct CaseMemberRegion
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

// A struct member can either be...
using Member = variant<
	// a variable member or
	Parameter,
	// a case member
	StructDef*
>;


using InstanceMember = variant<Parameter, struct StructDefInstance*>;

using TypeDefInstance = variant<struct StructDefInstance*, UnionTypeDef const*>;

namespace std
{
	template<>
	struct hash<::TypeDefInstance>
	{
		size_t operator () (::TypeDefInstance const &v) const
		{
			size_t h = 0;
			::combine_hashes(h, ::compute_hash(v.index()));
			v | match
			{
				[&](struct StructDefInstance const *s) { ::combine_hashes(h, ::compute_hash(s)); },
				[&](UnionTypeDef const *s) { ::combine_hashes(h, ::compute_hash(s)); },
			};

			return h;
		}
	};
}


struct StructDefInstance
{
	// Reference to the struct definition of which this is an instance of
	StructDef *def;
	// If `def` is defined inside another struct (i.e., as a case member), then `outer_struct`
	// refers to the appropriate instance of that outer struct
	optional<StructType> outer_struct;

	TypeArgList type_args;
	TypeEnv type_env;
	optional<vector<Parameter>> constructor_params;
	vector<InstanceMember> members;
	bool own_args_concrete;

	unordered_set<TypeDefInstance> type_deps;

	bool sema_done = false;
	// Available after semantic analysis (i.e., when `sema_done == true`)
	optional<MemoryLayout> own_layout = nullopt; // does not include `parent`
	optional<BuiltinType> discriminator_type = nullopt; // if `num_cases > 0`
	optional<CaseMemberRegion> cases_layout = nullopt; // if `num_cases > 0`

	Range<InstanceMember const*> initial_var_members() const;
	Range<InstanceMember const*> case_members() const;
	Range<InstanceMember const*> trailing_var_members() const;

	MemoryLayout layout()
	{
		if(outer_struct)
			return outer_struct->inst()->layout();

		return *own_layout;
	}

	bool is_case_member_of(StructDefInstance const *other) const
	{
		if(outer_struct)
		{
			if(outer_struct->inst() == other)
				return true;

			return outer_struct->inst()->is_case_member_of(other);
		}

		return false;
	}

	bool is_concrete() const;
};


struct CaseMemberOf { StructDef *struct_; size_t case_idx; };
struct ExtensionOf { StructDef *struct_; };
struct NoParent {};
using ParentRelation = variant<NoParent, CaseMemberOf, ExtensionOf>;

struct StructDef
{
	string name;
	vector<Member> members;
	vector<TypeVarDef*> type_params;
	Scope *scope; // Contains the struct's type and member variables
	optional<vector<Parameter>> constructor_params; // Available iff the struct does not contain any `case`s
	bool ctor_without_parens = false;
	StructDef *implicit_ctor = nullptr;
	bool is_extern = false;

	ParentRelation parent{};

	unordered_map<DefInstanceKey, StructDefInstance> instances{};

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

	StructDef* try_get_parent() const
	{
		return parent | match
		{
			[&](CaseMemberOf parent) { return parent.struct_; },
			[](ExtensionOf) -> StructDef* { return nullptr; },
			[](NoParent) -> StructDef* { return nullptr; },
		};
	}

	bool is_case_member_of(StructDef const *other) const
	{
		return parent | match
		{
			[&](CaseMemberOf outer_struct) { return outer_struct.struct_ == other; },
			[](ExtensionOf) { return false; },
			[](NoParent) { return false; },
		};
	}
};


Range<InstanceMember const*> StructDefInstance::initial_var_members() const { return {members.data(), members.data() + def->num_initial_var_members}; }
Range<InstanceMember const*> StructDefInstance::case_members() const { return {members.data() + def->num_initial_var_members, members.data() + def->num_initial_var_members + def->num_cases}; }
Range<InstanceMember const*> StructDefInstance::trailing_var_members() const { return {members.data() + def->num_initial_var_members + def->num_cases, members.data() + members.size()}; }


bool StructDefInstance::is_concrete() const
{
	bool parent_concrete = true;
	if(outer_struct)
		parent_concrete = outer_struct->inst()->is_concrete();

	return own_args_concrete and not def->is_extern and parent_concrete;
}




StructType::StructType(StructDefInstance *inst) :
	m_def(inst->def),
	m_inst(inst) {}

StructType::StructType(StructDef *def, TypeArgList &&type_args, NullableOwnPtr<StructType> parent) :
	m_def(def),
	m_type_args(std::move(type_args)),
	m_parent(std::move(parent)) {}

StructType::StructType(StructType &&rhs) :
	m_def(rhs.m_def),
	m_inst(rhs.m_inst),
	m_type_args(std::move(rhs.m_type_args)),
	m_parent(std::move(rhs.m_parent)) {}

StructType& StructType::operator = (StructType &&rhs)
{
	assert(this != &rhs);

	m_def = rhs.m_def;
	m_inst = rhs.m_inst;
	m_type_args = std::move(rhs.m_type_args);
	m_parent = std::move(rhs.m_parent);

	return *this;
}


StructType const* StructType::parent() const
{
	if(is_instantiated())
	{
		if(m_inst->outer_struct)
			return &*m_inst->outer_struct;
	}

	return m_parent.get();
}

StructType* StructType::parent()
{
	if(is_instantiated())
	{
		if(m_inst->outer_struct)
			return &*m_inst->outer_struct;
	}

	return m_parent.get();
}

TypeArgList const& StructType::type_args() const
{
	if(is_instantiated())
		return m_inst->type_args;

	return m_type_args;
}

TypeArgList& StructType::type_args()
{
	if(is_instantiated())
		return m_inst->type_args;

	return m_type_args;
}

StructType StructType::clone() const
{
	if(m_inst)
		return StructType(m_inst);

	NullableOwnPtr<StructType> parent;
	if(m_parent)
		parent = std::make_unique<StructType>(m_parent->clone());

	return StructType(m_def, ::clone(m_type_args), std::move(parent));
}

string const& StructType::name() const { return m_def->name; }


//--------------------------------------------------------------------
// Aliases
//--------------------------------------------------------------------
struct AliasDefInstance
{
	AliasDef *alias;
	TypeArgList type_args;
	TypeEnv type_env;
	Type aliased_type;
};

struct AliasDef
{
	string name;
	vector<TypeVarDef*> type_params;
	Type aliased_type;

	unordered_map<TypeArgList, AliasDefInstance> instances{};
};


//==============================================================================
// Scope/Module
//==============================================================================
class Module;


using ItemDef = variant<ProcDef, StructDef, AliasDef>;

class Scope
{
public:
	explicit Scope(Module *mod, bool accept_item_decls, Scope *parent = nullptr) :
		m_mod(mod),
		m_parent(parent),
		m_accept_item_decls(accept_item_decls) {}

	Scope* new_child(bool accept_item_decls)
	{
		m_children.push_back(std::make_unique<Scope>(m_mod, accept_item_decls, this));
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

	VarDef* declare_temp_var(IsMutable mutability = IsMutable::NO)
	{
		string name = "__myca__temp_" + std::to_string(m_temp_var_counter++);
		return declare_var(name, mutability, VarKind::LOCAL);
	}

	VarDef* declare_var(string name, IsMutable mutability, VarKind kind)
	{
		if(try_lookup_item(name))
			throw ParseError("A type/procedure with this name has already been declared: " + name);

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

	ProcDef* add_proc(string const &name, Scope *scope)
	{
		if(not m_accept_item_decls)
			return m_parent->add_proc(name, scope);

		if(m_defs.contains(name))
			throw ParseError("There already exists a definition with this name: " + name);

		auto res = m_defs.insert({name, ProcDef(name, scope)});

		ProcDef *proc = &std::get<ProcDef>(res.first->second);

		return proc;
	}

	StructDef* add_struct(string const &name, Scope *scope)
	{
		if(not m_accept_item_decls)
			return m_parent->add_struct(name, scope);

		if(m_defs.contains(name))
			throw ParseError("There already exists a definition with this name: " + name);

		auto res = m_defs.insert({name, StructDef{
			.name = name,
			.members = {},
			.type_params = {},
			.scope = scope,
			.constructor_params = nullopt,
		}});

		StructDef *struct_ = &std::get<StructDef>(res.first->second);

		return struct_;
	}

	AliasDef* add_alias(string const &name, Type &&type)
	{
		if(not m_accept_item_decls)
			return m_parent->add_alias(name, std::move(type));

		if(m_defs.contains(name))
			throw ParseError("There already exists a definition with this name: " + name);

		auto res = m_defs.insert({name, AliasDef{
			.name = name,
			.type_params = {},
			.aliased_type = std::move(type),
		}});

		AliasDef *alias = &std::get<AliasDef>(res.first->second);

		return alias;
	}

	ItemDef* try_lookup_item(string const &name, bool traverse_upwards = true)
	{
		auto it = m_defs.find(name);
		if(it == m_defs.end())
		{
			if(traverse_upwards && m_parent)
				return m_parent->try_lookup_item(name, traverse_upwards);

			return nullptr;
		}

		return &it->second;
	}

	ItemDef* lookup_item(string const &name, bool traverse_upwards = true)
	{
		ItemDef *item = try_lookup_item(name, traverse_upwards);
		if(!item)
			throw ParseError("Item not found: " + name);

		return item;
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
	bool m_accept_item_decls;
	int m_temp_var_counter = 0;

	unordered_map<string, TypeVarDef> m_type_vars;
	unordered_map<string, VarDef> m_vars;
	unordered_map<string, ItemDef> m_defs;
};

struct ModuleListener
{
	virtual void new_struct_inst(StructDefInstance*) {}
	virtual void new_union_inst(UnionTypeDef const*) {}
	virtual void new_proc_inst(ProcDefInstance*) {}
};

struct UnionTypeInfo
{
	MemoryLayout layout;
	unordered_set<TypeDefInstance> type_deps;
};

bool operator < (Type const &a, Type const &b);

void flatten_union(vector<Type> const &alternatives, vector<Type> &result)
{
	for(Type const &alt: alternatives)
	{
		if(UnionType const *child_union = std::get_if<UnionType>(&alt))
			flatten_union(child_union->def->alternatives, result);
		else
			result.push_back(clone(alt));
	}
}

class Module
{
public:
	Module() :
		m_global_scope(std::make_unique<Scope>(this, true)),
		m_cur_scope(m_global_scope.get()) {}

	Scope* scope() { return m_cur_scope; }
	Scope* global() { return m_global_scope.get(); }

	Scope* open_scope(bool accept_item_decls = true)
	{
		return m_cur_scope = m_cur_scope->new_child(accept_item_decls);
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

	ProcTypeDef const* get_canonical(ProcTypeDef const &proc)
	{
		auto it = m_canonical_proc_types.find(proc);
		if(it != m_canonical_proc_types.end())
			return &*it;

		auto res = m_canonical_proc_types.insert(clone(proc));
		return &*res.first;
	}

	UnionTypeDef* add_union_type(UnionTypeDef &&union_)
	{
		m_union_types.push_back(std::make_unique<UnionTypeDef>(std::move(union_)));
		return m_union_types.back().get();
	}

	UnionTypeDef const* get_canonical(UnionTypeDef const &union_)
	{
		UnionTypeDef canonical;
		flatten_union(union_.alternatives, canonical.alternatives);
		std::sort(canonical.alternatives.begin(), canonical.alternatives.end());

		auto it = m_canonical_union_types.find(canonical);
		if(it != m_canonical_union_types.end())
			return &*it;

		// TODO Make sure each alternative has a unique type

		auto res = m_canonical_union_types.insert(clone(canonical));
		UnionTypeDef const *def = &*res.first;

		for(ModuleListener *l: m_listeners)
			l->new_union_inst(def);

		return def;
	}

	UnionTypeInfo* try_get_union_info(UnionTypeDef const *canonical_def)
	{
		auto it = m_union_type_info.find(canonical_def);
		if(it == m_union_type_info.end())
			return nullptr;

		return &it->second;
	}

	UnionTypeInfo* set_union_info(UnionTypeDef const *canonical_def, UnionTypeInfo &&info)
	{
		auto res = m_union_type_info.emplace(canonical_def, std::move(info));
		assert(res.second);

		return &res.first->second;
	}

	vector<TopLevelItem>& items() { return m_items; }
	vector<TopLevelItem> const& items() const { return m_items; }

	void add_listener(ModuleListener *listener)
	{
		m_listeners.push_back(listener);
	}

	void remove_listener(ModuleListener *listener)
	{
		auto it = std::find(m_listeners.begin(), m_listeners.end(), listener);
		if(it != m_listeners.end())
			m_listeners.erase(it);
	}

	void notify_new_struct_inst(StructDefInstance *inst)
	{
		for(ModuleListener *l: m_listeners)
			l->new_struct_inst(inst);
	}

	void notify_new_proc_inst(ProcDefInstance *inst)
	{
		for(ModuleListener *l: m_listeners)
			l->new_proc_inst(inst);
	}

	auto union_types() const -> auto
	{
		return Range{m_canonical_union_types.begin(), m_canonical_union_types.end()};
	}

private:
	OwnPtr<Scope> m_global_scope;
	Scope *m_cur_scope;
	vector<TopLevelItem> m_items;

	vector<OwnPtr<ProcTypeDef>> m_proc_types;
	unordered_set<ProcTypeDef, std::hash<ProcTypeDef>, ProcTypeEquiv> m_canonical_proc_types;

	vector<OwnPtr<UnionTypeDef>> m_union_types;
	unordered_set<UnionTypeDef, std::hash<UnionTypeDef>, UnionTypeEquiv> m_canonical_union_types;
	unordered_map<UnionTypeDef const*, UnionTypeInfo> m_union_type_info;

	vector<ModuleListener*> m_listeners;
};


vector<Parameter> clone(vector<Parameter> const &params)
{
	vector<Parameter> cloned;
	for(Parameter const &p: params)
		cloned.push_back(clone(p));

	return cloned;
}


//==============================================================================
// AST builder
//==============================================================================
Pattern pattern_Ctor(Type &&ctor, vector<Pattern> &&args, bool has_parens = true)
{
	vector<PatternArgument> pat_args;
	for(Pattern &arg: args)
	{
		pat_args.push_back(PatternArgument{
			.pattern = std::make_unique<Pattern>(std::move(arg)),
			.param_name = nullopt,
			.param_idx = nullopt,
		});
	}

	return Pattern(ConstructorPattern{
		.ctor = std::move(ctor),
		.args = std::move(pat_args),
		.has_parens = has_parens,
	}, nullopt);
}

Pattern pattern_Var(VarDef *var, optional<Type> &&provided_type = nullopt)
{
	return Pattern(VarPattern(var), std::move(provided_type));
}

struct Segment
{
	string name;
	TypeArgList type_args{};
};

UnresolvedPath mk_path(Segment &&seg, Scope *scope)
{
	return UnresolvedPath(std::move(seg.name), std::move(seg.type_args), scope);
}

UnresolvedPath mk_path(Segment &&seg1, Segment &&seg2, Scope *scope)
{
	return UnresolvedPath(
		std::move(seg2.name),
		std::move(seg2.type_args),
		mk_path(std::move(seg1), scope)
	);
}


Expr expr_Assignment(Expr &&lhs, Expr &&rhs)
{
	return Expr(AssignmentExpr(std::move(lhs), std::move(rhs)), {});
}

Expr expr_Var(VarDef *var)
{
	return Expr(VarExpr(var), {});
}

Expr expr_Call(Expr &&callable, vector<Expr> &&args)
{
	vector<Argument> call_args;
	for(Expr &arg: args)
	{
		call_args.push_back(Argument{
			.expr = std::make_unique<Expr>(std::move(arg)),
			.name = nullopt,
		});
	}

	return Expr(CallExpr(std::move(callable), std::move(call_args)), {});
}

Expr expr_Ctor(Type &&ctor)
{
	return Expr(ConstructorExpr(std::move(ctor)), {});
}


Stmt stmt_Assignment(Expr &&lhs, Expr &&rhs)
{
	return ExprStmt(expr_Assignment(std::move(lhs), std::move(rhs)));
}

Stmt stmt_Ret(Expr &&expr)
{
	return ReturnStmt(std::move(expr));
}

Stmt stmt_Block(vector<Stmt> &&stmts)
{
	vector<OwnPtr<Stmt>> stmt_ptrs;
	for(Stmt &stmt: stmts)
		stmt_ptrs.push_back(std::make_unique<Stmt>(std::move(stmt)));

	return BlockStmt(nullptr, std::move(stmt_ptrs));
}

Stmt stmt_Let(Pattern &&lhs, optional<Expr> &&rhs = nullopt)
{
	return LetStmt(std::move(lhs), std::move(rhs));
}

Stmt stmt_Match(Expr &&subject, vector<MatchArm> &&arms)
{
	return MatchStmt(std::move(subject), std::move(arms));
}


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

std::ostream& operator << (std::ostream &os, Type const &type);

std::ostream& operator << (std::ostream &os, UnresolvedPath const &path)
{
	os << "!!";
	if(UnresolvedPath *parent_path = path.try_get_parent())
		os << *parent_path << ".";

	os << path.name;
	if(path.type_args.size())
		os << "'(" << RangeFmt(path.type_args.args, ", ") << ")";

	return os;
}

std::ostream& operator << (std::ostream &os, StructType const &struct_)
{
	if(struct_.parent())
		os << *struct_.parent() << ".";

	os << struct_.name();
	if(struct_.type_args().size())
		os << "'(" << RangeFmt(struct_.type_args().args, ", ") << ")";

	return os;
}

std::ostream& operator << (std::ostream &os, Type const &type)
{
	type | match
	{
		[&](BuiltinType const &t) { os << str(t); },
		[&](VarType const &t)
		{
			t | match
			{
				[&](TypeParameterVar v) { os << v.var->name; },
				[&](TypeDeductionVar v) { os << "$TypeDeductionVar(" << v.id << ")"; },
				[&](KnownIntTypeVar v) { os << "$KnownIntTypeVar(" << v.id << ")"; },
			};
		},
		[&](PointerType const &t)
		{
			os << "^";
			if(t.is_mutable())
				os << "mut ";
			os << *t.target_type;
		},
		[&](ManyPointerType const &t)
		{
			os << "[^]";
			if(t.is_mutable())
				os << "mut ";
			os << *t.element_type;
		},
		[&](StructType const &t)
		{
			os << t;
		},
		[&](ProcType const &t)
		{
			os << "proc(" << RangeFmt(t.def->params, ", ", [&](Parameter const  &p)
			{
				os << p.type;
			});
			os << ") -> " << t.def->ret;
		},
		[&](UnionType const &t)
		{
			os << "(" << RangeFmt(t.def->alternatives, " | ", [&](Type const &t)
			{
				os << t;
			}) << ")";
		},
		[&](UnresolvedPath const &p)
		{
			os << p;
		},
		[&](UnappliedProcType const &t)
		{
			os << "proc(" << RangeFmt(t.type->params, ", ", [&](Parameter const &p)
			{
				os << p.type;
			});

			os << ") -> " << t.type->ret;
			if(t.type_args.size())
				os << "'(" << RangeFmt(t.type_args.args, ", ") << ")";
		},
	};

	return os;
}


std::ostream& operator << (std::ostream &os, XInt64 xint)
{
	if(xint.is_negative())
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

void print(std::ostream &os, Stmt const &stmt, int indent = 0, PrintListener *listener = nullptr);

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
		[&](VarExpr const &e)
		{
			os << e.var->name;
		},
		[&](ConstructorExpr const &e)
		{
			os << e.ctor;
		},
		[&](ProcExpr const &e)
		{
			os << e.inst->def->name;
			if(e.inst->type_args.size())
				os << "'(" << RangeFmt(e.inst->type_args.args, ", ") << ")";
		},
		[&](UnaryExpr const &e)
		{
			os << "(";
			switch(e.op)
			{
				case UnaryOp::NOT: os << "not "; break;
				case UnaryOp::NEG: os << "-"; break;
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

				case BinaryOp::EQ: os << " == "; break;
				case BinaryOp::LT: os << " < "; break;
				case BinaryOp::LE: os << " <= "; break;
				case BinaryOp::GT: os << " > "; break;
				case BinaryOp::GE: os << " >= "; break;
			}
			print(os, *e.right, listener) << ")";
		},
		[&](AddressOfExpr const &e)
		{
			os << "&";
			if(e.mutability == IsMutable::YES)
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
		[&](UnionInitExpr const &e)
		{
			print(os, *e.value, listener);
		},
		[&](TryExpr const &e)
		{
			print(os, *e.subject, listener);
			os << "?";
			if(e.on_failure)
				print(os, *e.on_failure, 0, listener);
		},
		[&](UnresolvedPath const &p)
		{
			os << p;
		},
		[&](UnappliedProcExpr const &e)
		{
			os << e.proc->name;
			if(e.type_args.size())
				os << "'(" << RangeFmt(e.type_args.args, ", ") << ")";
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


std::ostream& print(std::ostream &os, Pattern const &pattern)
{
	pattern | match
	{
		[&](VarPattern const &p)
		{
			if(p.var->is_mutable())
				os << "mut ";
			os << p.var->name;

			if(p.var->type)
				os << ": " << *p.var->type;
		},
		[&](DerefPattern const &p)
		{
			print(os, *p.sub);
			os << " ^";
		},
		[&](AddressOfPattern const &p)
		{
			print(os, *p.sub);
			os << " &";
			if(p.mutability == IsMutable::YES)
				os << "mut";
		},
		[&](ConstructorPattern const &p)
		{
			os << p.ctor << "(" << RangeFmt(p.args, ", ", [&](PatternArgument const &arg)
			{
				print(os, *arg.pattern);
			}) << ")";

		},
		[&](WildcardPattern const&)
		{
			os << "_";
		}
	};

	return os;
}


void print(std::ostream &os, Stmt const &stmt, int indent, PrintListener *listener)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ');
			os << "let ";
			print(os, s.lhs);
			if(s.init_expr)
			{
				os << " = ";
				print(os, *s.init_expr, listener);
			}

			os << ";";
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
				os << string((indent+1)*INDENT_WIDTH, ' ') << "case ";
				print(os, arm.capture);
				os << "\n";
				print(os, *arm.stmt, indent+1, listener);
				os << "\n";
			}
			os << string(indent*INDENT_WIDTH, ' ') << "}";
		},
	};

	if(listener)
		listener->after_stmt(os, stmt, indent);
}

std::ostream& operator << (std::ostream &os, Stmt const &stmt)
{
	print(os, stmt);
	return os;
}

std::ostream& operator << (std::ostream &os, vector<TypeVarDef*> const &type_params)
{
	if(type_params.size())
	{
		os << "'(" << RangeFmt(type_params, ", ", [&](TypeVarDef const *tvar)
		{
			os << tvar->name;
		});
		os << ")";
	}

	return os;
}

void print(TopLevelItem const &item, std::ostream &os, int indent, PrintListener *listener = nullptr)
{
	item | match
	{
		[&](ProcDef *def)
		{
			if(listener) listener->before_proc(os, def);

			os << "proc " << def->name << def->type.type_params;
			os << "(";
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
					os << string(indent*INDENT_WIDTH, ' ') << "struct " << def->name << def->type_params;
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
							os << string((indent+1)*INDENT_WIDTH, ' ') << var_member.name << ": " << var_member.type;
							if(var_member.default_value)
							{
								os << " = ";
								print(os, *var_member.default_value);
							}
							os << ",";
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
			os << "typealias " << def->name << def->type_params << " = " << def->aliased_type << ";";
		},
	};
}


std::ostream& print(std::ostream &os, Module const &mod, PrintListener *listener = nullptr)
{
	for(TopLevelItem item: mod.items())
	{
		print(item, os, 0, listener);
		os << "\n\n";
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

	void insert(Lexeme lexeme)
	{
		m_tokens.insert(m_tokens.begin() + m_pos, Token{.kind = lexeme, .span = {}});
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
Type parse_prefix_type(Parser &parser, Module &mod);
Type parse_type(Parser &parser, Module &mod);

StructDef* parse_struct(Parser &parser, Module &mod, bool top_level, ParentRelation parent = NoParent());

TypeArgList parse_type_arg_list(Parser &parser, Module &mod)
{
	TypeArgList arg_list;
	if(try_consume(parser, Lexeme::LEFT_PAREN))
	{
		while(parser.peek() != Lexeme::RIGHT_PAREN)
		{
			arg_list.args.push_back(parse_type(parser, mod));
			if(parser.peek() != Lexeme::RIGHT_PAREN)
				consume(parser, Lexeme::COMMA);
		}
		consume(parser, Lexeme::RIGHT_PAREN);
	}
	else
		arg_list.args.push_back(parse_prefix_type(parser, mod));

	return arg_list;
}

UnresolvedPath parse_path(string &&first_name, Parser &parser, Module &mod)
{
	UnresolvedPath path(std::move(first_name), {}, mod.scope());
	while(true)
	{
		if(try_consume(parser, Lexeme::SINGLE_QUOTE))
			path.type_args = parse_type_arg_list(parser, mod);

		if(try_consume(parser, Lexeme::DOT))
		{
			string_view ident = consume(parser, Lexeme::IDENTIFIER).text;
			path = UnresolvedPath(string(ident), {}, std::move(path));
		}
		else
			break;
	}

	return path;
}

Type parse_prefix_type(Parser &parser, Module &mod)
{
	require_not_done(parser, "type");
	Token const &tok = parser.next();
	switch(tok.kind)
	{
		case Lexeme::IDENTIFIER:
		{
			if(tok.text == TYPE_NEVER_NAME)
				return BuiltinType::NEVER;
			else if(tok.text == TYPE_UNIT_NAME)
				return BuiltinType::UNIT;
			else if(tok.text == "bool")
				return BuiltinType::BOOL;
			else if(tok.text == "i8")
				return BuiltinType::I8;
			else if(tok.text == "u8")
				return BuiltinType::U8;
			else if(tok.text == "i32")
				return BuiltinType::I32;
			else if(tok.text == "u32")
				return BuiltinType::U32;
			else if(tok.text == TYPE_ISIZE_NAME)
				return BuiltinType::ISIZE;
			else if(tok.text == TYPE_USIZE_NAME)
				return BuiltinType::USIZE;

			if(TypeVarDef *tvar = mod.scope()->try_lookup_type_var(string(tok.text)))
				return VarType(TypeParameterVar(tvar));

			return parse_path(string(tok.text), parser, mod);
		}

		case Lexeme::CIRCUMFLEX:
		{
			IsMutable mutability = try_consume(parser, Lexeme::MUT) ? IsMutable::YES : IsMutable::NO;
			Type target_type = parse_prefix_type(parser, mod);
			return PointerType(std::move(target_type), mutability);
		}

		case Lexeme::LEFT_BRACKET:
		{
			consume(parser, Lexeme::CIRCUMFLEX);
			consume(parser, Lexeme::RIGHT_BRACKET);
			IsMutable mutability = try_consume(parser, Lexeme::MUT) ? IsMutable::YES : IsMutable::NO;
			Type element_type = parse_prefix_type(parser, mod);
			return ManyPointerType(std::move(element_type), mutability);
		}

		case Lexeme::QUESTIONMARK:
		{
			Type type = parse_prefix_type(parser, mod);
			return UnresolvedPath("Option", TypeArgList(std::move(type)), mod.global());
		}

		case Lexeme::BANG:
		{
			Type err_type = parse_prefix_type(parser, mod);
			return UnresolvedPath("Result", TypeArgList(BuiltinType::UNIT, std::move(err_type)), mod.global());
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

		case Lexeme::STRUCT:
		{
			StructDef* struct_ = parse_struct(parser, mod, false);
			if(struct_->type_params.size())
				throw ParseError("Inline struct definition cannot have type parameters");

			return UnresolvedPath(struct_->name, {}, mod.scope());
		}

		default: throw ParseError("Invalid token while parsing type: "s + str(tok.kind));
	}
}

Type parse_result_type(Parser &parser, Module &mod)
{
	Type type = parse_prefix_type(parser, mod);
	if(try_consume(parser, Lexeme::BANG))
	{
		Type err_type = parse_prefix_type(parser, mod);
		type = UnresolvedPath("Result", TypeArgList(std::move(type), std::move(err_type)), mod.global());
	}

	return type;
}

Type parse_union_type(Parser &parser, Module &mod)
{
	vector<Type> alternatives;
	alternatives.push_back(parse_result_type(parser, mod));
	while(try_consume(parser, Lexeme::BAR))
		alternatives.push_back(parse_result_type(parser, mod));

	if(alternatives.size() == 1)
		return std::move(alternatives.back());

	return UnionType(mod.add_union_type(UnionTypeDef(std::move(alternatives))));
}

Type parse_type(Parser &parser, Module &mod)
{
	return parse_union_type(parser, mod);
}


//--------------------------------------------------------------------
// Expressions
//--------------------------------------------------------------------
Expr parse_prefix_expr(Parser &parser, Module &mod);
Expr parse_infix_expr(Parser &parser, Module &mod, Expr &&left);

BinaryOp to_binary_op(Lexeme tok)
{
	return (BinaryOp)tok;
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

		// Try
		case Lexeme::QUESTIONMARK:
			return OperatorInfo{.precedence = 3, .assoc = Associativity::LEFT};

		// Unary minus
		case Lexeme::UNARY_MINUS:
			return OperatorInfo{.precedence = 4, .assoc = Associativity::LEFT};

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

		// Comparison
		case Lexeme::DOUBLE_EQ:
		case Lexeme::LT:
		case Lexeme::LE:
		case Lexeme::GT:
		case Lexeme::GE:
			return OperatorInfo{.precedence = 8, .assoc = Associativity::LEFT};

		// Assignment
		case Lexeme::COLON_EQ:
			return OperatorInfo{.precedence = 9, .assoc = Associativity::RIGHT};

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
			if(VarDef *var = mod.scope()->try_lookup_var(string(tok.text)))
				return with_location(VarExpr(var));

			UnresolvedPath path = parse_path(string(tok.text), parser, mod);
			return with_location(std::move(path));
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

		case Lexeme::MINUS:
		{
			Expr sub_expr = parse_expr(parser, mod, *get_operator_info(Lexeme::UNARY_MINUS));
			return with_location(UnaryExpr(std::move(sub_expr), UnaryOp::NEG));
		}

		case Lexeme::AMPERSAND:
		{
			IsMutable mutability = try_consume(parser, Lexeme::MUT) ? IsMutable::YES : IsMutable::NO;
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

enum class ScopePolicy
{
	REUSE_SCOPE,
	NEW_SCOPE,
};
BlockStmt parse_block_stmt(Parser &parser, Module &mod, ScopePolicy policy);

Pattern parse_pattern(Parser &parser, Module &mod);

template<typename T>
vector<T> mk_vec(T &&t)
{
	vector<T> vec;
	vec.push_back(std::move(t));
	return vec;
}

template<typename T>
vector<T> mk_vec(T &&a, T &&b)
{
	vector<T> vec;
	vec.push_back(std::move(a));
	vec.push_back(std::move(b));
	return vec;
}

Expr parse_infix_expr(Parser &parser, Module &mod, Expr &&left)
{
	Token tok = parser.next();
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
		case Lexeme::DOUBLE_EQ:
		case Lexeme::LT:
		case Lexeme::LE:
		case Lexeme::GT:
		case Lexeme::GE:
		{
			return with_location(BinaryExpr(
				std::move(left),
				parse_expr(parser, mod, *get_operator_info(tok.kind)),
				to_binary_op(tok.kind)
			));
		}

		case Lexeme::COLON_EQ:
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

		case Lexeme::QUESTIONMARK:
		{
			optional<Stmt> on_failure;
			optional<Pattern> error_capture;
			if(try_consume(parser, Lexeme::ELSE))
			{
				if(parser.peek() != Lexeme::LEFT_BRACE)
				{
					Pattern error_pat = parse_pattern(parser, mod);
					error_capture = pattern_Ctor(
						mk_path({"Result"}, {"Err"}, mod.global()),
						mk_vec(std::move(error_pat))
					);
				}

				on_failure = parse_block_stmt(parser, mod, ScopePolicy::NEW_SCOPE);
				parser.insert(Lexeme::SEMICOLON);
			}

			Expr expr = with_location(TryExpr(std::move(left), std::move(on_failure), std::move(error_capture)));
			return expr;
		}

		default: UNREACHABLE;
	}
}

#undef with_location


//--------------------------------------------------------------------
// Patterns
//--------------------------------------------------------------------
Pattern parse_primary_pattern(Parser &parser, Module &mod)
{
	require_not_done(parser, "pattern");
	switch(parser.tok().kind)
	{
		case Lexeme::LET:
		{
			parser.next();
			IsMutable mutability = IsMutable::NO;
			if(try_consume(parser, Lexeme::MUT))
				mutability = IsMutable::YES;

			string_view ident = consume(parser, Lexeme::IDENTIFIER).text;
			VarDef *var = mod.scope()->declare_var(string(ident), mutability, VarKind::LOCAL);

			return Pattern(VarPattern(var), nullopt);
		}

		case Lexeme::IDENTIFIER:
		{
			if(parser.tok().text == "_")
			{
				parser.next();
				return Pattern(WildcardPattern(), nullopt);
			}

			Type ctor = parse_type(parser, mod);

			bool has_parens = false;
			vector<PatternArgument> args;
			if(try_consume(parser, Lexeme::LEFT_PAREN))
			{
				has_parens = true;
				while(parser.peek() != Lexeme::RIGHT_PAREN)
				{
					args.push_back(PatternArgument{
						.pattern = std::make_unique<Pattern>(parse_pattern(parser, mod)),
						.param_name = nullopt,
						.param_idx = nullopt,
					});
					if(parser.peek() != Lexeme::RIGHT_PAREN)
						consume(parser, Lexeme::COMMA);
				}
				consume(parser, Lexeme::RIGHT_PAREN);
			}

			return Pattern(ConstructorPattern(std::move(ctor), std::move(args), has_parens), nullopt);
		}

		default:
			throw ParseError("Invalid lexeme while parsing pattern");
	}
}

Pattern parse_pattern(Parser &parser, Module &mod)
{
	Pattern p = parse_primary_pattern(parser, mod);
	while(not parser.done())
	{
		if(try_consume(parser, Lexeme::CIRCUMFLEX))
			p = Pattern(DerefPattern(std::make_unique<Pattern>(std::move(p))), nullopt);
	
		else if(try_consume(parser, Lexeme::AMPERSAND))
		{
			IsMutable mutability = IsMutable::NO;
			if(try_consume(parser, Lexeme::MUT))
				mutability = IsMutable::YES;

			p = Pattern(AddressOfPattern(std::make_unique<Pattern>(std::move(p)), mutability), nullopt);
		}

		else if(try_consume(parser, Lexeme::COLON))
			p.provided_type = parse_type(parser, mod);

		else
			break;
	}

	return p;
}


//--------------------------------------------------------------------
// Statements
//--------------------------------------------------------------------
BlockStmt parse_block_stmt(Parser &parser, Module &mod, ScopePolicy policy = ScopePolicy::NEW_SCOPE);

Stmt parse_stmt(Parser &parser, Module &mod)
{
	require_not_done(parser, "statement");
	switch(parser.tok().kind)
	{
		case Lexeme::LET:
		{
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

				mod.open_scope();
				Pattern capture = parse_pattern(parser, mod);
				Stmt body = parse_block_stmt(parser, mod, ScopePolicy::REUSE_SCOPE);

				arms.emplace_back(std::move(capture), std::move(body));
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
TypeVarDef* parse_type_param(Parser &parser, Module &mod)
{
	string_view type_var_name = consume(parser, Lexeme::IDENTIFIER).text;
	return mod.scope()->declare_type_var(string(type_var_name));
}

vector<TypeVarDef*> parse_type_param_list(Parser &parser, Module &mod)
{
	vector<TypeVarDef*> params;
	if(try_consume(parser, Lexeme::SINGLE_QUOTE))
	{
		if(try_consume(parser, Lexeme::LEFT_PAREN))
		{
			while(parser.peek() != Lexeme::RIGHT_PAREN)
			{
				params.push_back(parse_type_param(parser, mod));
				if(parser.peek() != Lexeme::RIGHT_PAREN)
					consume(parser, Lexeme::COMMA);
			}
			consume(parser, Lexeme::RIGHT_PAREN);
		}
		else
			params.push_back(parse_type_param(parser, mod));
	}

	return params;
}

ProcDef* parse_proc(Parser &parser, Module &mod)
{
	string_view name = consume(parser, Lexeme::IDENTIFIER).text;
	ProcDef *proc = mod.scope()->add_proc(string(name), mod.open_scope());
	proc->type.type_params = parse_type_param_list(parser, mod);

	// Parse parameters
	consume(parser, Lexeme::LEFT_PAREN);
	while(parser.peek() != Lexeme::RIGHT_PAREN)
	{
		string_view param_name = consume(parser, Lexeme::IDENTIFIER).text;
		consume(parser, Lexeme::COLON);
		Type param_type = parse_type(parser, mod);
		VarDef *param = mod.scope()->declare_var(string(param_name), IsMutable::NO, VarKind::PARAM);
		param->type = clone(param_type);
		proc->param_vars.push_back(param);

		NullableOwnPtr<Expr> default_value;
		if(try_consume(parser, Lexeme::EQ))
			default_value = std::make_unique<Expr>(parse_expr(parser, mod));

		proc->type.params.push_back(Parameter(string(param_name), std::move(param_type), std::move(default_value)));

		if(parser.peek() != Lexeme::RIGHT_PAREN)
			consume(parser, Lexeme::COMMA);
	}
	consume(parser, Lexeme::RIGHT_PAREN);

	// Parse return type
	proc->type.ret = BuiltinType::UNIT;
	if(try_consume(parser, Lexeme::THIN_ARROW))
		proc->type.ret = parse_type(parser, mod);

	// Parse body
	if(!try_consume(parser, Lexeme::SEMICOLON))
		proc->body = parse_block_stmt(parser, mod, ScopePolicy::REUSE_SCOPE);
	mod.close_scope();

	return proc;
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
		result.push_back(clone(std::get<Parameter>(m)));
}

void gather_trailing_params(StructDef *struct_, vector<Parameter> &result)
{
	for(Member const &m: struct_->trailing_var_members())
		result.push_back(clone(std::get<Parameter>(m)));

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
		struct_->constructor_params = vector<Parameter>();
		gather_initial_params(struct_, *struct_->constructor_params);
		gather_trailing_params(struct_, *struct_->constructor_params);
	}
	else
	{
		for(Member const &m: struct_->case_members())
			init_constructor_params(std::get<StructDef*>(m));
	}
}

StructDef* parse_struct(Parser &parser, Module &mod, bool top_level, ParentRelation parent)
{
	string_view name = consume(parser, Lexeme::IDENTIFIER).text;
	StructDef *struct_ = mod.scope()->add_struct(string(name), mod.open_scope());
	struct_->parent = parent;
	struct_->type_params = parse_type_param_list(parser, mod);

	// Parse members
	if(try_consume(parser, Lexeme::LEFT_BRACE))
	{
		while(parser.peek() != Lexeme::RIGHT_BRACE)
		{
			if(try_consume(parser, Lexeme::CASE))
			{
				bool implicit_ctor = (bool)try_consume(parser, Lexeme::IMPLICIT);
				StructDef *case_member = parse_struct(parser, mod, false, CaseMemberOf(struct_, struct_->num_cases));
				struct_->members.push_back(case_member);
				struct_->num_cases += 1;

				if(implicit_ctor)
				{
					if(struct_->implicit_ctor)
						throw ParseError("Only one case member can be marked implicit");

					struct_->implicit_ctor = case_member;
				}
			}
			else
			{
				string_view member_name = consume(parser, Lexeme::IDENTIFIER).text;
				consume(parser, Lexeme::COLON);
				VarDef *member_var = mod.scope()->declare_var(string(member_name), IsMutable::YES, VarKind::FIELD);
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
	}
	else
	{
		struct_->ctor_without_parens = true;
		if(top_level)
			consume(parser, Lexeme::SEMICOLON);
	}

	mod.close_scope();
	if(is<NoParent>(parent))
		init_constructor_params(struct_);

	if(struct_->implicit_ctor)
	{
		if(not struct_->implicit_ctor->constructor_params or struct_->implicit_ctor->constructor_params->size() != 1)
			throw ParseError("Only one case members with exactly one element can be marked implicit");
	}

	return struct_;
}


void parse_top_level_item(Parser &parser, Module &mod)
{
	require_not_done(parser, "top-level item");
	Token tok = parser.next();
	switch(tok.kind)
	{
		case Lexeme::PROC: mod.add_item(parse_proc(parser, mod)); break;
		case Lexeme::STRUCT: mod.add_item(parse_struct(parser, mod, true)); break;

		case Lexeme::TYPEALIAS:
		{
			string_view alias_name = consume(parser, Lexeme::IDENTIFIER).text;

			mod.open_scope(false);
			vector<TypeVarDef*> type_params = parse_type_param_list(parser, mod);
			consume(parser, Lexeme::EQ);
			Type type = parse_type(parser, mod);
			consume(parser, Lexeme::SEMICOLON);
			mod.close_scope();

			AliasDef *def = mod.scope()->add_alias(string(alias_name), std::move(type));
			def->type_params = std::move(type_params);
			mod.add_item(def);
		} break;

		case Lexeme::EXTERN:
		{
			if(try_consume(parser, Lexeme::PROC))
			{
				ProcDef *proc = parse_proc(parser, mod);
				if(proc->body)
					throw ParseError("Extern procedures must not have a body");

				mod.add_item(proc);
			}
			else if(try_consume(parser, Lexeme::STRUCT))
			{
				StructDef *struct_ = parse_struct(parser, mod, true);
				if(not struct_->ctor_without_parens)
					throw ParseError("Extern structs must not have any members");

				struct_->is_extern = true;
				mod.add_item(struct_);
			}
			else
				throw ParseError("Expected proc or struct declaration after extern");
		} break;

		default: throw ParseError("Invalid token while parsing top-level item: "s + str(tok.kind));
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
namespace std
{
	template<>
	struct hash<::TypeDeductionVar>
	{
		size_t operator () (::TypeDeductionVar const &v) const
		{
			return ::compute_hash(v.id);
		}
	};
}

// Stores temporary data needed during semantic analysis. Can be discarded afterwards
struct SemaContext
{
	explicit SemaContext(Module &mod) :
		m_mod(mod) {}

	TypeDeductionVar new_type_deduction_var()
	{
		return TypeDeductionVar(next_type_deduction_var_id++);
	}

	TypeDeductionVar new_type_deduction_var(TypeConstraint &&constraint)
	{
		TypeDeductionVar var = new_type_deduction_var();
		constraints.emplace(var, std::move(constraint));

		return var;
	}

	KnownIntTypeVar new_int_type_var(XInt64 value)
	{
		KnownIntTypeVar var(next_known_int_var_id++);
		known_ints.emplace(var, pair{value, value});

		return var;
	}

	KnownIntTypeVar new_int_type_var(XInt64 low, XInt64 high)
	{
		KnownIntTypeVar var(next_known_int_var_id++);
		known_ints.emplace(var, pair{low, high});

		return var;
	}

	pair<XInt64, XInt64> const* try_get_known_int(Type const &type) const
	{
		if(VarType const *t = std::get_if<VarType>(&type))
		{
			if(KnownIntTypeVar const *var = std::get_if<KnownIntTypeVar>(t))
				return &known_ints.at(*var);
		}

		return nullptr;
	}

	TypeConstraint* try_get_constraints(TypeDeductionVar var)
	{
		auto it = constraints.find(var);
		if(it == constraints.end())
			return nullptr;

		return &it->second;
	}

	void set_current_proc(ProcDef const *proc) { m_current_proc = proc; }

	Module& mod() { return m_mod; }
	ProcDef const* proc() { return m_current_proc; }


private:
	// The lower bound of constraints that the type must satisfy
	unordered_map<TypeDeductionVar, TypeConstraint> constraints;

	// TODO(performance): Consider replacing with a std::vector that is cleared after typechecking a
	//                    single full expression
	unordered_map<KnownIntTypeVar, pair<XInt64, XInt64>> known_ints;

	Module &m_mod;
	ProcDef const *m_current_proc = nullptr;
	uint32_t next_type_deduction_var_id = 0;
	uint32_t next_known_int_var_id = 0;
};

Type instantiate_known_int(pair<XInt64, XInt64> range)
{
	if(integer_assignable_to(BuiltinType::I32, range.first) and integer_assignable_to(BuiltinType::I32, range.second))
		return BuiltinType::I32;
	else
		return *smallest_int_type_for(range.first, range.second);
}

void TypeEnv::instantiate_int_vars(SemaContext const &ctx)
{
	for(auto &[var, known_int_vars]: m_deferred_type_instantiations)
	{
		Type &final_type = lookup(var);

		if(pair<XInt64, XInt64> const *value = ctx.try_get_known_int(final_type))
			final_type = instantiate_known_int(*value);
		
		for(KnownIntTypeVar int_var: known_int_vars)
		{
			auto res = m_mapping.emplace(int_var, clone(final_type));
			assert(res.second);
		}
	}

	m_deferred_type_instantiations.clear();
}


void resolve_identifiers(SemaContext &ctx);
void typecheck(SemaContext &ctx);
void compute_type_layouts(Module &mod);

void sema(Module &mod)
{
	SemaContext ctx(mod);

	resolve_identifiers(ctx);
	typecheck(ctx);
	compute_type_layouts(mod);
}


//--------------------------------------------------------------------
// Type relations
//--------------------------------------------------------------------
BuiltinType smallest_int_type_for(XInt64 val)
{
	if(val.is_negative())
	{
		if(std::numeric_limits<int8_t>::min() <= val.as_signed())
			return BuiltinType::I8;

		if(std::numeric_limits<int32_t>::min() <= val.as_signed())
			return BuiltinType::I32;

		return BuiltinType::ISIZE;
	}
	else
	{
		if(val.as_unsigned() <= std::numeric_limits<int8_t>::max())
			return BuiltinType::I8;

		if(val.as_unsigned() <= std::numeric_limits<uint8_t>::max())
			return BuiltinType::U8;

		if(val.as_unsigned() <= std::numeric_limits<int32_t>::max())
			return BuiltinType::I32;

		if(val.as_unsigned() <= std::numeric_limits<uint32_t>::max())
			return BuiltinType::U32;

		if(val.as_unsigned() <= std::numeric_limits<int64_t>::max())
			return BuiltinType::ISIZE;

		return BuiltinType::USIZE;
	}
}

bool integer_assignable_to(BuiltinType type, XInt64 val)
{
	switch(type)
	{
		case BuiltinType::NEVER: assert(!"integer_assignable_to: NEVER");
		case BuiltinType::UNIT:  assert(!"integer_assignable_to: UNIT");
		case BuiltinType::BOOL:  assert(!"integer_assignable_to: BOOL");
		case BuiltinType::I8:    return XInt64(int64_t(-128)) <= val && val <= XInt64(uint64_t(127));
		case BuiltinType::U8:    return XInt64(int64_t(0)) <= val && val <= XInt64(uint64_t(255));
		case BuiltinType::I32:   return XInt64(int64_t(-2147483648)) <= val && val <= XInt64(uint64_t(2147483647));
		case BuiltinType::U32:   return XInt64(int64_t(0)) <= val && val <= XInt64(uint64_t(127));
		case BuiltinType::ISIZE: return XInt64(std::numeric_limits<int64_t>::min()) <= val && val <= XInt64(std::numeric_limits<int64_t>::max());
		case BuiltinType::USIZE: return XInt64(std::numeric_limits<uint64_t>::min()) <= val && val <= XInt64(std::numeric_limits<uint64_t>::max());
		default: UNREACHABLE;
	}
}

bool is_builtin_int_type(BuiltinType type)
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
	assert(is_builtin_int_type(type));

	switch(type)
	{
		case BuiltinType::NEVER: assert(!"is_signed: NEVER");
		case BuiltinType::UNIT:  assert(!"is_signed: UNIT");
		case BuiltinType::BOOL:  assert(!"is_signed: BOOL");
		case BuiltinType::I8:  return true;
		case BuiltinType::U8:  return false;
		case BuiltinType::I32:   return true;
		case BuiltinType::U32:   return false;
		case BuiltinType::ISIZE: return true;
		case BuiltinType::USIZE: return false;
	}

	UNREACHABLE;
}

bool is_unsigned(BuiltinType type)
{
	return not is_signed(type);
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

optional<BuiltinType> merge_int_types(BuiltinType a, BuiltinType b)
{
	bool signed_type = is_signed(a) || is_signed(b);
	size_t size = std::max(get_layout(a).size, get_layout(b).size);
	if(is_signed(a) != is_signed(b))
		size *= 2;

	switch(size)
	{
		case 1:
			return signed_type ? BuiltinType::I8 : BuiltinType::U8;

		case 2:
		case 4:
			return signed_type ? BuiltinType::I32 : BuiltinType::U32;

		case 8:
			return signed_type ? BuiltinType::ISIZE : BuiltinType::USIZE;

		default: return nullopt;
	}
}

struct AbstractInt
{
	BuiltinType smallest_type;
	bool is_negative;
};

AbstractInt get_abstract_int(XInt64 val)
{
	if(val.is_negative())
	{
		if(std::numeric_limits<int8_t>::min() <= val.as_signed())
			return AbstractInt{BuiltinType::I8, true};

		if(std::numeric_limits<int32_t>::min() <= val.as_signed())
			return AbstractInt{BuiltinType::I32, true};

		return AbstractInt{BuiltinType::ISIZE, true};
	}
	else
	{
		if(val.as_unsigned() <= std::numeric_limits<int8_t>::max())
			return AbstractInt{BuiltinType::I8, false};

		if(val.as_unsigned() <= std::numeric_limits<uint8_t>::max())
			return AbstractInt{BuiltinType::U8, false};

		if(val.as_unsigned() <= std::numeric_limits<int32_t>::max())
			return AbstractInt{BuiltinType::I32, false};

		if(val.as_unsigned() <= std::numeric_limits<uint32_t>::max())
			return AbstractInt{BuiltinType::U32, false};

		if(val.as_unsigned() <= std::numeric_limits<int64_t>::max())
			return AbstractInt{BuiltinType::ISIZE, false};

		return AbstractInt{BuiltinType::USIZE, false};
	}
}

BuiltinType get_int_type_for(size_t size, bool is_signed)
{
	switch(size)
	{
		case 1:
			return is_signed ? BuiltinType::I8 : BuiltinType::U8;

		case 2:
		case 4:
			return is_signed ? BuiltinType::I32 : BuiltinType::U32;

		case 8:
			return is_signed ? BuiltinType::ISIZE : BuiltinType::USIZE;

		default: UNREACHABLE;
	}
}


// smallest_int_type_for(3, 220) == u8
//
optional<BuiltinType> smallest_int_type_for(XInt64 low, XInt64 high)
{
	AbstractInt low_abs = get_abstract_int(low);
	AbstractInt high_abs = get_abstract_int(high);

	if(low_abs.smallest_type == high_abs.smallest_type)
		return low_abs.smallest_type;

	if(low_abs.is_negative == high_abs.is_negative)
	{
		size_t required_size = std::max(
			get_layout(low_abs.smallest_type).size,
			get_layout(high_abs.smallest_type).size
		);

		bool use_signed_type = is_signed(low_abs.smallest_type) and is_signed(high_abs.smallest_type);
		return get_int_type_for(required_size, use_signed_type);
	}
	else
	{
		size_t required_size = std::max(
			get_layout(low_abs.smallest_type).size,
			get_layout(high_abs.smallest_type).size
		) * 2;

		return get_int_type_for(required_size, true);
	}
}


size_t size_of(BuiltinType type)
{
	return get_layout(type).size;
}

bool builtin_losslessly_convertible(BuiltinType dest, BuiltinType src)
{
	if(!is_builtin_int_type(dest) || !is_builtin_int_type(src))
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

optional<Type> common_int_type(Type const &a, Type const &b, SemaContext &ctx)
{
	assert(is_integer_type(a));
	assert(is_integer_type(b));

	BuiltinType const *a_bt = std::get_if<BuiltinType>(&a);
	BuiltinType const *b_bt = std::get_if<BuiltinType>(&b);
	pair<XInt64, XInt64> const *a_known = ctx.try_get_known_int(a);
	pair<XInt64, XInt64> const *b_known = ctx.try_get_known_int(b);

	if(a_bt && b_bt)
	{
		if(*a_bt == *b_bt)
			return *a_bt;

		if(builtin_losslessly_convertible(*a_bt, *b_bt))
			return *a_bt;

		if(builtin_losslessly_convertible(*b_bt, *a_bt))
			return *b_bt;
	}
	else if(a_bt && b_known)
	{
		if(integer_assignable_to(*a_bt, b_known->first) and integer_assignable_to(*a_bt, b_known->second))
			return *a_bt;
	}
	else if(a_known && b_bt)
	{
		if(integer_assignable_to(*b_bt, a_known->first) and integer_assignable_to(*b_bt, a_known->second))
			return *b_bt;
	}
	else
	{
		XInt64 low = a_known->first < b_known->first ? a_known->first : b_known->first;
		XInt64 high = a_known->second > b_known->second ? a_known->second : b_known->second;
		return Type(VarType(ctx.new_int_type_var(low, high)));
	}

	return nullopt;
}


bool equiv(Type const &a, Type const &b)
{
	if(a.index() != b.index())
		return false;

	return a | match
	{
		[&](BuiltinType const &ta)
		{
			BuiltinType const &tb = std::get<BuiltinType>(b);
			return ta == tb;
		},
		[&](VarType const &ta)
		{
			if(VarType const *tb = std::get_if<VarType>(&b))
				return ta == *tb;

			return false;
		},
		[&](PointerType const &ta)
		{
			if(PointerType const *tb = std::get_if<PointerType>(&b))
				return ta.mutability == tb->mutability && equiv(*ta.target_type, *tb->target_type);

			return false;
		},
		[&](ManyPointerType const &ta)
		{
			if(ManyPointerType const *tb = std::get_if<ManyPointerType>(&b))
				return ta.mutability == tb->mutability && equiv(*ta.element_type, *tb->element_type);

			return false;
		},
		[&](StructType const &ta)
		{
			assert(ta.is_instantiated());
			if(StructType const *tb = std::get_if<StructType>(&b))
				return ta.inst() == tb->inst();

			return false;
		},
		[&](ProcType const &ta)
		{
			if(ProcType const *tb = std::get_if<ProcType>(&b))
				return ta.canonical_def == tb->canonical_def;

			return false;
		},
		[&](UnionType const &ta)
		{
			if(UnionType const *tb = std::get_if<UnionType>(&b))
				return ta.canonical_def == tb->canonical_def;

			return false;
		},
		[&](UnresolvedPath const&) -> bool { assert(!"equiv: UnresolvedPath"); },
		[&](UnappliedProcType const&) -> bool { assert(!"equiv: UnappliedProcType"); },
	};
}

bool operator < (Type const &a, Type const &b)
{
	if(a.index() != b.index())
		return a.index() < b.index();

	return a | match
	{
		[&](BuiltinType const &ta)
		{
			return (int)ta < (int)std::get<BuiltinType>(b);
		},
		[&](VarType const &ta)
		{
			VarType const &tb = std::get<VarType>(b);
			if(ta.index() != tb.index())
				return ta.index() < tb.index();

			return ta | match
			{
				[&](TypeParameterVar v)
				{
					// TODO Comparison based on pointers is non-deterministic. I need a way to
					//      generate unique names for type parameters
					return v.var < std::get<TypeParameterVar>(tb).var;
				},
				[&](TypeDeductionVar) -> bool { assert(!"operator < (Type, Type): VarType: TypeDeductionVar"); },
				[&](KnownIntTypeVar) -> bool { assert(!"operator < (Type, Type): VarType: KnownIntTypeVar"); },
			};
		},
		[&](PointerType const &ta)
		{
			PointerType const &tb = std::get<PointerType>(b);
			if(ta.mutability != tb.mutability)
				return (int)ta.mutability < (int)tb.mutability;

			return *ta.target_type < *tb.target_type;
		},
		[&](ManyPointerType const &ta)
		{
			ManyPointerType const &tb = std::get<ManyPointerType>(b);
			if(ta.mutability != tb.mutability)
				return (int)ta.mutability < (int)tb.mutability;

			return *ta.element_type < *tb.element_type;
		},
		[&](StructType const &ta)
		{
			StructType const &tb = std::get<StructType>(b);

			assert(ta.inst());
			assert(tb.inst());

			// TODO Comparison based on pointers is non-deterministic. I need a way to
			//      generate unique names for type parameters
			return ta.inst() < tb.inst();
		},
		[&](ProcType const &ta)
		{
			ProcType const &tb = std::get<ProcType>(b);

			// TODO Comparison based on pointers is non-deterministic. I need a way to
			//      generate unique names for type parameters
			return ta.canonical_def < tb.canonical_def;
		},
		[&](UnionType const &ta)
		{
			UnionType const &tb = std::get<UnionType>(b);

			// TODO Comparison based on pointers is non-deterministic. I need a way to
			//      generate unique names for type parameters
			return ta.canonical_def < tb.canonical_def;
		},
		[&](UnresolvedPath const&) -> bool { assert(!"operator < (Type, Type): UnresolvedPath"); },
		[&](UnappliedProcType const&) -> bool { assert(!"operator < (Type, Type): UnappliedProcType"); },
	};
}


enum class UnificationMode
{
	VALUE_ASSIGNMENT,

	POINTER_ASSIGNMENT,

	// Find a common type while treating both sides equally. The common type might differ from both
	// the left and the right side (e.g., unifying i8 and u8 yields i16)
	COMMON_TYPE,

	EQUAL,
};

void unify(
	Type const &dest,
	Type const &src,
	TypeEnv &subst,
	SemaContext &ctx,
	UnificationMode mode,
	Expr *src_expr = nullptr
);

bool is_type_assignable(Type const &dest, Type const &src, SemaContext &ctx, Expr *src_expr = nullptr)
{
	try {
		TypeEnv subst;
		unify(dest, src, subst, ctx, UnificationMode::VALUE_ASSIGNMENT, src_expr);
		return true;
	} catch (ParseError const&) {
		return false;
	}
}

bool is_type_assignable(Type const &dest, Type const &src, SemaContext &ctx, TypeEnv &subst, Expr *src_expr = nullptr)
{
	try {
		unify(dest, src, subst, ctx, UnificationMode::VALUE_ASSIGNMENT, src_expr);
		return true;
	} catch (ParseError const&) {
		return false;
	}
}


bool is_expr_assignable(Type const &dest, Expr &src, SemaContext &ctx)
{
	if(IntLiteralExpr const *src_val = std::get_if<IntLiteralExpr>(&src))
	{
		if(BuiltinType const *dest_int_type = std::get_if<BuiltinType>(&dest))
			return builtin_losslessly_convertible(*dest_int_type, smallest_int_type_for(src_val->value));
	}

	assert(src.type);
	return is_type_assignable(dest, *src.type, ctx, &src);
}


MemoryLayout compute_union_layout(UnionTypeDef const *union_, Module &mod, unordered_set<TypeDefInstance> &seen);
MemoryLayout compute_own_layout(StructDefInstance *struct_, Module &mod, unordered_set<TypeDefInstance> &seen);

Type const* is_optional_ptr(StructType const *struct_)
{
	while(struct_->parent())
		struct_ = struct_->parent();

	if(struct_->name() == "Option" and (is<PointerType>(struct_->type_args().args[0]) or is<ManyPointerType>(struct_->type_args().args[0])))
		return &struct_->type_args().args[0];

	return nullptr;
}

Type const* is_optional_ptr(Type const &type)
{
	if(StructType const *st = std::get_if<StructType>(&type))
		return is_optional_ptr(st);

	return nullptr;
}

MemoryLayout compute_layout(
	Type const &type,
	Module &mod,
	unordered_set<TypeDefInstance> *parent_type_deps,
	unordered_set<TypeDefInstance> &seen
)
{
	return type | match
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
			assert(t.is_instantiated());
			if(parent_type_deps)
				parent_type_deps->insert(t.inst());

			StructType const *cur = &t;
			while(cur->parent())
				cur = cur->parent();

			if(is_optional_ptr(cur))
				return MemoryLayout{.size = 8, .alignment = 8};

			return compute_own_layout(cur->inst(), mod, seen);
		},
		[&](ProcType const&) -> MemoryLayout { assert(!"compute_layout: ProcType"); },
		[&](UnionType const &t) -> MemoryLayout
		{
			if(parent_type_deps)
				parent_type_deps->insert(t.canonical_def);

			return compute_union_layout(t.canonical_def, mod, seen);
		},
		[&](UnresolvedPath const&) -> MemoryLayout { assert(!"compute_layout: UnresolvedPath"); },
		[&](UnappliedProcType const&) -> MemoryLayout { assert(!"compute_layout: UnappliedProcType"); },
	};
}

MemoryLayout compute_union_layout(UnionTypeDef const *union_, Module &mod, unordered_set<TypeDefInstance> &seen)
{
	if(UnionTypeInfo *info = mod.try_get_union_info(union_))
		return info->layout;

	if(!seen.insert(union_).second)
		throw ParseError("Cyclic type definition");

	BuiltinType discr_type = smallest_int_type_for(XInt64(union_->alternatives.size()));

	UnionTypeInfo info;
	info.layout.extend(get_layout(discr_type));

	MemoryLayout alt_layouts;
	for(Type const &alt: union_->alternatives)
		alt_layouts.extend(compute_layout(alt, mod, &info.type_deps, seen));

	info.layout.extend(alt_layouts);
	return mod.set_union_info(union_, std::move(info))->layout;
}

MemoryLayout compute_own_layout(StructDefInstance *struct_, Module &mod, unordered_set<TypeDefInstance> &seen)
{
	if(struct_->sema_done)
		return *struct_->own_layout;

	// We do cycle detection on StructDefs and not on StructInstanceDefs. Not sure if this is
	// necessary
	if(!seen.insert(struct_).second)
		throw ParseError("Cyclic type definition");

	struct_->own_layout = MemoryLayout{};

	// Reserve space for the discriminator
	if(struct_->def->num_cases > 0)
	{
		struct_->discriminator_type = smallest_int_type_for(XInt64(struct_->def->num_cases));
		struct_->own_layout->extend(get_layout(*struct_->discriminator_type));
	}

	// Reserve space for initial variable members
	size_t member_idx = 0;
	for(;member_idx < struct_->members.size(); ++member_idx)
	{
		InstanceMember &member = struct_->members[member_idx];
		if(Parameter *var_member = std::get_if<Parameter>(&member))
		{
			MemoryLayout var_layout = compute_layout(var_member->type, mod, &struct_->type_deps, seen);
			struct_->own_layout->extend(var_layout);
		}
		else
			break;
	}

	// Reserve space for case members
	if(struct_->def->num_cases > 0)
	{
		struct_->cases_layout = CaseMemberRegion{};
		struct_->cases_layout->start = struct_->own_layout->size;
		MemoryLayout case_members_layout{};
		for(;member_idx < struct_->members.size(); ++member_idx)
		{
			InstanceMember &member = struct_->members[member_idx];
			if(StructDefInstance **case_member = std::get_if<StructDefInstance*>(&member))
			{
				MemoryLayout case_layout = compute_own_layout(*case_member, mod, seen);

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
		InstanceMember &member = struct_->members[member_idx];
		if(Parameter *var_member = std::get_if<Parameter>(&member))
		{
			MemoryLayout var_layout = compute_layout(var_member->type, mod, &struct_->type_deps, seen);
			struct_->own_layout->extend(var_layout);
		}
		else {
			UNREACHABLE;
		}
	}

	struct_->sema_done = true;
	return *struct_->own_layout;
}


MemoryLayout compute_layout(Type const &type, Module &mod)
{
	unordered_set<TypeDefInstance> seen;
	return compute_layout(type, mod, nullptr, seen);
}

void compute_type_layouts(Scope *scope)
{
	for(ItemDef &item: scope->item_defs())
	{
		if(StructDef *struct_ = std::get_if<StructDef>(&item))
		{
			for(auto &[_, instance]: struct_->instances)
			{
				if(not instance.is_concrete())
					continue;

				unordered_set<TypeDefInstance> seen;
				compute_own_layout(&instance, *scope->mod(), seen);
			}
		}
	}
}

void compute_type_layouts(Module &mod)
{
	compute_type_layouts(mod.global());

	for(UnionTypeDef const &union_: mod.union_types())
	{
		unordered_set<TypeDefInstance> seen;
		compute_union_layout(&union_, mod, seen);
	}
}


//--------------------------------------------------------------------
// Resolve identifiers
//--------------------------------------------------------------------
void resolve_types(Type &type, SemaContext &ctx);

void type_substitute(Type &type, TypeEnv const &type_env)
{
	type | match
	{
		[&](BuiltinType&) {},
		[&](VarType &t)
		{
			auto it = type_env.find(t);
			if(it != type_env.end())
				type = clone(it->second);
		},
		[&](PointerType &t)
		{
			type_substitute(*t.target_type, type_env);
		},
		[&](ManyPointerType &t)
		{
			type_substitute(*t.element_type, type_env);
		},
		[&](StructType &t)
		{
			// The way type_substitute() is used should ensure that `type_env` does not affect the
			// type arguments of a fully instantiated StructType
			if(not t.is_instantiated())
			{
				StructType *cur = &t;
				while(cur)
				{
					for(Type &arg: cur->type_args())
						type_substitute(arg, type_env);

					cur = cur->parent();
				}
			}
		},
		[&](ProcType&)
		{
			// The way type_substitute() is used should ensure that `type_env` does not affect the
			// type arguments of a fully instantiated ProcType
		},
		[&](UnionType&)
		{
			// The way type_substitute() is used should ensure that `type_env` does not affect the
			// type arguments of a fully instantiated UnionType
		},
		[&](UnappliedProcType &t)
		{
			for(Type &arg: t.type_args)
				type_substitute(arg, type_env);

		},
		[&](UnresolvedPath&) { assert(!"type_substitute: Type: UnresolvedPath"); },
	};
}

Type resolve_path_to_type(UnresolvedPath &&path, SemaContext &ctx);

variant<Type, Expr> resolve_path(UnresolvedPath &&path, SemaContext &ctx)
{
	for(Type &arg: path.type_args)
		resolve_types(arg, ctx);

	NullableOwnPtr<StructType> resolved_parent;
	ItemDef *resolved_item = path.context | match
	{
		[&](Scope *scope)
		{
			return scope->try_lookup_item(path.name);
		},
		[&](OwnPtr<UnresolvedPath> &parent_path)
		{
			Type parent_type = resolve_path_to_type(std::move(*parent_path), ctx);
			if(StructType *st = std::get_if<StructType>(&parent_type))
				resolved_parent = std::make_unique<StructType>(std::move(*st));
			else
				throw ParseError("Expected struct type");

			Scope *scope = resolved_parent->def()->scope;
			return scope->try_lookup_item(path.name, false);
		}
	};

	if(not resolved_item)
		throw ParseError("Undeclared identifier: "s + path.name);

	return *resolved_item | match
	{
		[&](StructDef &struct_) -> variant<Type, Expr>
		{
			assert(is<NoParent>(struct_.parent) == (resolved_parent == nullptr));
			assert(path.type_args.size() <= struct_.type_params.size());

			size_t num_missing_args = struct_.type_params.size() - path.type_args.size();
			TypeArgList remaining_args;
			remaining_args.append(path.type_args);
			while(num_missing_args--)
				remaining_args.args.push_back(ctx.new_type_deduction_var());

			assert(remaining_args.size() == struct_.type_params.size());
			return StructType(&struct_, std::move(remaining_args), std::move(resolved_parent));
		},
		[&](ProcDef &proc) -> variant<Type, Expr>
		{
			assert(path.type_args.size() <= proc.type.type_params.size());

			size_t num_missing_args = proc.type.type_params.size() - path.type_args.size();
			TypeArgList remaining_args;
			remaining_args.append(path.type_args);
			while(num_missing_args--)
				remaining_args.args.push_back(ctx.new_type_deduction_var());

			return Expr(UnappliedProcExpr(&proc, std::move(remaining_args)), {});
		},
		[&](AliasDef &alias) -> variant<Type, Expr>
		{
			assert(path.type_args.size() <= alias.type_params.size());

			resolve_types(alias.aliased_type, ctx);

			TypeEnv env;
			size_t arg_idx = 0;
			while(arg_idx < path.type_args.size())
			{
				env.add_unique(TypeParameterVar(alias.type_params[arg_idx]), clone(path.type_args.args[arg_idx]));
				arg_idx += 1;
			}
			while(arg_idx < alias.type_params.size())
			{
				env.add_unique(TypeParameterVar(alias.type_params[arg_idx]), ctx.new_type_deduction_var());
				arg_idx += 1;
			}

			Type type = clone(alias.aliased_type);
			type_substitute(type, env);

			return type;
		},
	};
}

Type resolve_path_to_type(UnresolvedPath &&path, SemaContext &ctx)
{
	variant<Type, Expr> result = resolve_path(std::move(path), ctx);
	return result | match
	{
		[&](Type &type)
		{
			return std::move(type);
		},
		[&](Expr&) -> Type { throw ParseError("Expected path to resolve to type, got expr"); },
	};
}

Expr resolve_path_to_expr(UnresolvedPath &&path, SemaContext &ctx)
{
	variant<Type, Expr> result = resolve_path(std::move(path), ctx);
	return result | match
	{
		[&](Type &type) -> Expr
		{
			Expr expr(ConstructorExpr(std::move(type)), {});
			if(StructType *struct_type = std::get_if<StructType>(&type))
			{
				if(struct_type->def()->ctor_without_parens)
					expr = Expr(CallExpr(std::move(expr), {}), {});
			}

			return expr;
		},
		[&](Expr &expr)
		{
			return std::move(expr);
		},
	};
}

void instantiate_type(Type &type, TypeEnv const &type_env, SemaContext &ctx);

void resolve_types(Type &type, SemaContext &ctx)
{
	type | match
	{
		[&](BuiltinType&) {},
		[&](VarType&) {},
		[&](PointerType &t)
		{
			resolve_types(*t.target_type, ctx);
		},
		[&](ManyPointerType &t)
		{
			resolve_types(*t.element_type, ctx);
		},
		[&](StructType&) {},
		[&](ProcType &t)
		{
			if(not t.canonical_def)
			{
				resolve_types(t.def->ret, ctx);
				for(Parameter &p: t.def->params)
					resolve_types(p.type, ctx);

				t.canonical_def = ctx.mod().get_canonical(*t.def);
			}
		},
		[&](UnionType &t)
		{
			if(not t.canonical_def)
			{
				for(Type &alt: t.def->alternatives)
				{
					resolve_types(alt, ctx);
					instantiate_type(alt, {}, ctx);
				}

				t.canonical_def = ctx.mod().get_canonical(*t.def);
			}
		},
		[&](UnresolvedPath &p)
		{
			type = resolve_path_to_type(std::move(p), ctx);
		},
		[&](UnappliedProcType&) {},
	};
}


void resolve_identifiers(Stmt &stmt, SemaContext &ctx);
void resolve_identifiers(Pattern &pattern, SemaContext &ctx);

void resolve_identifiers(Expr &expr, SemaContext &ctx)
{
	expr | match
	{
		[&](IntLiteralExpr&) {},
		[&](BoolLiteralExpr&) {},
		[&](StringLiteralExpr&) {},
		[&](UnresolvedPath &p)
		{
			expr = resolve_path_to_expr(std::move(p), ctx);
		},
		[&](UnappliedProcExpr&) {},
		[&](VarExpr&) {},
		[&](ConstructorExpr &t)
		{
			resolve_types(t.ctor, ctx);
		},
		[&](ProcExpr&) {},
		[&](UnaryExpr &e)
		{
			resolve_identifiers(*e.sub, ctx);
		},
		[&](BinaryExpr &e)
		{
			resolve_identifiers(*e.left, ctx);
			resolve_identifiers(*e.right, ctx);
		},
		[&](AddressOfExpr &e)
		{
			resolve_identifiers(*e.object_expr, ctx);
		},
		[&](DerefExpr &e)
		{
			resolve_identifiers(*e.ptr_expr, ctx);
		},
		[&](IndexExpr &e)
		{
			resolve_identifiers(*e.ptr_expr, ctx);
			resolve_identifiers(*e.idx_expr, ctx);
		},
		[&](MemberAccessExpr &e)
		{
			resolve_identifiers(*e.object, ctx);
		},
		[&](AssignmentExpr &e)
		{
			resolve_identifiers(*e.lhs, ctx);
			resolve_identifiers(*e.rhs, ctx);
		},
		[&](AsExpr &e)
		{
			resolve_identifiers(*e.src_expr, ctx);
			resolve_types(e.target_type, ctx);
		},
		[&](CallExpr &e)
		{
			resolve_identifiers(*e.callable, ctx);
			for(Argument &arg: e.args)
				resolve_identifiers(*arg.expr, ctx);
		},
		[&](SizeOfExpr &e)
		{
			resolve_types(e.type, ctx);
		},
		[&](MakeExpr &e)
		{
			resolve_identifiers(*e.init, ctx);
			resolve_identifiers(*e.addr, ctx);
		},
		[&](UnionInitExpr &e)
		{
			resolve_identifiers(*e.value, ctx);
		},
		[&](TryExpr &e)
		{
			resolve_identifiers(*e.subject, ctx);
			if(e.match_stmt) resolve_identifiers(*e.match_stmt, ctx);
			if(e.result_expr) resolve_identifiers(*e.result_expr, ctx);
		},
	};
}

void resolve_identifiers(Pattern &pattern, SemaContext &ctx)
{
	if(pattern.provided_type)
		resolve_types(*pattern.provided_type, ctx);

	pattern | match
	{
		[&](VarPattern&) {},
		[&](DerefPattern &p) { resolve_identifiers(*p.sub, ctx); },
		[&](AddressOfPattern &p) { resolve_identifiers(*p.sub, ctx); },
		[&](ConstructorPattern &p)
		{
			resolve_types(p.ctor, ctx);
			for(PatternArgument &arg: p.args)
				resolve_identifiers(*arg.pattern, ctx);
		},
		[&](WildcardPattern&) {},
	};
}

void resolve_identifiers(Stmt &stmt, SemaContext &ctx)
{
	stmt | match
	{
		[&](LetStmt &s)
		{
			resolve_identifiers(s.lhs, ctx);
			if(s.init_expr) resolve_identifiers(*s.init_expr, ctx);
		},
		[&](ExprStmt &s)
		{
			resolve_identifiers(s.expr, ctx);
		},
		[&](BlockStmt &s)
		{
			for(OwnPtr<Stmt> &stmt: s.stmts)
				resolve_identifiers(*stmt, ctx);
		},
		[&](ReturnStmt &s)
		{
			if(s.ret_expr)
				resolve_identifiers(*s.ret_expr, ctx);
		},
		[&](IfStmt &s)
		{
			resolve_identifiers(s.condition, ctx);
			resolve_identifiers(*s.then, ctx);
			if(s.else_)
				resolve_identifiers(*s.else_, ctx);
		},
		[&](WhileStmt &s)
		{
			resolve_identifiers(s.condition, ctx);
			resolve_identifiers(*s.body, ctx);
		},
		[&](MatchStmt &s)
		{
			resolve_identifiers(s.expr, ctx);
			for(MatchArm &arm: s.arms)
			{
				resolve_identifiers(arm.capture, ctx);
				resolve_identifiers(*arm.stmt, ctx);
			}
		},
	};
}

void resolve_identifiers(vector<Parameter> &params, SemaContext &ctx)
{
	for(Parameter &param: params)
	{
		resolve_types(param.type, ctx);
		if(param.default_value)
			resolve_identifiers(*param.default_value, ctx);
	}
}

void resolve_identifiers(TopLevelItem item, SemaContext &ctx)
{
	item | match
	{
		[&](ProcDef *def)
		{
			resolve_identifiers(def->type.params, ctx);

			for(VarDef *param: def->param_vars)
				resolve_types(param->type.value(), ctx);

			if(def->body)
				resolve_identifiers(*def->body, ctx);

			resolve_types(def->type.ret, ctx);
		},
		[&](StructDef *def)
		{
			for(Member &member: def->members)
			{
				member | match
				{
					[&](Parameter &var_member)
					{
						resolve_types(var_member.type, ctx);
						if(var_member.default_value)
							resolve_identifiers(*var_member.default_value, ctx);
					},
					[&](StructDef *case_member)
					{
						resolve_identifiers(case_member, ctx);
					},
				};
			}

			if(def->constructor_params)
				resolve_identifiers(*def->constructor_params, ctx);
		},
		[&](AliasDef *def)
		{
			resolve_types(def->aliased_type, ctx);
		},
	};
}


void resolve_identifiers(SemaContext &ctx)
{
	for(TopLevelItem item: ctx.mod().items())
		resolve_identifiers(item, ctx);
}


//--------------------------------------------------------------------
// Type instantiation
//--------------------------------------------------------------------
struct TypingHint
{
	TypingHint() = default;

	explicit TypingHint(TypeEnv *subst) :
		subst(subst) {}

	explicit TypingHint(Type const *type, TypeEnv *subst) :
		type(type),
		subst(subst) {}

	Type const *type = nullptr;
	TypeEnv *subst;

	TypingHint with_type(Type const *new_type)
	{
		return TypingHint(new_type, subst);
	}

	optional<BuiltinType> try_int_get_type() const
	{
		if(type)
		{
			if(BuiltinType const *bt = std::get_if<BuiltinType>(type))
			{
				if(is_builtin_int_type(*bt))
					return *bt;
			}
		}

		return nullopt;
	}
};


void typecheck(Expr &expr, SemaContext &ctx, Type const *type_hint = nullptr);

void instantiate_type(Type &type, TypeEnv const &type_env, SemaContext &ctx);
void instantiate_types(Expr &expr, TypeEnv const &type_env, SemaContext &ctx);

void fill_type_env(TypeEnv &type_env, vector<TypeVarDef*> const &type_params, TypeArgList &&type_args)
{
	assert(type_args.size() <= type_params.size());
	for(size_t i = 0; i < type_args.size(); ++i)
	{
		TypeVarDef *tparam = type_params[i];
		type_env.add_unique(TypeParameterVar(tparam), std::move(type_args.args[i]));
	}
}

bool is_concrete(Type const &type)
{
	return type | match
	{
		[&](BuiltinType const&) { return true; },
		[&](VarType const&) { return false; },
		[&](PointerType const &t)
		{
			return is_concrete(*t.target_type);
		},
		[&](ManyPointerType const &t)
		{
			return is_concrete(*t.element_type);
		},
		[&](StructType const &t)
		{
			for(Type const &arg: t.type_args())
			{
				if(not is_concrete(arg))
					return false;
			}

			return true;
		},
		[&](ProcType const &t)
		{
			for(Parameter &p: t.def->params)
			{
				if(not is_concrete(p.type))
					return false;
			}

			return is_concrete(t.def->ret);
		},
		[&](UnionType const &t)
		{
			for(Type &alt: t.def->alternatives)
			{
				if(not is_concrete(alt))
					return false;
			}

			return true;
		},
		[&](UnresolvedPath const&) -> bool { assert(!"is_concrete: UnresolvedPath"); },
		[&](UnappliedProcType const&) -> bool { assert(!"is_concrete: UnappliedProcType"); },
	};
}

bool is_fully_instantiated(Type const &type)
{
	return type | match
	{
		[&](BuiltinType const&) { return true; },
		[&](VarType const &t) { return not is<TypeDeductionVar>(t) && not is<KnownIntTypeVar>(t); },
		[&](PointerType const &t)
		{
			return is_fully_instantiated(*t.target_type);
		},
		[&](ManyPointerType const &t)
		{
			return is_fully_instantiated(*t.element_type);
		},
		[&](StructType const &t)
		{
			if(not t.is_instantiated())
			{
				StructType const *cur = &t;
				while(cur)
				{
					for(Type const &arg: cur->type_args())
					{
						if(not is_fully_instantiated(arg))
							return false;
					}

					cur = cur->parent();
				}
			}

			return true;
		},
		[&](ProcType const &t)
		{
			for(Parameter &p: t.def->params)
			{
				if(not is_fully_instantiated(p.type))
					return false;
			}

			return is_fully_instantiated(t.def->ret);
		},
		[&](UnionType const &t)
		{
			for(Type &alt: t.def->alternatives)
			{
				if(not is_fully_instantiated(alt))
					return false;
			}

			return true;
		},
		[&](UnresolvedPath const&) -> bool { assert(!"is_fully_instantiated: UnresolvedPath"); },
		[&](UnappliedProcType const &t)
		{
			for(Type const &arg: t.type_args)
			{
				if(not is_fully_instantiated(arg))
					return false;
			}

			return true;
		},
	};
}

bool typecheck_partial(Expr &expr, SemaContext &ctx, TypingHint hint);

StructDefInstance* instantiate_struct(
	StructDef *struct_,
	TypeArgList &&type_args, // Must already be fully resolved
	optional<StructType> outer_struct,
	SemaContext &ctx
)
{
	assert(type_args.size() == struct_->type_params.size());

	// Check if we have already instantiated the struct
	DefInstanceKey inst_key{clone(type_args), outer_struct ? outer_struct->inst() : nullptr};
	auto it = struct_->instances.find(inst_key);
	if(it != struct_->instances.end())
		return &it->second;

	bool args_concrete = true;
	for(Type &type: type_args)
	{
		if(not is_fully_instantiated(type)) {
			assert(!"instantiate_struct with type deduction vars");
		}

		if(not is_concrete(type))
			args_concrete = false;
	}

	StructDefInstance *inst = &struct_->instances.emplace(pair{std::move(inst_key), StructDefInstance()}).first->second;
	inst->def = struct_;
	inst->outer_struct = std::move(outer_struct);
	inst->type_args = clone(type_args);
	inst->own_args_concrete = args_concrete;

	inst->type_env = inst->outer_struct ? clone(inst->outer_struct->inst()->type_env) : TypeEnv();
	fill_type_env(inst->type_env, struct_->type_params, clone(type_args));

	if(struct_->constructor_params)
	{
		inst->constructor_params = vector<Parameter>();
		for(Parameter const &p: *struct_->constructor_params)
		{
			Parameter inst_param = clone(p);

			// Make sure to compute the type of the default value expression
			if(inst_param.default_value)
			{
				TypeEnv subst = clone(inst->type_env);;
				bool fully_instantiated = typecheck_partial(*inst_param.default_value, ctx, TypingHint(&inst_param.type, &subst));
				unify(inst_param.type, *inst_param.default_value->type, subst, ctx, UnificationMode::VALUE_ASSIGNMENT, inst_param.default_value.get());

				if(not fully_instantiated)
					instantiate_types(*inst_param.default_value, subst, ctx);
			}

			instantiate_type(inst_param.type, inst->type_env, ctx);
			inst->constructor_params->push_back(std::move(inst_param));
		}
	}

	inst->members.clear();
	for(Member const &m: struct_->members)
	{
		m | match
		{
			[&](Parameter const &p)
			{
				Parameter inst_param = clone(p);
				instantiate_type(inst_param.type, inst->type_env, ctx);
				inst->members.push_back(std::move(inst_param));
			},
			[&](StructDef *case_member)
			{
				StructDefInstance *case_inst = instantiate_struct(case_member, {}, StructType(inst), ctx);
				inst->members.push_back(case_inst);
			},
		};
	}

	ctx.mod().notify_new_struct_inst(inst);

	return inst;
}


void StructType::instantiate(TypeEnv const &type_env, SemaContext &ctx)
{
	if(is_instantiated())
	{
		// Evaluate types of outer struct instance, if it exists
		optional<StructType> outer_struct;
		if(m_inst->outer_struct)
		{
			outer_struct = m_inst->outer_struct->clone();
			outer_struct->instantiate(type_env, ctx);
		}

		// Evaluate the type args of `inst` under `type_env`
		TypeArgList new_type_args = ::clone(m_inst->type_args);
		for(Type &arg: new_type_args)
			instantiate_type(arg, type_env, ctx);

		// Create a new instance with the new type args
		m_inst = instantiate_struct(m_inst->def, std::move(new_type_args), std::move(outer_struct), ctx);
	}
	else
	{
		assert(m_def->name != "Option" or m_type_args.size() == 1);

		if(m_parent)
			m_parent->instantiate(type_env, ctx);

		for(Type &arg: m_type_args)
			instantiate_type(arg, type_env, ctx);

		optional<StructType> outer_struct;
		if(m_parent) outer_struct = std::move(*m_parent);
		m_inst = instantiate_struct(m_def, std::move(m_type_args), std::move(outer_struct), ctx);
		m_parent = nullptr;
	}
}


ProcDefInstance* instantiate_proc(
	ProcDef *proc,
	TypeArgList &&type_args, // Must already be fully resolved
	SemaContext &ctx
)
{
	assert(type_args.size() == proc->type.type_params.size());

	// Check if we have already instantiated the procedure
	auto it = proc->instances.find(type_args);
	if(it != proc->instances.end())
		return &it->second;

	bool args_concrete = true;
	for(Type &type: type_args)
	{
		if(not is_fully_instantiated(type)) {
			assert(!"instantiate_proc with type deduction vars");
		}

		if(not is_concrete(type))
			args_concrete = false;
	}

	ProcDefInstance *inst = &proc->instances.emplace(pair{clone(type_args), ProcDefInstance()}).first->second;
	inst->def = proc;
	inst->is_concrete = true;
	inst->type_args = clone(type_args);
	inst->is_concrete = args_concrete;
	inst->type = clone(proc->type);

	fill_type_env(inst->type_env, proc->type.type_params, clone(type_args));
	for(Parameter &p: inst->type.params)
	{
		instantiate_type(p.type, inst->type_env, ctx);

		// Make sure to compute the type of the default value expression
		if(p.default_value)
			typecheck(*p.default_value, ctx);
	}

	instantiate_type(inst->type.ret, inst->type_env, ctx);

	ctx.mod().notify_new_proc_inst(inst);

	return inst;
}

ProcType instantiate_proc_type(ProcTypeDef const &proc_def, TypeEnv const &type_env, SemaContext &ctx)
{
	ProcTypeDef new_type;
	for(TypeVarDef *p: proc_def.type_params)
	{
		if(not type_env.contains(TypeParameterVar(p)))
			new_type.type_params.push_back(p);
	}

	for(Parameter const &p: proc_def.params)
	{
		Parameter new_param = clone(p);
		instantiate_type(new_param.type, type_env, ctx);
		new_type.params.push_back(std::move(new_param));
	}

	new_type.ret = clone(proc_def.ret);
	instantiate_type(new_type.ret, type_env, ctx);

	ProcTypeDef *def = ctx.mod().add_proc_type(std::move(new_type));
	return ProcType{
		.def = def,
		.canonical_def = ctx.mod().get_canonical(*def),
	};
}

UnionType instantiate_union_type(UnionTypeDef const &union_, TypeEnv const &type_env, SemaContext &ctx)
{
	UnionTypeDef new_type;
	for(Type const &alt: union_.alternatives)
	{
		Type new_alt = clone(alt);
		instantiate_type(new_alt, type_env, ctx);
		new_type.alternatives.push_back(std::move(new_alt));
	}

	UnionType ut;
	ut.def = ctx.mod().add_union_type(std::move(new_type));
	ut.canonical_def = ctx.mod().get_canonical(*ut.def);

	return ut;
}


optional<TypeDeductionVar> try_get_type_deduction_var(Type const &type);

void instantiate_type(Type &type, TypeEnv const &type_env, SemaContext &ctx)
{
	type | match
	{
		[&](BuiltinType&) {},
		[&](VarType&)
		{
			do
			{
				auto it = type_env.find(std::get<VarType>(type));
				if(it == type_env.end())
					break;

				type = clone(it->second);
			}
			while(is<VarType>(type));

			if(pair<XInt64, XInt64> const *value = ctx.try_get_known_int(type))
				type = instantiate_known_int(*value);
			else if(VarType const *t = std::get_if<VarType>(&type))
			{
				if(is<TypeDeductionVar>(*t) || is<KnownIntTypeVar>(*t))
				{
					// If the type var is not in the environment it means it is either a TypeParameterVar
					// and we are in an abstract context, or it is a free TypeDeductionVar. In the
					// latter case we don't have enough information to deduce a type, which is an error.
					throw ParseError("Insufficiently constrained type");
				}
			}
			else
				// TODO Instantiate all types in TypeEnv just once
				instantiate_type(type, type_env, ctx);
		},
		[&](PointerType &t)
		{
			instantiate_type(*t.target_type, type_env, ctx);
		},
		[&](ManyPointerType &t)
		{
			instantiate_type(*t.element_type, type_env, ctx);
		},
		[&](StructType &t)
		{
			t.instantiate(type_env, ctx);
		},
		[&](ProcType &t)
		{
			type = instantiate_proc_type(*t.def, type_env, ctx);
		},
		[&](UnionType &t)
		{
			type = instantiate_union_type(*t.def, type_env, ctx);
		},
		[&](UnappliedProcType &t)
		{
			type = instantiate_proc_type(*t.type, type_env, ctx);
		},
		[&](UnresolvedPath&) { assert(!"instantiate_type: Type: UnresolvedPath"); },
	};
}


void instantiate_types(Stmt &stmt, TypeEnv const &type_env, SemaContext &ctx);
void instantiate_types(Pattern &pattern, TypeEnv const &type_env, SemaContext &ctx);

void instantiate_types(Expr &expr, TypeEnv const &type_env, SemaContext &ctx)
{
	instantiate_type(*expr.type, type_env, ctx);
	expr | match
	{
		[&](IntLiteralExpr&) {},
		[&](BoolLiteralExpr&) {},
		[&](StringLiteralExpr&) {},
		[&](UnresolvedPath const&) { assert(!"instantiate_types: Expr: UnresolvedPath"); },
		[&](UnappliedProcExpr &e)
		{
			for(Type &arg: e.type_args)
				instantiate_type(arg, type_env, ctx);

			ProcDefInstance *inst = instantiate_proc(e.proc, std::move(e.type_args), ctx);
			expr = expr.clone_as(ProcExpr(inst));
			expr.type = ProcType(&inst->type, ctx.mod().get_canonical(inst->type));
		},
		[&](VarExpr&) {},
		[&](ConstructorExpr &e)
		{
			instantiate_type(e.ctor, type_env, ctx);

			StructType &st = std::get<StructType>(e.ctor);

			// We need to update expr.type. Reason:
			// - the Parameters in expr.type are expected to be typechecked, in particular,
			//   Parameter.default_value.type must have been computed
			// - this is not the case for UnappliedConstructorExpr
			ProcTypeDef ctor_type;
			ctor_type.params = clone(*st.inst()->constructor_params);
			ctor_type.ret = StructType(st.inst());
			expr.type = ProcType{
				.def = ctx.mod().add_proc_type(clone(ctor_type)),
				.canonical_def = ctx.mod().get_canonical(ctor_type)
			};
		},
		[&](ProcExpr &e)
		{
			TypeArgList new_type_args = clone(e.inst->type_args);
			for(Type &arg: new_type_args)
				instantiate_type(arg, type_env, ctx);

			e.inst = instantiate_proc(e.inst->def, std::move(new_type_args), ctx);
			expr.type = ProcType(&e.inst->type, ctx.mod().get_canonical(e.inst->type));
		},
		[&](UnaryExpr &e)
		{
			instantiate_types(*e.sub, type_env, ctx);
		},
		[&](BinaryExpr &e)
		{
			instantiate_types(*e.left, type_env, ctx);
			instantiate_types(*e.right, type_env, ctx);
		},
		[&](AddressOfExpr &e)
		{
			instantiate_types(*e.object_expr, type_env, ctx);
		},
		[&](DerefExpr &e)
		{
			instantiate_types(*e.ptr_expr, type_env, ctx);
		},
		[&](IndexExpr &e)
		{
			instantiate_types(*e.ptr_expr, type_env, ctx);
			instantiate_types(*e.idx_expr, type_env, ctx);
		},
		[&](MemberAccessExpr &e)
		{
			instantiate_types(*e.object, type_env, ctx);
		},
		[&](AssignmentExpr &e)
		{
			instantiate_types(*e.lhs, type_env, ctx);
			instantiate_types(*e.rhs, type_env, ctx);
		},
		[&](AsExpr &e)
		{
			instantiate_types(*e.src_expr, type_env, ctx);
			instantiate_type(e.target_type, type_env, ctx);
		},
		[&](CallExpr &e)
		{
			instantiate_types(*e.callable, type_env, ctx);

			for(Argument &arg: e.args)
				instantiate_types(*arg.expr, type_env, ctx);
		},
		[&](SizeOfExpr &e)
		{
			instantiate_type(e.type, type_env, ctx);
		},
		[&](MakeExpr &e)
		{
			instantiate_types(*e.init, type_env, ctx);
			instantiate_types(*e.addr, type_env, ctx);
		},
		[&](UnionInitExpr &e)
		{
			instantiate_types(*e.value, type_env, ctx);
		},
		[&](TryExpr &e)
		{
			instantiate_types(*e.subject, type_env, ctx);
			if(e.match_stmt) instantiate_types(*e.match_stmt, type_env, ctx);
			if(e.result_expr) instantiate_types(*e.result_expr, type_env, ctx);
		},
	};
}

void instantiate_types(Pattern &pattern, TypeEnv const &type_env, SemaContext &ctx)
{
	instantiate_type(*pattern.type, type_env, ctx);
	if(pattern.provided_type)
		instantiate_type(*pattern.provided_type, type_env, ctx);

	pattern | match
	{
		[&](VarPattern &p)
		{
			if(p.var->type)
				instantiate_type(*p.var->type, type_env, ctx);
		},
		[&](DerefPattern &p)
		{
			instantiate_types(*p.sub, type_env, ctx);
		},
		[&](AddressOfPattern const &p)
		{
			instantiate_types(*p.sub, type_env, ctx);
		},
		[&](ConstructorPattern &p)
		{
			instantiate_type(p.ctor, type_env, ctx);
			for(PatternArgument &arg: p.args)
				instantiate_types(*arg.pattern, type_env, ctx);
		},
		[&](WildcardPattern&) {}
	};
}

void instantiate_types(Stmt &stmt, TypeEnv const &type_env, SemaContext &ctx)
{
	stmt | match
	{
		[&](LetStmt &s)
		{
			if(s.init_expr) instantiate_types(*s.init_expr, type_env, ctx);
			instantiate_types(s.lhs, type_env, ctx);
		},
		[&](ExprStmt &s)
		{
			instantiate_types(s.expr, type_env, ctx);
		},
		[&](BlockStmt &s)
		{
			for(OwnPtr<Stmt> &stmt: s.stmts)
				instantiate_types(*stmt, type_env, ctx);
		},
		[&](ReturnStmt &s)
		{
			if(s.ret_expr)
				instantiate_types(*s.ret_expr, type_env, ctx);
		},
		[&](IfStmt &s)
		{
			instantiate_types(s.condition, type_env, ctx);
			instantiate_types(*s.then, type_env, ctx);

			if(s.else_)
				instantiate_types(*s.else_, type_env, ctx);
		},
		[&](WhileStmt &s)
		{
			instantiate_types(s.condition, type_env, ctx);
			instantiate_types(*s.body, type_env, ctx);
		},
		[&](MatchStmt &s)
		{
			instantiate_types(s.expr, type_env, ctx);
			for(MatchArm &arm: s.arms)
			{
				instantiate_types(arm.capture, type_env, ctx);
				instantiate_types(*arm.stmt, type_env, ctx);
			}
		},
	};
}


//--------------------------------------------------------------------
// Unification
//--------------------------------------------------------------------
bool type_var_occurs_in(TypeDeductionVar var, Type const &type)
{
	return type | match
	{
		[&](BuiltinType const&) { return false; },
		[&](VarType const &t)
		{
			if(TypeDeductionVar const *dv = std::get_if<TypeDeductionVar>(&t))
				return *dv == var;

			return false;
		},
		[&](PointerType const &t)
		{
			return type_var_occurs_in(var, *t.target_type);
		},
		[&](ManyPointerType const &t)
		{
			return type_var_occurs_in(var, *t.element_type);
		},
		[&](StructType const &t)
		{
			if(not t.is_instantiated())
			{
				for(Type const &arg: t.type_args())
				{
					if(type_var_occurs_in(var, arg))
						return true;
				}
			}

			return false;
		},
		[&](ProcType const&) { return false; },
		[&](UnionType const&) { return false; },
		[&](UnappliedProcType const &t)
		{
			for(Type const &arg: t.type_args)
			{
				if(type_var_occurs_in(var, arg))
					return true;
			}

			return false;
		},
		[&](UnresolvedPath const&) -> bool { assert(!"type_var_occurs_in: UnresolvedPath"); },
	};
}


// Unify type deduction variables
//------------------------------------------------
optional<TypeDeductionVar> try_get_type_deduction_var(Type const &type)
{
	if(VarType const *t = std::get_if<VarType>(&type); t && is<TypeDeductionVar>(*t))
		return std::get<TypeDeductionVar>(*t);

	return nullopt;
}

bool try_unify_type_deduction_variables(Type const &left, Type const &right, TypeEnv &subst, SemaContext &ctx, UnificationMode mode, Expr *right_expr)
{
	optional<TypeDeductionVar> left_var = try_get_type_deduction_var(left);
	optional<TypeDeductionVar> right_var = try_get_type_deduction_var(right);
	if(left_var && right_var)
	{
		if(*left_var != *right_var)
		{
			if(Type const *existing_type = subst.try_lookup(*left_var))
				unify(*existing_type, right, subst, ctx, mode, right_expr);
			else
				subst.add_unique(*left_var, clone(right));


			TypeConstraint *right_constraint = ctx.try_get_constraints(*right_var);
			TypeConstraint *left_constraint = ctx.try_get_constraints(*left_var);
			if(right_constraint && left_constraint)
			{
				right_constraint->subsume(*left_constraint);
				left_constraint->subsume(*right_constraint);
			}
		}

		return true;
	}

	if(left_var || right_var)
	{
		optional<TypeDeductionVar> var = left_var ? left_var : right_var;
		Type const *type = left_var ? &right : &left;
		bool args_swapped = left_var ? false : true;

		assert(not type_var_occurs_in(*var, *type));
		if(Type const *existing_type = subst.try_lookup(*var))
		{
			if(is_integer_type(*existing_type) && is_integer_type(*type))
			{
				optional<Type> common_type = common_int_type(*existing_type, *type, ctx);
				if(not common_type)
					throw ParseError("Unification failed: No common integer type for " + str(*existing_type) + " and " + str(*type));

				subst.replace(*var, std::move(*common_type));
			}
			else
			{
				if(args_swapped)
					unify(*type, *existing_type, subst, ctx, mode, right_expr);
				else
					unify(*existing_type, *type, subst, ctx, mode, right_expr);
			}
		}
		else
			subst.add_unique(*var, clone(*type));


		TypeConstraint const *var_constraint = ctx.try_get_constraints(*var);
		if(var_constraint && not var_constraint->satisfied_by(*type))
			throw ParseError("Unification failed: Type does not satisfy constraints");

		return true;
	}

	return false;
}


// Unify known integer variables
//------------------------------------------------
optional<KnownIntTypeVar> try_get_known_int_var(Type const &type)
{
	if(VarType const *t = std::get_if<VarType>(&type); t && is<KnownIntTypeVar>(*t))
		return std::get<KnownIntTypeVar>(*t);

	return nullopt;
}

bool try_unify_integer_types(Type const &left, Type const &right, TypeEnv &subst, SemaContext &ctx, UnificationMode mode)
{
	if(not is_integer_type(left) or not is_integer_type(right))
		return false;

	optional<Type> common_type = common_int_type(left, right, ctx);
	if(not common_type)
		throw ParseError("Failed to unify integer types " + str(left) + " and " + str(right));

	switch(mode)
	{
		case UnificationMode::VALUE_ASSIGNMENT:
		{
			if(optional<KnownIntTypeVar> int_var = try_get_known_int_var(left))
				subst.add_unique(*int_var, clone(*common_type));
			else if(not equiv(left, *common_type))
					throw ParseError("Unification failed: Cannot assign " + str(right) + " to " + str(left));

			if(optional<KnownIntTypeVar> int_var = try_get_known_int_var(right))
				subst.add_unique(*int_var, clone(*common_type));
		} break;

		case UnificationMode::POINTER_ASSIGNMENT:
		{
			optional<KnownIntTypeVar> left_int_var = try_get_known_int_var(left);
			if(left_int_var)
				subst.add_unique(*left_int_var, clone(*common_type));

			optional<KnownIntTypeVar> right_int_var = try_get_known_int_var(right);
			if(right_int_var)
				subst.add_unique(*right_int_var, clone(*common_type));

			if(not left_int_var and not right_int_var)
			{
				if(not equiv(left, right))
					throw ParseError("Unification failed: Got " + str(right) + " and " + str(left));
			}

		} break;

		case UnificationMode::COMMON_TYPE:
		{
			if(optional<KnownIntTypeVar> left_int_var = try_get_known_int_var(left))
				subst.add_unique(*left_int_var, clone(*common_type));

			if(optional<KnownIntTypeVar> right_int_var = try_get_known_int_var(right))
				subst.add_unique(*right_int_var, clone(*common_type));
		} break;

		case UnificationMode::EQUAL:
		{
			if(optional<KnownIntTypeVar> left_int_var = try_get_known_int_var(left))
				subst.add_unique(*left_int_var, clone(*common_type));
			else if(not equiv(left, *common_type))
					throw ParseError("Unification failed: Got " + str(left) + " and " + str(right));

			if(optional<KnownIntTypeVar> right_int_var = try_get_known_int_var(right))
				subst.add_unique(*right_int_var, clone(*common_type));
			else if(not equiv(right, *common_type))
					throw ParseError("Unification failed: Got " + str(left) + " and " + str(right));
		} break;
	}

	return true;
}


// Unify struct types
//------------------------------------------------

bool equiv_struct(StructType const &left, StructType const &right, TypeEnv &subst, SemaContext &ctx)
{
	if(left.is_instantiated() and right.is_instantiated())
		return left.inst() == right.inst();

	if(left.def() != right.def())
		return false;

	assert(left.type_args().size() == right.type_args().size());
	for(size_t i = 0; i < left.type_args().size(); ++i)
	{
		Type const &dest_arg = left.type_args().args[i];
		Type const &src_arg = right.type_args().args[i];
		try {
			unify(dest_arg, src_arg, subst, ctx, UnificationMode::EQUAL);
		} catch(ParseError const&) {
			return false;
		}
	}

	assert(bool(left.parent()) == bool(right.parent()));
	if(left.parent())
	{
		if(not equiv_struct(*left.parent(), *right.parent(), subst, ctx))
			return false;
	}

	return true;
}

void create_type_env(StructType const &t, TypeEnv &result)
{
	if(t.parent())
		create_type_env(*t.parent(), result);

	for(size_t i = 0; i < t.type_args().size(); ++i)
		result.add_unique(TypeParameterVar(t.def()->type_params[i]), clone(t.type_args().args[i]));
}

TypeEnv create_type_env(StructType const &t)
{
	TypeEnv type_env;
	create_type_env(t, type_env);
	return type_env;
}

// Handle unification for both StructType and UnappliedStructPath
bool try_unify_structs(Type const &dest, Type const &src, TypeEnv &subst, SemaContext &ctx, UnificationMode mode, Expr *right_expr)
{
	StructType const *dest_struct = std::get_if<StructType>(&dest);
	if(not dest_struct)
		return false;


	// TODO(performance): Fast-path if both `src` and `dest` are instantiated

	StructType const *cur_src_struct = std::get_if<StructType>(&src);
	if(cur_src_struct)
	{
		StructType const *cur_dest_struct = dest_struct;

		if(cur_src_struct->def() != cur_dest_struct->def())
		{
			if(mode == UnificationMode::COMMON_TYPE || mode == UnificationMode::EQUAL)
				throw ParseError("Unification failed: Expected "s + str(dest) + ", got " + str(src));

			while(true)
			{
				if(cur_src_struct->def()->is_case_member_of(cur_dest_struct->def()))
				{
					cur_src_struct = cur_src_struct->parent();
					break;
				}

				cur_src_struct = cur_src_struct->parent();
				if(not cur_src_struct)
				{
					// Trying to unify both sides without a potential implicit ctor failed, so now
					// try unification with the implicit ctor
					goto TRY_UNIFY_WITH_IMPLICIT_CTOR;
				}
			}
		}
		assert(cur_dest_struct->def() == cur_src_struct->def());

		if(equiv_struct(*cur_dest_struct, *cur_src_struct, subst, ctx))
			return true;

		// Otherwise, go to TRY_UNIFY_WITH_IMPLICIT_CTOR
	}

TRY_UNIFY_WITH_IMPLICIT_CTOR:
	if(mode == UnificationMode::VALUE_ASSIGNMENT && dest_struct->def()->implicit_ctor)
	{
		Parameter param = clone(dest_struct->def()->implicit_ctor->constructor_params->at(0));
		type_substitute(param.type, create_type_env(*dest_struct));
		unify(param.type, src, subst, ctx, UnificationMode::VALUE_ASSIGNMENT, right_expr);

		if(right_expr)
		{
			assert(dest_struct->def()->implicit_ctor->type_params.empty() && "TODO");

			Type ctor_struct_type(StructType(
				dest_struct->def()->implicit_ctor,
				{},
				std::make_unique<StructType>(dest_struct->clone())
			));
			vector<Argument> ctor_args; ctor_args.push_back(Argument{
				.expr = std::make_unique<Expr>(std::move(*right_expr)),
				.name = {},
				.param_idx = 0
			});

			Expr ctor_expr(ConstructorExpr(clone(ctor_struct_type)), {});
			ProcTypeDef ctor_type{
				.type_params = {},
				.params = mk_vec(std::move(param)),
				.ret = clone(ctor_struct_type),
			};
			ctor_expr.type = Type(UnappliedProcType(std::move(ctor_type), {}));

			Expr call_expr(CallExpr(std::move(ctor_expr), std::move(ctor_args)), {});
			call_expr.type = clone(ctor_struct_type);

			*right_expr = Expr(AsExpr(std::move(call_expr), clone(dest)), {});
			right_expr->type = clone(dest);
		}

		return true;
	}

	throw ParseError("Unification failed: Cannot assign "s + str(src) + " to " + str(dest));
}


// Full unification algo
//------------------------------------------------
// See
// - https://eli.thegreenplace.net/2018/unification/
// - https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm

void unify(Type const &left, Type const &right, TypeEnv &subst, SemaContext &ctx, UnificationMode mode, Expr *right_expr)
{
	if(try_unify_type_deduction_variables(left, right, subst, ctx, mode, right_expr))
		return;

	if(try_unify_integer_types(left, right, subst, ctx, mode))
		return;

	if(try_unify_structs(left, right, subst, ctx, mode, right_expr))
		return;

	// Handle everything else
	left | match
	{
		[&](BuiltinType const &left_t)
		{
			// Integer types have already been handled above
			if(BuiltinType const *right_t = std::get_if<BuiltinType>(&right))
			{
				if(left_t == *right_t)
					return;
			}

			throw ParseError("Unification failed: Expected " + str(left) + ", got " + str(right));
		},
		[&](PointerType const &left_t)
		{
			switch(mode)
			{
				case UnificationMode::VALUE_ASSIGNMENT:
				case UnificationMode::POINTER_ASSIGNMENT:
				{
					if(PointerType const *right_t = std::get_if<PointerType>(&right))
					{
						bool mut_compatible = right_t->is_mutable() || left_t.is_const();
						if(not mut_compatible)
							throw ParseError("Unification failed: Pointer mutability is incompatible");

						unify(*left_t.target_type, *right_t->target_type, subst, ctx, UnificationMode::POINTER_ASSIGNMENT);
					}
					else
						throw ParseError("Unification failed: expected pointer type");
				} break;

				case UnificationMode::EQUAL:
				{
					if(PointerType const *right_t = std::get_if<PointerType>(&right))
					{
						if(left_t.mutability != right_t->mutability)
							throw ParseError("Unification failed: Pointer mutability is incompatible");

						unify(*left_t.target_type, *right_t->target_type, subst, ctx, UnificationMode::EQUAL);
					}
					else
						throw ParseError("Unification failed: expected pointer type");
				} break;

				case UnificationMode::COMMON_TYPE:
				{
					assert(!"unify: PointerType: COMMON_TYPE: TODO");
				} break;
			}
		},
		[&](ManyPointerType const &left_t)
		{
			switch(mode)
			{
				case UnificationMode::VALUE_ASSIGNMENT:
				case UnificationMode::POINTER_ASSIGNMENT:
				{
					if(ManyPointerType const *right_t = std::get_if<ManyPointerType>(&right))
					{
						bool mut_compatible = right_t->is_mutable() || left_t.is_const();
						if(not mut_compatible)
							throw ParseError("Unification failed: Pointer mutability is incompatible");

						unify(*left_t.element_type, *right_t->element_type, subst, ctx, UnificationMode::POINTER_ASSIGNMENT);
					}
					else
						throw ParseError("Unification failed: expected many pointer type");
				} break;

				case UnificationMode::EQUAL:
				{
					if(ManyPointerType const *right_t = std::get_if<ManyPointerType>(&right))
					{
						if(left_t.mutability != right_t->mutability)
							throw ParseError("Unification failed: Pointer mutability is incompatible");

						unify(*left_t.element_type, *right_t->element_type, subst, ctx, UnificationMode::EQUAL);
					}
					else
						throw ParseError("Unification failed: expected many pointer type");
				} break;

				case UnificationMode::COMMON_TYPE:
				{
					assert(!"unify: ManyPointerType: COMMON_TYPE: TODO");
				} break;
			}
		},

		[&](VarType const &left_t)
		{
			// Above we already handled the case where `left` is a TypeDeductionVar or a KnownIntTypeVar.
			// Thus, if we get here, we know that `left` is a TypeParameterVar.
			assert(is<TypeParameterVar>(left_t));

			if(not equiv(left, right))
				throw ParseError("Unification failed: Expected " + str(left) + ", got " + str(right));

			return;
		},

		[&](ProcType const&) { assert(!"unify: ProcType: TODO"); },
		[&](UnappliedProcType const&) { assert(!"unify: UnappliedProcType: TODO"); },

		[&](UnionType const &left_t) 
		{
			if(equiv(left, right))
				return;

			assert(mode == UnificationMode::VALUE_ASSIGNMENT);

			vector<Type> const &alts = left_t.canonical_def->alternatives;
			optional<size_t> match_idx;
			TypeEnv alt_subst;
			for(size_t i = 0; i < alts.size(); ++i)
			{
				Type const &alt = alts[i];
				pair<XInt64, XInt64> const *right_known_int = ctx.try_get_known_int(right);
				if(is_integer_type(alt) and right_known_int)
				{
					BuiltinType alt_int_type = std::get<BuiltinType>(alt);
					if(integer_assignable_to(alt_int_type, right_known_int->first) and integer_assignable_to(alt_int_type, right_known_int->second))
					{
						if(match_idx)
							throw ParseError("Assignment of integer value to union type is ambiguous");

						subst.add_unique(std::get<VarType>(right), clone(alt));
						match_idx = i;
					}
					else
						throw ParseError("Assignment to union failed: no alternative found for type " + str(right));
				}
				else
				{
					TypeEnv alt_subst_tmp;
					if(is_type_assignable(alt, right, ctx, alt_subst_tmp, right_expr))
					{
						if(match_idx)
							throw ParseError("Assignment to union type is ambiguous");

						match_idx = i;
						alt_subst = std::move(alt_subst_tmp);
					}
				}
			}

			if(not match_idx)
				throw ParseError("Assignment to union failed: no alternative found for type " + str(right));

			subst.take(std::move(alt_subst));
			if(right_expr)
			{
				*right_expr = Expr(UnionInitExpr(*match_idx, std::move(*right_expr)), {});
				right_expr->type = clone(left);
			}
		},

		// Already handled above
		[&](StructType const&) { UNREACHABLE; },

		[&](UnresolvedPath const&) { assert(!"unify: UnresolvedPath"); },
	};
}


//--------------------------------------------------------------------
// Type checking
//--------------------------------------------------------------------
optional<size_t> find_by_name(vector<Parameter> const &params, string const &name)
{
	for(size_t i = 0; i < params.size(); ++i)
	{
		if(params[i].name == name)
			return i;
	}

	return nullopt;
}

optional<IsMutable> is_lvalue_expr(Expr const &expr)
{
	return expr | match
	{
		[](VarExpr const &e) -> optional<IsMutable>
		{
			return e.var->mutability;
		},
		[](DerefExpr const &e) -> optional<IsMutable>
		{
			return std::get<PointerType>(*e.ptr_expr->type).mutability;
		},
		[](IndexExpr const &e) -> optional<IsMutable>
		{
			return std::get<ManyPointerType>(*e.ptr_expr->type).mutability;
		},
		[](MemberAccessExpr const &e) -> optional<IsMutable>
		{
			return is_lvalue_expr(*e.object);
		},

		[](auto const&) -> optional<IsMutable> { return nullopt; },
	};
}

bool is_cast_ok(Type const &dest_type, Type const &src_type, SemaContext &ctx)
{
	if(equiv(dest_type, src_type))
		return true;

	return src_type | match
	{
		[&](BuiltinType const &src_t)
		{
			if(BuiltinType const *dest_t = std::get_if<BuiltinType>(&dest_type))
				return is_builtin_int_type(src_t) && is_builtin_int_type(*dest_t);

			return false;
		},
		[&](VarType const&)
		{
			return false;
		},
		[&](PointerType const &src_ptr_type)
		{
			if(PointerType const *dest_ptr_type = std::get_if<PointerType>(&dest_type))
				return src_ptr_type.is_mutable() || dest_ptr_type->is_const();

			if(ManyPointerType const *dest_ptr_type = std::get_if<ManyPointerType>(&dest_type))
				return src_ptr_type.is_mutable() || dest_ptr_type->is_const();

			return false;
		},
		[&](ManyPointerType const &src_ptr_type)
		{
			if(PointerType const *dest_ptr_type = std::get_if<PointerType>(&dest_type))
				return src_ptr_type.is_mutable() || dest_ptr_type->is_const();

			if(ManyPointerType const *dest_ptr_type = std::get_if<ManyPointerType>(&dest_type))
				return src_ptr_type.is_mutable() || dest_ptr_type->is_const();

			return false;
		},
		[&](StructType const&)
		{
			return is_type_assignable(dest_type, src_type, ctx);
		},
		[&](ProcType const&) -> bool { assert(!"is_cast_ok: ProcType: TODO"); },
		[&](UnionType const&) -> bool { assert(!"is_cast_ok: UnionType: TODO"); },
		[&](UnappliedProcType const&) -> bool { assert(!"is_cast_ok: UnappliedProcType"); },
		[&](UnresolvedPath const&) -> bool { assert(!"is_cast_ok: UnresolvedPath"); },
	};
}

bool is_integer_type(Type const &type)
{
	return type | match
	{
		[&](BuiltinType const &t) { return is_builtin_int_type(t); },
		[&](VarType const &t) { return is<KnownIntTypeVar>(t); },
		[&](auto const&) { return false; },
	};
}

Parameter* find_var_member(StructType const *struct_, string const &field)
{
	assert(struct_->is_instantiated());
	for(InstanceMember &m: struct_->inst()->members)
	{
		Parameter *param = std::get_if<Parameter>(&m);
		if(param && param->name == field)
			return param;
	}

	if(struct_->parent())
		return find_var_member(struct_->parent(), field);

	return nullptr;
}

void add_size_to_alloc_call(Expr &addr, Expr &&size_expr);

ProcTypeDef get_callee_proc_type(Type const &callee_type)
{
	return callee_type | match
	{
		[&](ProcType const &t)
		{
			return clone(*t.def);
		},
		[&](UnappliedProcType const &t) -> ProcTypeDef
		{
			TypeEnv env;
			for(size_t i = 0; i < t.type_args.size(); ++i)
				env.add_unique(TypeParameterVar(t.type->type_params[i]), clone(t.type_args.args[i]));

			vector<Parameter> callee_params = clone(t.type->params);
			for(Parameter &param: callee_params)
				type_substitute(param.type, env);

			Type ret = clone(t.type->ret);
			type_substitute(ret, env);

			return ProcTypeDef{
				.type_params = {},
				.params = std::move(callee_params),
				.ret = std::move(ret),
			};
		},
		[&](auto const&) -> ProcTypeDef
		{
			throw ParseError("Type is not callable: " + str(callee_type));
		}
	};
}


void typecheck_pattern(
	Pattern &lhs_pattern,
	Type const &rhs_type,
	optional<IsMutable> rhs_is_lvalue,
	SemaContext &ctx,
	TypeEnv &subst,
	bool infallible_pattern_required,
	Expr *rhs_expr
);



void typecheck(Stmt &stmt, SemaContext &ctx);

bool typecheck_partial(Expr &expr, SemaContext &ctx, TypingHint hint)
{
	bool fully_instantiated = expr | match
	{
		[&](IntLiteralExpr &e)
		{
			if(hint.type)
			{
				if(BuiltinType const *bt = std::get_if<BuiltinType>(hint.type))
				{
					if(integer_assignable_to(*bt, e.value))
					{
						expr.type = *bt;
						return true;
					}
				}
			}

			expr.type = ctx.new_int_type_var(e.value);
			return false;
		},
		[&](BoolLiteralExpr&)
		{
			expr.type = BuiltinType::BOOL;
			return true;
		},
		[&](StringLiteralExpr &e)
		{
			switch(e.type)
			{
				case StringLiteralType::C:
				{
					ItemDef *c_char_item = ctx.mod().global()->lookup_item("c_char");
					StructDef &c_char = std::get<StructDef>(*c_char_item);
					StructType c_char_type(&c_char, {}, nullptr);
					c_char_type.instantiate({}, ctx);
					expr.type = ManyPointerType(std::move(c_char_type), IsMutable::NO);
				} break;
			}

			return true;
		},
		[&](UnresolvedPath&) -> bool { assert(!"typecheck_partial: UnresolvedPath"); },
		[&](UnappliedProcExpr &e)
		{
			expr.type = UnappliedProcType(clone(e.proc->type), clone(e.type_args));
			return false;
		},
		[&](ConstructorExpr &e)
		{
			StructType const *struct_type = std::get_if<StructType>(&e.ctor);
			if(not struct_type)
				throw ParseError("Only structs can be used as constructor");

			assert(struct_type->def()->constructor_params);

			TypeEnv env;
			create_type_env(*struct_type, env);
			vector<Parameter> callee_params = clone(*struct_type->def()->constructor_params);
			for(Parameter &param: callee_params)
				type_substitute(param.type, env);

			ProcTypeDef ctor_type{
				.type_params = {},
				.params = std::move(callee_params),
				.ret = clone(e.ctor),
			};

			expr.type = UnappliedProcType(std::move(ctor_type), {});
			return false;
		},
		[&](VarExpr &e)
		{
			assert(e.var->type && "typecheck_partial(VarExpr): VarDef::type is null");
			expr.type = clone(*e.var->type);

			return true;
		},
		[&](ProcExpr &e)
		{
			expr.type = ProcType(&e.inst->type, ctx.mod().get_canonical(e.inst->type));
			return true;
		},
		[&](UnaryExpr &e)
		{
			switch(e.op)
			{
				case UnaryOp::NOT:
				{
					typecheck(*e.sub, ctx);
					if(not equiv(*e.sub->type, BuiltinType::BOOL))
						throw ParseError("NOT expected type bool");

					expr.type = clone(*e.sub->type);
					return true;
				} break;

				case UnaryOp::NEG:
				{
					bool sub_fully_instantiated = typecheck_partial(*e.sub, ctx, TypingHint(hint.subst));

					if(pair<XInt64, XInt64> const *known_int = ctx.try_get_known_int(*e.sub->type))
						expr.type = ctx.new_int_type_var(-known_int->second, -known_int->first);
					else if(is_integer_type(*e.sub->type))
						expr.type = clone(*e.sub->type);
					else
						throw ParseError("Negation requires integer type");

					return sub_fully_instantiated;
				} break;

				default: UNREACHABLE;
			}
		},
		[&](BinaryExpr &e)
		{
			switch(e.op)
			{
				case BinaryOp::ADD:
				case BinaryOp::SUB:
				case BinaryOp::MUL:
				case BinaryOp::DIV:
				{
					bool left_fully_instantiated = typecheck_partial(*e.left, ctx, TypingHint(hint.subst));
					bool right_fully_instantiated = typecheck_partial(*e.right, ctx, TypingHint(hint.subst));

					unify(*e.left->type, *e.right->type, *hint.subst, ctx, UnificationMode::COMMON_TYPE);

					pair<XInt64, XInt64> const *left_known_int = ctx.try_get_known_int(*e.left->type);
					pair<XInt64, XInt64> const *right_known_int = ctx.try_get_known_int(*e.right->type);
					if(left_known_int && right_known_int)
					{
						// TODO Implement interval arithmetic
						assert(left_known_int->first == left_known_int->second);
						assert(right_known_int->first == right_known_int->second);

						XInt64 result;
						switch(e.op)
						{
							case BinaryOp::ADD: result = left_known_int->first + right_known_int->first; break;
							case BinaryOp::SUB: result = left_known_int->first - right_known_int->first; break;
							case BinaryOp::MUL: assert(!"typecheck_partial: BinaryExpr: KnownIntType: MUL: TODO"); break;
							case BinaryOp::DIV: assert(!"typecheck_partial: BinaryExpr: KnownIntType: DIV: TODO"); break;
							default: UNREACHABLE;
						}

						expr.type = ctx.new_int_type_var(result);
					}
					else if(is_integer_type(*e.left->type) && is_integer_type(*e.right->type))
					{
						optional<Type> result_type = common_int_type(*e.left->type, *e.right->type, ctx);
						if(not result_type)
							throw ParseError("Binary op: expected same integral types, got " + str(*e.left->type) + " and " + str(*e.right->type));

						expr.type = std::move(*result_type);
					}
					else
						throw ParseError("Binary operation requires integer types");

					return left_fully_instantiated && right_fully_instantiated;
				} break;

				case BinaryOp::EQ:
				{
					typecheck(*e.left, ctx);
					typecheck(*e.right, ctx);

					if(not equiv(*e.left->type, *e.right->type))
						throw ParseError("Equality: expected equivalent types, got " + str(*e.left->type) + " and " + str(*e.right->type));

					expr.type = BuiltinType::BOOL;

					return true;
				} break;

				case BinaryOp::LT:
				case BinaryOp::LE:
				case BinaryOp::GT:
				case BinaryOp::GE:
				{
					TypeEnv subst;
					bool left_fully_instantiated = typecheck_partial(*e.left, ctx, TypingHint(&subst));
					bool right_fully_instantiated = typecheck_partial(*e.right, ctx, TypingHint(&subst));

					unify(*e.left->type, *e.right->type, subst, ctx, UnificationMode::COMMON_TYPE);

					if(not left_fully_instantiated) instantiate_types(*e.left, subst, ctx);
					if(not right_fully_instantiated) instantiate_types(*e.right, subst, ctx);

					if(not is_integer_type(*e.left->type) || not equiv(*e.left->type, *e.right->type))
						throw ParseError("Comparison: expected equivalent integral types, got " + str(*e.left->type) + " and " + str(*e.right->type));

					expr.type = BuiltinType::BOOL;

					return true;
				} break;

				default: UNREACHABLE;
			}
		},
		[&](AddressOfExpr &e)
		{
			typecheck(*e.object_expr, ctx);
			if(optional<IsMutable> mutability = is_lvalue_expr(*e.object_expr))
			{
				if(*mutability == IsMutable::YES || e.mutability == IsMutable::NO)
				{
					expr.type = PointerType(clone(*e.object_expr->type), e.mutability);
					return true;
				}

				throw ParseError("Cannot make mutable reference to const object");
			}

			throw ParseError("Can only take address of lvalue expression");
		},
		[&](DerefExpr &e)
		{
			typecheck(*e.ptr_expr, ctx);
			if(PointerType const *ptr_type = std::get_if<PointerType>(&*e.ptr_expr->type))
			{
				expr.type = clone(*ptr_type->target_type);
				return true;
			}

			throw ParseError("Pointer dereference expr requires pointer type");
		},
		[&](IndexExpr &e)
		{
			typecheck(*e.ptr_expr, ctx);
			typecheck(*e.idx_expr, ctx);
			if(ManyPointerType const *ptr_type = std::get_if<ManyPointerType>(&*e.ptr_expr->type))
			{
				if(is_integer_type(*e.idx_expr->type))
				{
					expr.type = clone(*ptr_type->element_type);
					return true;
				}

				throw ParseError("Index expr must be of integral type");
			}

			throw ParseError("Indexing expr requires many pointer type");
		},
		[&](MemberAccessExpr &e)
		{
			typecheck(*e.object, ctx);
			if(StructType const *struct_type = std::get_if<StructType>(&*e.object->type))
			{
				if(Parameter *param = find_var_member(struct_type, e.member))
				{
					expr.type = clone(param->type);
					return true;
				}

				throw ParseError("Member access: unknown field: " + e.member);
			}

			throw ParseError("Member access requires object of struct type");
		},
		[&](AssignmentExpr &e)
		{
			bool lhs_fully_instantiated = typecheck_partial(*e.lhs, ctx, TypingHint(hint.subst));
			bool rhs_fully_instantiated = typecheck_partial(*e.rhs, ctx, hint.with_type(&*e.lhs->type));
			unify(*e.lhs->type, *e.rhs->type, *hint.subst, ctx, UnificationMode::VALUE_ASSIGNMENT, e.rhs.get());

			if(is_lvalue_expr(*e.lhs) == IsMutable::YES)
				expr.type = clone(*e.lhs->type);
			else
				throw ParseError("LHS does not denote a mutable lvalue in assignment");

			return lhs_fully_instantiated and rhs_fully_instantiated;
		},
		[&](AsExpr &e)
		{
			typecheck(*e.src_expr, ctx);
			instantiate_type(e.target_type, *hint.subst, ctx);

			if(!is_cast_ok(e.target_type, *e.src_expr->type, ctx))
				throw ParseError("Invalid cast");

			expr.type = clone(e.target_type);

			return true;
		},
		[&](CallExpr &e)
		{
			bool fully_instantiated = typecheck_partial(*e.callable, ctx, TypingHint(hint.subst));

			ProcTypeDef callee_type = get_callee_proc_type(*e.callable->type);
			if(e.args.size() > callee_type.params.size())
				throw ParseError("Too many arguments");

			if(hint.type)
				unify(*hint.type, callee_type.ret, *hint.subst, ctx, UnificationMode::VALUE_ASSIGNMENT);

			unordered_set<size_t> assigned_params;
			bool has_ooo_named_args = false;
			for(size_t i = 0; i < e.args.size(); ++i)
			{
				Argument &arg = e.args[i];

				// Find the corresponding parameter depending on whether the argument is named or not
				if(arg.name)
				{
					if(optional<size_t> param_idx_opt = find_by_name(callee_type.params, *arg.name))
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

				Parameter const &param = callee_type.params.at(*arg.param_idx);
				bool arg_fully_instantiated = typecheck_partial(*arg.expr, ctx, hint.with_type(&param.type));
				if(not arg_fully_instantiated) fully_instantiated = false;

				try {
					unify(param.type, *arg.expr->type, *hint.subst, ctx, UnificationMode::VALUE_ASSIGNMENT, arg.expr.get());
				} catch(ParseError const &e) {
					throw ParseError("Procedure/struct argument has invalid type: "s + e.what());
				}
			}

			for(size_t param_idx = 0; param_idx < callee_type.params.size(); ++param_idx)
			{
				if(!assigned_params.contains(param_idx))
				{
					if(!callee_type.params.at(param_idx).default_value)
						throw ParseError("Missing value for procedure/struct argument in call");
				}
			}

			expr.type = clone(callee_type.ret);
			type_substitute(*expr.type, *hint.subst);

			return fully_instantiated;
		},
		[&](SizeOfExpr&)
		{
			expr.type = BuiltinType::USIZE;
			return true;
		},
		[&](MakeExpr &e)
		{
			typecheck(*e.init, ctx);

			add_size_to_alloc_call(*e.addr, Expr(SizeOfExpr(clone(*e.init->type)), {}));
			typecheck(*e.addr, ctx);

			expr.type = PointerType(clone(*e.init->type), IsMutable::YES);
			return true;
		},
		[&](UnionInitExpr &e)
		{
			return typecheck_partial(*e.value, ctx, hint);
		},
		[&](TryExpr &e)
		{
			bool fully_instantiated = typecheck_partial(*e.subject, ctx, hint);

			StructType const *st = std::get_if<StructType>(&*e.subject->type);
			if(not st)
				throw ParseError("Try expressions only work on Option or Result");

			if(st->name() == "Option")
				expr.type = clone(st->type_args().args[0]);
			else if(st->name() == "Result")
				expr.type = clone(st->type_args().args[0]);
			else
				throw ParseError("Try expressions only work on Option or Result");


			if(st->name() == "Option")
			{
				VarDef *some_match_var = ctx.proc()->scope->declare_temp_var();
				VarDef *result_var = ctx.proc()->scope->declare_temp_var(IsMutable::YES);

				// Construct success arm
				MatchArm success_arm(
					pattern_Ctor(
						mk_path({"Option"}, {"Some"}, ctx.mod().global()),
						mk_vec(pattern_Var(some_match_var))
					),
					stmt_Assignment(expr_Var(result_var), expr_Var(some_match_var))
				);

				// Construct error arm
				Pattern error_capture = e.error_capture ? clone(*e.error_capture) : Pattern(WildcardPattern(), nullopt);
				Stmt on_failure =
					e.on_failure ?
						clone(*e.on_failure)
						:
						stmt_Ret(
							expr_Call(expr_Ctor(mk_path({"Option"}, {"None"}, ctx.mod().global())), {})
						);
				MatchArm error_arm(clone(error_capture), std::move(on_failure));

				// Construct final match statement
				e.match_stmt = std::make_unique<Stmt>(
					stmt_Block(
						mk_vec(
							stmt_Let(pattern_Var(result_var, clone(*expr.type))),
							stmt_Match(
								clone(*e.subject),
								mk_vec(
									std::move(success_arm),
									std::move(error_arm))))));

				resolve_identifiers(*e.match_stmt, ctx);
				typecheck(*e.match_stmt, ctx);

				e.result_expr = std::make_unique<Expr>(Expr(VarExpr(result_var), {}));
				resolve_identifiers(*e.result_expr, ctx);
				typecheck(*e.result_expr, ctx);
			}
			else if(st->name() == "Result")
			{
				VarDef *ok_match_var = ctx.proc()->scope->declare_temp_var();
				VarDef *err_match_var = ctx.proc()->scope->declare_temp_var();
				VarDef *result_var = ctx.proc()->scope->declare_temp_var(IsMutable::YES);

				// Construct success arm
				MatchArm success_arm(
					pattern_Ctor(
						mk_path({"Result"}, {"Ok"}, ctx.mod().global()),
						mk_vec(pattern_Var(ok_match_var))
					),
					stmt_Assignment(expr_Var(result_var), expr_Var(ok_match_var))
				);

				// Construct error arm
				Pattern error_capture =
					e.error_capture ?
						clone(*e.error_capture)
						:
						pattern_Ctor(
							mk_path({"Result"}, {"Err"}, ctx.mod().global()),
							mk_vec(pattern_Var(err_match_var))
						);

				Stmt on_failure =
					e.on_failure ?
						clone(*e.on_failure)
						:
						stmt_Ret(
							expr_Call(
								expr_Ctor(mk_path({"Result"}, {"Err"}, ctx.mod().global())),
								mk_vec(expr_Var(err_match_var))
							)
						);

				MatchArm error_arm(clone(error_capture), std::move(on_failure));

				// Construct final match statement
				e.match_stmt = std::make_unique<Stmt>(
					stmt_Block(
						mk_vec(
							stmt_Let(pattern_Var(result_var, clone(*expr.type))),
							stmt_Match(
								clone(*e.subject),
								mk_vec(
									std::move(success_arm),
									std::move(error_arm))))));

				resolve_identifiers(*e.match_stmt, ctx);
				typecheck(*e.match_stmt, ctx);

				e.result_expr = std::make_unique<Expr>(Expr(VarExpr(result_var), {}));
				resolve_identifiers(*e.result_expr, ctx);
				typecheck(*e.result_expr, ctx);
			}

			return fully_instantiated;
		},
	};

	return fully_instantiated;
}

void typecheck(Expr &expr, SemaContext &ctx, Type const *type_hint)
{
	TypeEnv subst;
	if(not typecheck_partial(expr, ctx, TypingHint(type_hint, &subst)))
		instantiate_types(expr, subst, ctx);
}


void add_size_to_alloc_call(Expr &addr, Expr &&size_expr)
{
	vector<Argument> *args = nullptr;
	vector<Parameter> const *params = nullptr;

	// Try to extract the arguments and parameters of the function call
	if(CallExpr *addr_call = std::get_if<CallExpr>(&addr))
	{
		args = &addr_call->args;
		if(ProcExpr *proc_expr = std::get_if<ProcExpr>(addr_call->callable.get()))
			params = &proc_expr->inst->type.params;
		else if(UnappliedProcExpr *proc_expr = std::get_if<UnappliedProcExpr>(addr_call->callable.get()))
			params = &proc_expr->proc->type.params;
	}

	// Add `size_expr` as an argument
	if(args)
	{
		if(
			args->size() == 0 &&
			params->size() == 1 &&
			equiv(params->at(0).type, Type(BuiltinType::USIZE))
		)
		{
			args->push_back(Argument{
				.expr = std::make_unique<Expr>(std::move(size_expr)),
				.name = nullopt,
				.param_idx = 0,
			});
		}
	}
}

void typecheck_pattern(
	Pattern &lhs_pattern,
	Type const &rhs_type,
	optional<IsMutable> rhs_is_lvalue,
	SemaContext &ctx,
	TypeEnv &subst,
	bool infallible_pattern_required,
	Expr *rhs_expr
)
{
	Type const *expected_type = &rhs_type;
	if(lhs_pattern.provided_type)
	{
		try {
			if(infallible_pattern_required)
				unify(*lhs_pattern.provided_type, rhs_type, subst, ctx, UnificationMode::VALUE_ASSIGNMENT, rhs_expr);
			else
				unify(rhs_type, *lhs_pattern.provided_type, subst, ctx, UnificationMode::VALUE_ASSIGNMENT, rhs_expr);
		} catch (ParseError const &e) {
			throw ParseError("Inferred type does not match specified type in let statement: "s + e.what());
		}
		instantiate_type(*lhs_pattern.provided_type, subst, ctx);
		expected_type = &*lhs_pattern.provided_type;
	}


	lhs_pattern | match
	{
		[&](VarPattern &p)
		{
			p.var->type = clone(*expected_type);
			lhs_pattern.type = clone(*expected_type);
		},
		[&](DerefPattern &p)
		{
			if(PointerType const *pointer = std::get_if<PointerType>(expected_type))
			{
				typecheck_pattern(*p.sub, *pointer->target_type, rhs_is_lvalue, ctx, subst, infallible_pattern_required, nullptr);
				lhs_pattern.type = clone(*p.sub->type);
			}
			else
				throw ParseError("Invalid pattern op: target type not a pointer");
		},
		[&](AddressOfPattern &p)
		{
			if(rhs_is_lvalue)
			{
				if(*rhs_is_lvalue == IsMutable::YES || p.mutability == IsMutable::NO)
				{
					Type pt(PointerType(clone(*expected_type), p.mutability));
					typecheck_pattern(*p.sub, pt, rhs_is_lvalue, ctx, subst, infallible_pattern_required, nullptr);
					lhs_pattern.type = clone(*p.sub->type);
				}
				else
					throw ParseError("Cannot make mutable reference to const object");
			}
			else
				throw ParseError("Can only take address of lvalue expression");
		},
		[&](ConstructorPattern &p)
		{
			unify(*expected_type, p.ctor, subst, ctx, UnificationMode::VALUE_ASSIGNMENT, nullptr);
			instantiate_type(p.ctor, subst, ctx);

			// Validate constructor
			StructType const *ctor = std::get_if<StructType>(&p.ctor);
			if(ctor)
			{
				if(p.has_parens and (ctor->def()->ctor_without_parens or not ctor->def()->constructor_params))
					throw ParseError("Invalid parentheses after type in pattern");

				// If no parens are given then we only match on the type
				if(p.has_parens and ctor->def()->constructor_params)
				{
					if(p.args.size() > ctor->def()->constructor_params->size())
						throw ParseError("Too many arguments provided for constructor in pattern");

					if(p.args.size() < ctor->def()->constructor_params->size())
						throw ParseError("TODO: Support default arguments in patterns");

					for(size_t i = 0; i < p.args.size(); ++i)
					{
						PatternArgument &arg = p.args[i];

						// TODO Calling type_substitute() is not necessary if `ctor->is_instantiated()`
						Type param_type = clone(ctor->def()->constructor_params->at(i).type);
						type_substitute(param_type, create_type_env(*ctor));

						if(arg.param_name)
							throw ParseError("TODO: Support named arguments in constructor pattern");

						typecheck_pattern(*arg.pattern, param_type, rhs_is_lvalue, ctx, subst, infallible_pattern_required, nullptr);
						arg.param_idx = i;
					}
				}
			}

			// Check whether the constructor can actually match against the RHS
			if(StructType const *rhs_struct_type = std::get_if<StructType>(expected_type))
			{
				if(not ctor)
					throw ParseError("Constructor in pattern must refer to a struct type");

				if(not ctor->def()->constructor_params)
					throw ParseError("Struct in pattern does not have a constructor");


				if(equiv_struct(*ctor, *rhs_struct_type, subst, ctx)) {
					// Always succeeds
				}
				else if(ctor->parent() && equiv_struct(*ctor->parent(), *rhs_struct_type, subst, ctx))
				{
					if(infallible_pattern_required && rhs_struct_type->def()->case_members().size() > 1)
						throw ParseError("Constructor pattern must be infallible but is not");
				}
				else
					throw ParseError("Constructor in pattern must match against a compatible struct type");
			}
			else if(UnionType const *rhs_union_type = std::get_if<UnionType>(expected_type))
			{
				UnionTypeDef const *union_ = rhs_union_type->canonical_def;
				if(not union_->contains(p.ctor))
					throw ParseError("Constructor in pattern must match against a member of the union type");
			}
			else
				throw ParseError("Constructor in pattern must match against a compatible struct or union type");

			lhs_pattern.type = clone(p.ctor);
		},
		[&](WildcardPattern&)
		{
			lhs_pattern.type = clone(*expected_type);
		},
	};
}


Type const* try_get_explicit_type(Pattern &pattern)
{
	return pattern | match
	{
		[&](VarPattern const &p) -> Type const*
		{
			if(p.var->type)
				return &*p.var->type;

			return nullptr;
		},
		[&](auto const&) -> Type const*
		{
			if(pattern.type)
				return &*pattern.type;

			return nullptr;
		}
	};
}


void typecheck(Stmt &stmt, SemaContext &ctx)
{
	stmt | match
	{
		[&](LetStmt &s)
		{
			if(s.init_expr)
			{
				TypeEnv subst;
				bool fully_instantiated = typecheck_partial(*s.init_expr, ctx, TypingHint(try_get_explicit_type(s.lhs), &subst));
				typecheck_pattern(s.lhs, *s.init_expr->type, is_lvalue_expr(*s.init_expr), ctx, subst, true, &*s.init_expr);

				if(not fully_instantiated)
				{
					instantiate_types(*s.init_expr, subst, ctx);
					instantiate_types(s.lhs, subst, ctx);
				}
			}
			else
			{
				VarPattern *var_pat = std::get_if<VarPattern>(&s.lhs);
				if(not var_pat)
					throw ParseError("Expected variable name in let-statement");

				if(not s.lhs.provided_type)
					throw ParseError("Expected explicit type in let-statement");

				var_pat->var->type = clone(*s.lhs.provided_type);
				s.lhs.type = clone(*s.lhs.provided_type);
			}
		},
		[&](ExprStmt &s)
		{
			typecheck(s.expr, ctx);
		},
		[&](BlockStmt &s)
		{
			for(OwnPtr<Stmt> &stmt: s.stmts)
				typecheck(*stmt, ctx);
		},
		[&](ReturnStmt &s)
		{
			if(s.ret_expr)
			{
				TypeEnv subst;
				bool fully_instantiated = typecheck_partial(*s.ret_expr, ctx, TypingHint(&ctx.proc()->type.ret, &subst));
				try {
					unify(ctx.proc()->type.ret, *s.ret_expr->type, subst, ctx, UnificationMode::VALUE_ASSIGNMENT, &*s.ret_expr);
					if(not fully_instantiated)
						instantiate_types(*s.ret_expr, subst, ctx);
				}
				catch(ParseError const &e) {
					throw ParseError("Invalid return expression: "s + e.what());
				}
			}
			else
			{
				if(!equiv(ctx.proc()->type.ret, BuiltinType::UNIT))
					throw ParseError("Return statement must return unit value");
			}
		},
		[&](IfStmt &s)
		{
			typecheck(s.condition, ctx);
			if(!equiv(*s.condition.type, BuiltinType::BOOL))
				throw ParseError("If condition must be of type bool");

			typecheck(*s.then, ctx);
			if(s.else_) typecheck(*s.else_, ctx);
		},
		[&](WhileStmt &s)
		{
			typecheck(s.condition, ctx);
			if(!equiv(*s.condition.type, BuiltinType::BOOL))
				throw ParseError("Loop condition must be of type bool");

			typecheck(*s.body, ctx);
		},
		[&](MatchStmt &s)
		{
			typecheck(s.expr, ctx);
			*s.expr.type | match
			{
				[&](StructType const &t)
				{
					assert(t.is_instantiated());

					StructDefInstance *subject = t.inst();
					unordered_set<StructDefInstance const*> matched_cases;
					bool has_wildcard = false;
					for(MatchArm &arm: s.arms)
					{
						if(has_wildcard)
							throw ParseError("Pattern is following wildcard pattern and is therefore unreachable");

						TypeEnv subst;
						typecheck_pattern(arm.capture, *s.expr.type, is_lvalue_expr(s.expr), ctx, subst, false, nullptr);
						instantiate_type(*arm.capture.type, subst, ctx);

						if(is<WildcardPattern>(arm.capture))
							has_wildcard = true;
						else
						{
							StructDefInstance const *arm_inst = std::get<StructType>(*arm.capture.type).inst();
							assert(arm_inst->outer_struct->inst() == subject);
							if(!matched_cases.insert(arm_inst).second)
								throw ParseError("Duplicate case value");

							arm.discr = arm_inst->def->get_case_idx();
						}

						typecheck(*arm.stmt, ctx);
					}

					if(matched_cases.size() != subject->def->num_cases and not has_wildcard)
						throw ParseError("Match is not exhaustive");
				},
				[&](UnionType const &t)
				{
					unordered_set<Type, std::hash<Type>, TypeEquiv> matched_cases;
					bool has_wildcard = false;
					for(MatchArm &arm: s.arms)
					{
						if(has_wildcard)
							throw ParseError("Pattern is following wildcard pattern and is therefore unreachable");

						TypeEnv subst;
						typecheck_pattern(arm.capture, *s.expr.type, is_lvalue_expr(s.expr), ctx, subst, false, nullptr);
						instantiate_type(*arm.capture.type, subst, ctx);

						if(is<WildcardPattern>(arm.capture))
							has_wildcard = true;
						else
						{
							if(!matched_cases.insert(clone(*arm.capture.type)).second)
								throw ParseError("Duplicate case value");

							arm.discr = *t.canonical_def->contains(*arm.capture.type);
						}

						typecheck(*arm.stmt, ctx);
					}

					if(matched_cases.size() != t.canonical_def->alternatives.size() and not has_wildcard)
						throw ParseError("Match is not exhaustive");
				},
				[&](auto const&)
				{
					throw ParseError("Match statements only work on structs and union types, got " + str(*s.expr.type));
				},
			};
		},
	};
}


void typecheck(TopLevelItem item, SemaContext &ctx)
{
	item | match
	{
		[&](ProcDef *def)
		{
			for(VarDef *param: def->param_vars)
				instantiate_type(*param->type, {}, ctx);

			// HACK: Why instantiate a copy of the return type and not the return type itself?
			//       - We need the return type to remain unapplied for type inference (see
			//         typecheck_partial() for CallExpr)
			//       - But we also need to instantiate any types that are only mentioned in the
			//         return type (if a function return `?^FILE` we need to instantiate the Option
			//         type appropriately)
			Type ret_type = clone(def->type.ret);
			instantiate_type(ret_type, {}, ctx);

			for(Parameter &param: def->type.params)
			{
				if(!param.default_value)
					continue;

				TypeEnv subst;
				bool fully_instantiated = typecheck_partial(*param.default_value, ctx, TypingHint(&subst));
				try {
					unify(param.type, *param.default_value->type, subst, ctx, UnificationMode::VALUE_ASSIGNMENT, &*param.default_value);
				}
				catch(ParseError const &e) {
					throw ParseError("Parameter default value has incorrect type: "s + e.what());
				}

				if(not fully_instantiated)
					instantiate_types(*param.default_value, subst, ctx);
			}

			if(def->body)
			{
				ctx.set_current_proc(def);
				typecheck(*def->body, ctx);
				ctx.set_current_proc(nullptr);
			}
		},
		[&](StructDef *def)
		{
			enum MemberListState
			{
				INITIAL_VAR_MEMBERS,
				CASE_MEMBERS,
				TRAILING_VAR_MEMBERS,
			};

			if(def->ctor_without_parens and def->constructor_params and def->constructor_params->size())
				throw ParseError("Struct must have braces because it inherits members from parent");

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
							typecheck(*var_member.default_value, ctx, &var_member.type);
							if(!is_type_assignable(var_member.type, *var_member.default_value->type, ctx))
								throw ParseError(
									"Default value for struct member `"s + var_member.name + "` has incorrect type. "
									"Expected " + str(var_member.type) + ", got " + str(*var_member.default_value->type)
								);
						}
					},
					[&](StructDef *case_member)
					{
						if(state == INITIAL_VAR_MEMBERS)
							state =  CASE_MEMBERS;
						else if(state == TRAILING_VAR_MEMBERS)
							throw ParseError("Variable members must come before or after all case members");

						typecheck(case_member, ctx);
					},
				};
			}
		},
		[&](AliasDef*) {},
	};
}


void typecheck(SemaContext &ctx)
{
	for(TopLevelItem item: ctx.mod().items())
		typecheck(item, ctx);
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
		*this << "#include <stdlib.h>" << LineEnd;
		*this << "#include <stdio.h>" << LineEnd;
		*this << "#include <limits.h>" << LineEnd;
		*this << "#include <assert.h>" << LineEnd;
		*this << "#include <errno.h>" << LineEnd;
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

		*this << LineEnd;
		*this << "typedef struct Unit { char _; } Unit;" << LineEnd;
		*this << LineEnd;

		*this << LineEnd;
		*this << "int get_errno() { return errno; }" << LineEnd;
		*this << LineEnd;

		*this << LineEnd;
		*this << "__attribute__((noreturn)) void fatal(char const *msg) { fputs((char const*)msg, stderr); exit(1); }" << LineEnd;
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

	void set_current_proc(ProcDefInstance const *proc) { m_cur_proc = proc; }
	ProcDefInstance const* proc() const { return m_cur_proc; }

private:
	std::ostream &os;
	int indent_level = 0;
	bool insert_indent = true;
	int tmp_var_counter = 0;

	ProcDefInstance const *m_cur_proc = nullptr;
};


//--------------------------------------------------------------------
// Name mangling
//--------------------------------------------------------------------

// All mangled Myca symbols start with '_Y'.
//
// <symbol> ::= '_Y' <kind> <path-segment>+
//
// <kind> ::= 'T'  // type
//          | 'F'  // Procedure
//          | 'C'  // constructor
//
// <path-segment> ::= <id-segment> | <type-segment>
//
// <id-segment> ::= <num>                   // length of the following identifier
//                  <identifier>
//                  ('G' <path-segment>+ 'E')*  // Generic type arguments start with 'G' and ends with 'E'
//
// <type-segment> ::= 'P' 'm'? <path-segment>  // Pointer
//                  | 'M' 'm'? <path-segment>  // Many pointer
//                  | 'U' <path-segment>+ 'E'  // Union type

string mangle_type_segment(Type const &type);

string mangle_type_args(TypeArgList const &types)
{
	string mangled;
	for(Type const &arg: types)
		mangled += 'G' + mangle_type_segment(arg) + 'E';

	return mangled;
}

string mangle_union_type_segment(UnionTypeDef const &union_)
{
	string mangled = "U";
	for(Type const &alt: union_.alternatives)
		mangled += mangle_type_segment(alt);

	return mangled + "E";
}

string mangle_type_segment(Type const &type)
{
	return type | match
	{
		[&](BuiltinType const &t) -> string
		{
			switch(t)
			{
				case BuiltinType::NEVER: return "5Never";
				case BuiltinType::UNIT: return "4Unit";
				case BuiltinType::BOOL: return "4bool";
				case BuiltinType::I8: return "2i8";
				case BuiltinType::U8: return "2u8";
				case BuiltinType::I32: return "3i32";
				case BuiltinType::U32: return "3u32";
				case BuiltinType::ISIZE: return "5isize";
				case BuiltinType::USIZE: return "5usize";
			}

			UNREACHABLE;
		},
		[&](VarType const&) -> string { assert(!"mangle_type_segment: VarType"); },
		[&](PointerType const &t)
		{
			string mangled = "P";
			if(t.is_mutable())
				mangled += "m";

			return mangled + mangle_type_segment(*t.target_type);
		},
		[&](ManyPointerType const &t)
		{
			string mangled = "M";
			if(t.is_mutable())
				mangled += "m";

			return mangled + mangle_type_segment(*t.element_type);
		},
		[&](StructType const &struct_)
		{
			string segment = std::to_string(struct_.inst()->def->name.length()) + struct_.inst()->def->name;
			segment += mangle_type_args(struct_.inst()->type_args);

			if(struct_.inst()->outer_struct)
				return mangle_type_segment(StructType(struct_.inst()->outer_struct->inst())) + segment;

			return segment;
		},
		[&](ProcType const&) -> string { assert(!"mangle_type_segment: ProcType: TODO"); },
		[&](UnionType const &t) -> string
		{
			return mangle_union_type_segment(*t.canonical_def);
		},

		[&](UnappliedProcType const&) -> string { assert(!"mangle_type_segment: UnappliedProcType"); },
		[&](UnresolvedPath const&) -> string { assert(!"mangle_type_segment: UnresolvedPath"); },
	};
}


string mangle_type(Type const &type)
{
	return "_YT" + mangle_type_segment(type);
}

string mangle_union_type(UnionTypeDef const &union_)
{
	return "_YT" + mangle_union_type_segment(union_);
}

string mangle_constructor(Type const &type)
{
	return "_YC" + mangle_type_segment(type);
}

string mangle_procedure(ProcDefInstance const *proc)
{
	if(proc->def->name == "main" || not proc->def->body)
		return proc->def->name;

	string mangled = std::to_string(proc->def->name.length()) + proc->def->name;
	mangled += mangle_type_args(proc->type_args);

	return "_YF" + mangled;
}


//--------------------------------------------------------------------
// CStruct
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
	string constructor_name;
	MemoryLayout cur_layout{};
	vector<CMember> members;

	explicit CStruct(string const &name, string const &constructor_name) :
		name(name),
		constructor_name(constructor_name) {}

	size_t add(CMember &&m, Module &mod)
	{
		cur_layout.extend(compute_layout(m.type, mod));
		members.push_back(std::move(m));
		return members.size() - 1;
	}
};

void generate_c_struct_def(CStruct const &cstruct, CBackend &backend)
{
	// In Myca, types can have zero alignment (e.g., Never), but this is not allowed in C.
	size_t alignment = std::max(cstruct.cur_layout.alignment, size_t(1));
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

	if(cstruct.members.empty())
		backend << "char dummy;" << LineEnd;

	backend.decrease_indent();
	backend << "};" << LineEnd;
}


//--------------------------------------------------------------------
// Types
//--------------------------------------------------------------------
string generate_c_to_str(BuiltinType const &type)
{
	switch(type)
	{
		case BuiltinType::NEVER: return "Never";
		case BuiltinType::UNIT: return "Unit";
		case BuiltinType::BOOL: return "bool";
		case BuiltinType::I8: return "int8_t";
		case BuiltinType::U8: return "uint8_t";
		case BuiltinType::I32: return "int32_t";
		case BuiltinType::U32: return "uint32_t";
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
			if(t.inst()->def->name == "c_void")
				return "void"s;

			if(t.inst()->def->name == "c_char")
				return "char"s;

			if(Type const *pointee = is_optional_ptr(&t))
				return generate_c_to_str(*pointee);

			if(t.inst()->def->is_extern)
				return t.inst()->def->name;

			return "struct " + mangle_type(type);
		},
		[&](ProcType const&) -> string { assert(!"generate_c_to_str: ProcType: TODO"); },
		[&](UnionType const&) -> string { return "struct " + mangle_type(type); },

		[&](UnappliedProcType const&) -> string { assert(!"generate_c_to_str: UnappliedProcType"); },
		[&](UnresolvedPath const&) -> string { assert(!"generate_c_to_str: UnresolvedPath"); },
	};
}

void generate_c(Type const &type, CBackend &backend)
{
	backend << generate_c_to_str(type);
}


//--------------------------------------------------------------------
// Expressions
//--------------------------------------------------------------------
string generate_c(Expr const &expr, CBackend &backend, bool need_result = true);
void generate_c(Stmt const &stmt, CBackend &backend);
void generate_c_pattern(Pattern const &lhs_pattern, string const &rhs_expr, Type const &rhs_type, CBackend &backend);

string generate_c_cast(Type const &target_type, string const &expr, Type const &expr_type)
{
	if(equiv(target_type, expr_type) || expr == "NULL")
		return expr;

	string type_val = generate_c_to_str(target_type);

	if(is<StructType>(target_type) and not is_optional_ptr(target_type))
		return "(*(" + type_val + "*)&(" + expr + "))";
	else
		return "((" + type_val + ")(" + expr + "))";
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
					return "(" + generate_c_to_str(*expr.type) + ")\""s + e.value + '"';
			}

			UNREACHABLE;
		},
		[&](UnresolvedPath const&) -> string { assert(!"generate_c: UnresolvedPath"); },
		[&](UnappliedProcExpr const&) -> string { assert(!"generate_c: UnappliedProcExpr"); },
		[&](VarExpr const &e)
		{
			return e.var->name;
		},
		[&](ConstructorExpr const &e)
		{
			StructType const &st = std::get<StructType>(e.ctor);
			if(st.name() == "c_char")
				return "(char)"s;

			return mangle_constructor(StructType(st.inst()));
		},
		[&](ProcExpr const &e)
		{
			return mangle_procedure(e.inst);
		},
		[&](UnaryExpr const &e)
		{
			string sub_val = generate_c(*e.sub, backend);
			switch(e.op)
			{
				case UnaryOp::NOT: return "(!" + sub_val + ")";
				case UnaryOp::NEG: return "(-" + sub_val + ")";
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

				case BinaryOp::EQ: return "(" + left_val + " == " + right_val + ")";
				case BinaryOp::LT: return "(" + left_val + " < "  + right_val + ")";
				case BinaryOp::LE: return "(" + left_val + " <= " + right_val + ")";
				case BinaryOp::GT: return "(" + left_val + " > "  + right_val + ")";
				case BinaryOp::GE: return "(" + left_val + " >= " + right_val + ")";
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
			if(is_optional_ptr(*e.object->type) && e.member == "value")
				return generate_c(*e.object, backend);
			else
			{
				string object_val = generate_c(*e.object, backend);
				return object_val + "." + e.member;
			}
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
			if(ConstructorExpr const *ctor_expr = std::get_if<ConstructorExpr>(e.callable.get()))
			{
				if(Type const *pointee_type = is_optional_ptr(&ctor_expr->struct_()))
				{
					if(ctor_expr->struct_().inst()->def->name == "None")
						return "NULL"s;

					// Optional.Some
					return generate_c_cast(*pointee_type, *e.args[0].expr, backend);
				}
			}


			vector<Parameter> const &params = std::get<ProcType>(*e.callable->type).def->params;

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
					arg_vals[i] = generate_c_cast(params[i].type, *params[i].default_value, backend);
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
		[&](UnionInitExpr const &e)
		{
			UnionTypeDef const  &union_ = *std::get<UnionType>(*expr.type).canonical_def;
			string init_val = generate_c_cast(union_.alternatives[e.alt_idx], *e.value, backend);

			string union_var = backend.new_tmp_var();
			backend << "struct " << mangle_type(*expr.type) << " " << union_var << ";" << LineEnd;
			backend << union_var << ".__myca__discr = " << e.alt_idx << ";" << LineEnd;
			backend << union_var << ".__myca_alt" << e.alt_idx << " = " << init_val << ";" << LineEnd;

			return union_var;
		},
		[&](TryExpr const &e) -> string
		{
			generate_c(*e.match_stmt, backend);
			return generate_c(*e.result_expr, backend);
		},
	};
}


//--------------------------------------------------------------------
// Statements
//--------------------------------------------------------------------
void generate_c_pattern(Pattern const &lhs_pattern, string const &rhs_expr, Type const &rhs_type, CBackend &backend)
{
	lhs_pattern | match
	{
		[&](VarPattern const &p)
		{
			string expr_str = generate_c_cast(*lhs_pattern.type, rhs_expr, rhs_type);
			backend << *lhs_pattern.type << " " << p.var->name << " = " << expr_str << ";" << LineEnd;
		},
		[&](DerefPattern const &p)
		{
			string deref_expr = generate_c_cast(*std::get<PointerType>(rhs_type).target_type, "*(" + rhs_expr + ")", *lhs_pattern.type);
			generate_c_pattern(*p.sub, deref_expr, *lhs_pattern.type, backend);
		},
		[&](AddressOfPattern const &p)
		{
			string addr_expr = generate_c_cast(PointerType(clone(rhs_type), p.mutability), "&(" + rhs_expr + ")", *lhs_pattern.type);
			generate_c_pattern(*p.sub, addr_expr, *lhs_pattern.type, backend);
		},
		[&](ConstructorPattern const &p)
		{
			StructType const *ctor = std::get_if<StructType>(&p.ctor);
			if(p.has_parens and ctor and ctor->inst()->constructor_params)
			{
				assert(p.args.size() == ctor->inst()->constructor_params->size());
				for(PatternArgument const &arg: p.args)
				{
					string object_str = generate_c_cast(p.ctor, rhs_expr, rhs_type);
					Parameter const &param = ctor->inst()->constructor_params->at(*arg.param_idx);

					if(is_optional_ptr(p.ctor) && param.name == "value")
						generate_c_pattern(*arg.pattern, object_str, param.type, backend);
					else
						generate_c_pattern(*arg.pattern, object_str + "." + param.name, param.type, backend);
				}
			}
		},
		[&](WildcardPattern const&) {},
	};
}

MatchArm const* get_optional_some(MatchStmt const &stmt)
{
	for(MatchArm const &arm: stmt.arms)
	{
		if(StructType const *st = std::get_if<StructType>(&*arm.capture.type))
		{
			if(st->inst()->def->name == "Option" or st->inst()->def->name == "Some")
				return &arm;
		}
	}

	return nullptr;
}

MatchArm const* get_optional_none(MatchStmt const &stmt)
{
	for(MatchArm const &arm: stmt.arms)
	{
		if(StructType const *st = std::get_if<StructType>(&*arm.capture.type))
		{
			if(st->inst()->def->name == "Option" or st->inst()->def->name == "None")
				return &arm;
		}
	}

	return nullptr;
}

void generate_c(Stmt const &stmt, CBackend &backend)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			if(s.init_expr)
			{
				string init_expr_var = generate_c(*s.init_expr, backend);
				generate_c_pattern(s.lhs, init_expr_var, *s.init_expr->type, backend);
			}
			else
			{
				VarDef *var = std::get<VarPattern>(s.lhs).var;
				backend << *var->type << " " << var->name << ";" << LineEnd;
			}
		},
		[&](ExprStmt const &s)
		{
			string expr_str = generate_c(s.expr, backend, false);
			if(expr_str.length())
				backend << expr_str << ";" << LineEnd;
		},
		[&](BlockStmt const &s)
		{
			if(s.scope)
			{
				backend << "{" << LineEnd;
				backend.increase_indent();
			}

			for(OwnPtr<Stmt> const &stmt: s.stmts)
				generate_c(*stmt, backend);

			if(s.scope)
			{
				backend.decrease_indent();
				backend << "}" << LineEnd;
			}
		},
		[&](ReturnStmt const &s)
		{
			if(s.ret_expr)
			{
				string ret_val = generate_c_cast(backend.proc()->type.ret, *s.ret_expr, backend);
				backend << "return " << ret_val << ";" << LineEnd;
			}
			else
				backend << "return;" << LineEnd;
		},
		[&](IfStmt const &s)
		{
			string cond_str = generate_c(s.condition, backend);
			backend << "if(" << cond_str << ")" << LineEnd;
			generate_c(*s.then, backend);
			if(s.else_)
			{
				backend << "else" << LineEnd;
				generate_c(*s.else_, backend);
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
				generate_c(*s.body, backend);
			backend.decrease_indent();
			backend << "}" << LineEnd;
		},
		[&](MatchStmt const &s)
		{
			if(is_optional_ptr(*s.expr.type))
			{
				string subject_str = generate_c(s.expr, backend);
				backend << "if(" << subject_str << ")" << LineEnd;
				backend << "{" << LineEnd;
				backend.increase_indent();
					if(MatchArm const *some_arm = get_optional_some(s))
					{
						generate_c_pattern(some_arm->capture, subject_str, *s.expr.type, backend);
						generate_c(*some_arm->stmt, backend);
					}
				backend.decrease_indent();
				backend << "}" << LineEnd;
				backend << "else" << LineEnd;
				backend << "{" << LineEnd;
				backend.increase_indent();
					if(MatchArm const *none_arm = get_optional_none(s))
					{
						generate_c_pattern(none_arm->capture, subject_str, *s.expr.type, backend);
						generate_c(*none_arm->stmt, backend);
					}
				backend.decrease_indent();
				backend << "}" << LineEnd;
			}
			else
			{
				string subject_str = generate_c(s.expr, backend);
				backend << "switch(" << subject_str << ".__myca__discr)" << LineEnd;
				backend << "{" << LineEnd;
				backend.increase_indent();
					bool has_wildcard = false;
					for(MatchArm const &arm: s.arms)
					{
						if(is<WildcardPattern>(arm.capture))
						{
							backend << "default:" << LineEnd;
							has_wildcard = true;
						}
						else
							backend << "case " << *arm.discr << ":" << LineEnd;

						backend << "{" << LineEnd;
						backend.increase_indent();

						if(not is<WildcardPattern>(arm.capture))
						{
							if(is<UnionType>(*s.expr.type))
							{
								string arm_expr = subject_str + ".__myca_alt" + std::to_string(*arm.discr);
								generate_c_pattern(arm.capture, arm_expr, *arm.capture.type, backend);
							}
							else
								generate_c_pattern(arm.capture, subject_str, *s.expr.type, backend);
						}

						generate_c(*arm.stmt, backend);

						backend.decrease_indent();
						backend << "}" << LineEnd;

						backend << "break;" << LineEnd;
					}
					if(not has_wildcard)
						backend << "default: assert(0);" << LineEnd;
				backend.decrease_indent();
				backend << "}" << LineEnd;
			}
		},
	};
}


//--------------------------------------------------------------------
// Structs
//--------------------------------------------------------------------

// Generate C struct fields for all the variable members of `struct_` that come before the first
// case member
void create_c_struct_initial_vars(StructDefInstance const *struct_inst, optional<size_t> child_case_idx, CStruct *result)
{
	Module &mod = *struct_inst->def->scope->mod();
	if(struct_inst->outer_struct)
		create_c_struct_initial_vars(struct_inst->outer_struct->inst(), struct_inst->def->get_case_idx(), result);

	if(struct_inst->discriminator_type)
		// REVISIT: When the struct contains multiple nested case members then the name is not
		// unique anymore
		result->add(CMember("__myca__discr", child_case_idx, Type(*struct_inst->discriminator_type)), mod);

	for(InstanceMember const &member: struct_inst->initial_var_members())
	{
		Parameter const &param = std::get<Parameter>(member);
		result->add(CMember(param.name, clone(param.type)), mod);
	}
}

// Generate C struct fields for all the variable members of `struct_inst` that come after its case members
void create_c_struct_trailing_vars(StructDefInstance const *struct_inst, CStruct *result)
{
	Module &mod = *struct_inst->def->scope->mod();
	for(InstanceMember const &member: struct_inst->trailing_var_members())
	{
		Parameter const &param = std::get<Parameter>(member);
		result->add(CMember(param.name, clone(param.type)), mod);
	}

	if(struct_inst->outer_struct)
	{
		size_t additional_padding = struct_inst->outer_struct->inst()->cases_layout->end() - result->cur_layout.size;
		if(additional_padding)
		{
			for(size_t i = 0; i < additional_padding; ++i)
				result->add(CMember(BuiltinType::U8), mod);
		}

		create_c_struct_trailing_vars(struct_inst->outer_struct->inst(), result);
	}
}

CStruct create_c_struct(StructDefInstance *struct_inst)
{
	Module &mod = *struct_inst->def->scope->mod();
	CStruct result{
		mangle_type(StructType(struct_inst)),
		mangle_constructor(StructType(struct_inst))
	};

	create_c_struct_initial_vars(struct_inst, nullopt, &result);

	if(struct_inst->cases_layout)
	{
		for(size_t i = 0; i < struct_inst->cases_layout->size; ++i)
			result.add(CMember(BuiltinType::U8), mod);
	}

	create_c_struct_trailing_vars(struct_inst, &result);

	MemoryLayout struct_layout = struct_inst->layout();
	assert(result.cur_layout.size == struct_layout.size);
	result.cur_layout.alignment = std::max(result.cur_layout.alignment, struct_layout.alignment);

	return result;
}

void generate_c_struct_methods(CStruct const &cstruct, CBackend &backend)
{
	// Generate constructor
	{
		// Function header
		backend << "struct " << cstruct.name << " " << cstruct.constructor_name << "(";
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


//--------------------------------------------------------------------
// Union types
//--------------------------------------------------------------------
void generate_c_union_type(UnionTypeDef const &union_, CBackend &backend)
{
	assert(union_.alternatives.size() <= 255);



	backend << "struct " << mangle_union_type(union_) << LineEnd;
	backend << "{" << LineEnd;
	backend.increase_indent();
		backend << "uint8_t __myca__discr;" << LineEnd;
		backend << "union" << LineEnd;
		backend << "{" << LineEnd;
		backend.increase_indent();
			for(size_t i = 0; i < union_.alternatives.size(); ++i)
			{
				Type const &alt = union_.alternatives[i];
				backend << alt << " " << "__myca_alt" << i << ";" << LineEnd;
			}
		backend.decrease_indent();
		backend << "};" << LineEnd;
	backend.decrease_indent();
	backend << "};" << LineEnd;
}


//--------------------------------------------------------------------
// Procedures
//--------------------------------------------------------------------
void generate_c_proc_sig(ProcDefInstance const *proc, CBackend &backend)
{
	if(equiv(proc->type.ret, BuiltinType::UNIT))
		backend << "void";
	else
		backend << proc->type.ret;

	backend << " " << mangle_procedure(proc) << "(";
	backend << RangeFmt(proc->type.params, ", ", [&](Parameter const &p)
	{
		backend << p.type << " " << p.name;
	});
	backend << ")";
}


//--------------------------------------------------------------------
// Whole module
//--------------------------------------------------------------------
void _sort_types_by_deps(
	TypeDefInstance type,
	vector<TypeDefInstance> &result,
	unordered_set<TypeDefInstance> &visited,
	Module &mod
)
{
	// We assume the semantic analysis would already have detected any dependency cycles
	auto res = visited.insert(type);
	if(!res.second)
		return;

	unordered_set<TypeDefInstance> const &deps = type | match
	{
		[&](StructDefInstance const *inst) -> unordered_set<TypeDefInstance> const&
		{
			return inst->type_deps;
		},
		[&](UnionTypeDef const *union_) -> unordered_set<TypeDefInstance> const&
		{
			return mod.try_get_union_info(union_)->type_deps;
		},
	};

	for(TypeDefInstance dep: deps)
		_sort_types_by_deps(dep, result, visited, mod);

	result.push_back(type);
}

vector<TypeDefInstance> sort_types_by_deps(vector<TypeDefInstance> const &types, Module &mod)
{
	vector<TypeDefInstance> result;
	unordered_set<TypeDefInstance> visited;
	for(TypeDefInstance type: types)
		_sort_types_by_deps(type, result, visited, mod);

	return result;
}

void gather_concrete_instances(StructDef *struct_, vector<TypeDefInstance> &result)
{
	for(auto &[_, inst]: struct_->instances)
	{
		if(inst.is_concrete())
			result.push_back(&inst);
	}

	for(Member &member: struct_->members)
	{
		if(StructDef **case_member = std::get_if<StructDef*>(&member))
			gather_concrete_instances(*case_member, result);
	}
}

struct NewInstanceListener : ModuleListener
{
	virtual void new_struct_inst(StructDefInstance *inst) override
	{
		structs.push_back(inst);
	}

	virtual void new_union_inst(UnionTypeDef const *def) override
	{
		unions.push_back(def);
	}

	virtual void new_proc_inst(ProcDefInstance *inst) override
	{
		procs.push_back(inst);
	}

	vector<StructDefInstance*> structs;
	vector<UnionTypeDef const*> unions;
	vector<ProcDefInstance*> procs;
};

struct ConcreteProcInstance
{
	ConcreteProcInstance(ProcDefInstance *proc, SemaContext &ctx) :
		proc(proc)
	{
		assert(proc->is_concrete);
		if(proc->def->body)
		{
			body = clone(*proc->def->body);
			instantiate_types(*body, proc->type_env, ctx);
		}
	}

	ProcDefInstance *proc;
	optional<Stmt> body;
};

void generate_c(Module &mod, CBackend &backend)
{
	NewInstanceListener listener;
	mod.add_listener(&listener);

	SemaContext ctx(mod);

	vector<TypeDefInstance> types;
	vector<ConcreteProcInstance> procs;
	for(TopLevelItem item: mod.items())
	{
		item | match
		{
			[&](ProcDef *def)
			{
				// Make sure we have at least one instance
				if(def->type.type_params.empty())
					instantiate_proc(def, {}, ctx);

				for(auto &[_, inst]: def->instances)
				{
					if(inst.is_concrete)
						procs.push_back(ConcreteProcInstance(&inst, ctx));
				}
			},
			[&](StructDef *def) { gather_concrete_instances(def, types); },
			[&](AliasDef*) {},
		};
	}

	for(UnionTypeDef const &union_: mod.union_types())
		types.push_back(&union_);

	while(listener.structs.size() || listener.procs.size())
	{
		vector<StructDefInstance*> new_structs = std::move(listener.structs);
		vector<ProcDefInstance*> new_procs = std::move(listener.procs);
		listener.structs = {};
		listener.unions = {};
		listener.procs = {};

		for(StructDefInstance *new_struct: listener.structs)
			types.push_back(new_struct);

		for(UnionTypeDef const *new_union: listener.unions)
			types.push_back(new_union);

		for(ProcDefInstance *new_proc: listener.procs)
		{
			if(new_proc->is_concrete)
				procs.push_back(ConcreteProcInstance(new_proc, ctx));
		}
	}

	mod.remove_listener(&listener);

	// Type definitions
	vector<TypeDefInstance> sorted_types = sort_types_by_deps(types, mod);
	unordered_map<StructDefInstance*, CStruct> cstructs;
	for(TypeDefInstance type: sorted_types)
	{
		type | match
		{
			[&](StructDefInstance *inst)
			{
				CStruct cstruct = create_c_struct(inst);
				generate_c_struct_def(cstruct, backend);
				backend << LineEnd;

				cstructs.emplace(inst, std::move(cstruct));
			},
			[&](UnionTypeDef const *union_)
			{
				generate_c_union_type(*union_, backend);
			},
		};
	}

	// Type method definitions
	for(TypeDefInstance type: sorted_types)
	{
		type | match
		{
			[&](StructDefInstance *inst)
			{
				generate_c_struct_methods(cstructs.at(inst), backend);
				backend << LineEnd;
			},
			[&](UnionTypeDef const*) {},
		};
	}

	// Procedures
	for(ConcreteProcInstance &inst: procs)
	{
		generate_c_proc_sig(inst.proc, backend);
		backend << ";" << LineEnd;
	}

	for(ConcreteProcInstance &inst: procs)
	{
		if(inst.body)
		{
			backend.set_current_proc(inst.proc);
			generate_c_proc_sig(inst.proc, backend);
			backend << LineEnd;
			generate_c(*inst.body, backend);
			backend << LineEnd;
		}
	}
}


//==============================================================================
// Main
//==============================================================================
#define NEXT(args) (*++args)

int main(int argc, char *argv[])
{
	(void)argc;

	optional<string> arg_input_filename;
	optional<string> arg_output_filename;
	while(NEXT(argv))
	{
		if(*argv == "-o"sv) {
			if(!NEXT(argv)) {
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
