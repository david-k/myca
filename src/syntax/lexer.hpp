#pragma once

#include "utils.hpp"
#include <optional>

using std::optional;
using std::nullopt;

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
	SLASH,
	STAR,
	EQ,
	NE,
	COLON_EQ,
	DOUBLE_EQ,
	LT,
	LE,
	GT,
	GE,
	NOT,
	AND,
	OR,

	TYPE_NEVER,
	TYPE_BOOL,
	TYPE_UNIT,
	TYPE_I8,
	TYPE_U8,
	TYPE_I32,
	TYPE_U32,
	TYPE_ISIZE,
	TYPE_USIZE,

	AS,
	MUT,
	REF,
	BARE,

	SIZE_OF,
	MAKE,

	IF,
	ELSE,
	WHILE,
	MATCH,
	RETURN,

	LET,
	PROC,
	STRUCT,
	CASE,
	IMPLICIT,
	TYPEALIAS,
	EXTERN,

	END,
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

struct Token
{
	Lexeme kind;
	string_view text;
	SourceSpan span;
};

static_assert(sizeof(Token) == 56);

struct TokenIdx { uint32_t value; };
inline constexpr TokenIdx INVALID_TOKEN_IDX = TokenIdx(-1);
inline TokenIdx operator + (TokenIdx idx, uint32_t offset) { return TokenIdx(idx.value + offset); }

struct TokenRange { TokenIdx first{}, last{}; };
inline constexpr TokenRange UNKNOWN_TOKEN_RANGE = TokenRange(INVALID_TOKEN_IDX, INVALID_TOKEN_IDX);

string_view str(Lexeme tok);
inline string str(SourceLocation loc) {
	return std::to_string(loc.line) + ":" + std::to_string(loc.col);
}


class LexingError : public std::runtime_error
{
public:
	explicit LexingError(string const &msg) :
		std::runtime_error{msg} {}
};

class Lexer
{
public:
	Lexer(
		string_view source,
		SourceLocation initial_location = {.pos = 0, .line = 1, .col = 1}
	) :
		m_source{source},
		m_initial_byte_pos{initial_location.pos},
		m_byte_pos{0},
		m_line{initial_location.line},
		m_column{initial_location.col} {}

	string_view source() const {
		return m_source;
	}

	bool has_more(size_t how_many = 1) const {
		return m_byte_pos + how_many <= m_source.length();
	}

	size_t global_byte_pos() const {
		return m_initial_byte_pos + m_byte_pos;
	}

	size_t initial_byte_pos() const {
		return m_initial_byte_pos;
	}

	string_view remaining_string() const {
		return {m_source.begin() + m_byte_pos, m_source.end()};
	}

	char get() const
	{
		assert(m_byte_pos < m_source.length());
		return m_source[m_byte_pos];
	}

	optional<char> peek(size_t offset = 0) const
	{
		if(m_byte_pos + offset < m_source.length())
			return m_source[m_byte_pos + offset];

		return nullopt;
	}

	SourceLocation location() const
	{
		return SourceLocation{
			.pos = m_initial_byte_pos + m_byte_pos,
			.line = m_line,
			.col = m_column,
		};
	}

	// Returns the substring from start_pos to the current position
	string_view substr_from(size_t global_start_pos) const {
		assert(global_start_pos >= m_initial_byte_pos);
		size_t local_start_pos = global_start_pos - m_initial_byte_pos;
		assert(local_start_pos <= m_byte_pos);
		return m_source.substr(local_start_pos, m_byte_pos - local_start_pos);
	}

	void advance(size_t n = 1)
	{
		while(n-- && has_more())
		{
			if(m_source[m_byte_pos] == '\n')
			{
				m_line += 1;
				m_column = 1;
			}
			else
				m_column += 1;

			m_byte_pos += 1;
		}
	}

private:
	string_view m_source;
	size_t m_initial_byte_pos;
	size_t m_byte_pos;
	int m_line;
	int m_column;
};

void consume(Lexer &lexer, string_view expected_str);
bool try_consume(Lexer &lexer, string_view expected_str);
void skip_whitespace(Lexer &lexer);
void skip_whitespace_and_comments(Lexer &lexer);

enum class CommentStyle
{
	LINE,
	BLOCK,
};
enum class CommentKind
{
	NORMAL,
	DOC,
	SPECIAL,
};
// Wether a comment is attached to the next or previous token, if at all
enum class CommentTarget
{
	NONE,
	PREVIOUS_TOKEN,
	NEXT_TOKEN,
	AMBIGUOUS,
};
struct Comment
{
	SourceLocation start_loc; // Location of the start of the comment marker ("//" or "/*")
	SourceLocation end_loc; // Location of the end of the comment ("\n" or the end of "*/")
	string_view text; // Comment text without comment markers
	CommentStyle style;
	CommentKind kind;
	optional<CommentTarget> target; // Needs to be computed separately
};
optional<Comment> try_read_comment(Lexer &lexer);

optional<string_view> try_read_identifier(Lexer &lexer);
optional<string_view> try_read_number(Lexer &lexer);
optional<Lexeme> try_read_punctuation(Lexer &lexer);

optional<Token> next_token(Lexer &lex);
