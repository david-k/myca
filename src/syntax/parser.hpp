#pragma once

#include "utils.hpp"
#include "syntax/lexer.hpp"

struct Type;
struct Expr;
struct GenericArg;
struct Pattern;
struct Stmt;

struct StructItem;
struct ProcItem;
struct Module;
struct TopLevelItem;

class ParseError : public std::runtime_error
{
public:
	ParseError(string const &msg) :
		std::runtime_error{msg} {}
};

class Parser
{
public:
	Parser() = default;

	explicit Parser(string_view source)
	{
		append_source(source);
		fetch_tokens_until(m_token_pos);
	}

	Lexer const& lexer() const {
		return m_lexer;
	}

	TokenIdx token_idx() const {
		return TokenIdx(m_token_pos);
	}

	TokenIdx prev_token_idx() const {
		return TokenIdx(m_token_pos > 0 ? m_token_pos - 1 : 0);
	}

	size_t bytes_consumed() const
	{
		if (m_token_pos > 0)
			return m_tokens[m_token_pos - 1].span.end.pos;

		return 0;
	}

	Token const& get() const
	{
		if (m_token_pos < m_tokens.size())
			return m_tokens[m_token_pos];

		return m_end_token;
	}

	Token const& token_at(TokenIdx idx) const
	{
		if (idx.value < m_tokens.size())
			return m_tokens[idx.value];

		return m_end_token;
	}

	string_view get_text(SourceSpan span) const
	{
		size_t start = span.begin.pos;
		size_t end = span.end.pos;
		size_t current_offset = 0;

		// Linear search is okay for now as this function is only used for
		// diagnostics and there is usually only one source
		for (string_view source: m_sources)
		{
			if (start < current_offset + source.length())
			{
				// SourceSpans cannot cross source boundaries
				assert(end <= current_offset + source.length());
				return source.substr(start - current_offset, end - start);
			}

			current_offset += source.length();
		}

		assert(!"Invalid SourceSpan");
	}

	void append_source(string_view new_source)
	{
		SourceLocation prev_end = get().span.begin;
		m_tokens.erase(m_tokens.begin() + m_token_pos, m_tokens.end());

		if (m_sources.size())
		{
			string_view &prev_source = m_sources.back();
			size_t unprocessed = (m_lexer.initial_byte_pos() + prev_source.length()) - prev_end.pos;
			prev_source.remove_suffix(unprocessed);
		}

		m_sources.push_back(new_source);
		m_lexer = Lexer(new_source, prev_end);
		fetch_tokens_until(m_token_pos);
	}

	Token const& next()
	{
		fetch_tokens_until(m_token_pos + 1);
		if (m_token_pos < m_tokens.size())
			return m_tokens[m_token_pos++];

		return m_end_token;
	}

	void rewind()
	{
		assert(m_token_pos > 0);
		m_token_pos -= 1;
	}

private:
	void fetch_tokens_until(size_t pos)
	{
		while(pos >= m_tokens.size())
		{
			if(optional<Token> tok = next_token(m_lexer))
				m_tokens.push_back(*tok);
			else
				break;
		}

		m_end_token.span.begin = m_end_token.span.end = m_lexer.location();
	}

	Lexer m_lexer = {""};
	vector<string_view> m_sources;
	vector<Token> m_tokens;
	size_t m_token_pos = 0; // Index of the next token to be processed

	// It's a member so we can return a reference to it
	Token m_end_token{
		.kind = Lexeme::END,
		.text = "END",
		.span = {
			.begin = {.pos = 0, .line = 1, .col = 1},
			.end = {.pos = 0, .line = 1, .col = 1},
		},
	};
};

Token const& consume(Parser &parser, Lexeme kind);
optional<Token> try_consume(Parser &parser, Lexeme kind);

uint64_t parse_integer(string_view integer_string);
Type parse_type(Parser &parser, Memory M);
Expr parse_expr(Parser &parser, Memory M);
Pattern parse_pattern(Parser &parser, Memory M);
Stmt parse_stmt(Parser &parser, Memory M);

enum class StructParseContext
{
	TOP_LEVEL,
	CASE_MEMBER,
	INLINE,
};
StructItem parse_struct(Parser &parser, StructParseContext struct_context, Memory M);
ProcItem parse_proc(Parser &parser, Memory M);
TopLevelItem parse_top_level_item(Parser &parser, Memory M);

Module parse_module(string_view source, Memory M);
