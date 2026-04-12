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

	Token get() const
	{
		if (m_token_pos < m_tokens.size())
			return m_tokens[m_token_pos];

		return end_token();
	}

	Token token_at(TokenIdx idx) const
	{
		if (idx.value < m_tokens.size())
			return m_tokens[idx.value];

		return end_token();
	}

	size_t token_count() const { return m_tokens.size() + 1; }

	string_view get_text(size_t start, size_t end) const
	{
		size_t current_offset = 0;

		// Linear search is okay for now as this function is only used for
		// diagnostics and there is usually only one source
		for(string_view source: m_sources)
		{
			if(start <= current_offset + source.length())
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

	Token next()
	{
		fetch_tokens_until(m_token_pos + 1);
		if (m_token_pos < m_tokens.size())
			return m_tokens[m_token_pos++];

		return end_token();
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
	}

	Token end_token() const
	{
		return Token{
			.kind = Lexeme::END,
			.text = "END",
			.span = {m_lexer.location(), m_lexer.location()},
		};
	}

	Lexer m_lexer = {""};
	vector<string_view> m_sources;
	vector<Token> m_tokens;
	size_t m_token_pos = 0; // Index of the next token to be processed
};

Token consume(Parser &parser, Lexeme kind);
optional<Token> try_consume(Parser &parser, Lexeme kind);

uint64_t parse_valid_uint(string_view integer_string);
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

// Working comments
struct PrecedingText
{
	SourceLocation start_loc;
	SourceLocation end_loc;
	string_view text;
	bool until_eof;
};
// Get the text (whitespace and comments) preceding the Token at the given TokenIdx.
// The PrecedingText starts immediatly after the Token preceding token_idx.
PrecedingText get_preceding_text(TokenIdx token_idx, Parser const &parser);
vector<Comment> parse_comments(PrecedingText text);
vector<string_view> extract_clean_lines(Comment comment);

struct OptionSet
{
	optional<string_view> try_get(string_view key) const
	{
		auto it = opts.find(key);
		if(it == opts.end())
			return nullopt;

		return it->second;
	}

	unordered_map<string_view, string_view> opts;
};
void parse_options_from_comment(Comment const &comment, OptionSet &options);
