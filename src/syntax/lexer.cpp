#include "syntax/lexer.hpp"

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
		case Lexeme::SLASH: return "SLASH";
		case Lexeme::STAR: return "STAR";
		case Lexeme::EQ: return "EQ";
		case Lexeme::NE: return "NE";
		case Lexeme::COLON_EQ: return "COLON_EQ";
		case Lexeme::DOUBLE_EQ: return "DOUBLE_EQ";
		case Lexeme::LT: return "LT";
		case Lexeme::LE: return "LE";
		case Lexeme::GT: return "GT";
		case Lexeme::GE: return "GE";

		case Lexeme::NOT: return "NOT";
		case Lexeme::AND: return "AND";
		case Lexeme::OR: return "OR";

		case Lexeme::LET: return "LET";
		case Lexeme::PROC: return "PROC";
		case Lexeme::STRUCT: return "STRUCT";
		case Lexeme::CASE: return "CASE";
		case Lexeme::IMPLICIT: return "IMPLICIT";

		case Lexeme::TYPE_NEVER: return "TYPE_NEVER";
		case Lexeme::TYPE_BOOL: return "TYPE_BOOL";
		case Lexeme::TYPE_UNIT: return "TYPE_UNIT";
		case Lexeme::TYPE_I8: return "TYPE_I8";
		case Lexeme::TYPE_U8: return "TYPE_U8";
		case Lexeme::TYPE_I32: return "TYPE_I32";
		case Lexeme::TYPE_U32: return "TYPE_U32";
		case Lexeme::TYPE_ISIZE: return "TYPE_ISIZE";
		case Lexeme::TYPE_USIZE: return "TYPE_USIZE";

		case Lexeme::IF: return "IF";
		case Lexeme::ELSE: return "ELSE";
		case Lexeme::WHILE: return "WHILE";
		case Lexeme::MATCH: return "MATCH";
		case Lexeme::RETURN: return "RETURN";

		case Lexeme::AS: return "AS";
		case Lexeme::MUT: return "MUT";
		case Lexeme::REF: return "REF";
		case Lexeme::BARE: return "BARE";

		case Lexeme::TYPEALIAS: return "TYPEALIAS";
		case Lexeme::EXTERN: return "EXTERN";

		case Lexeme::SIZE_OF: return "SIZE_OF";
		case Lexeme::MAKE: return "MAKE";

		case Lexeme::END: return "END_OF_FILE";
	}

	UNREACHABLE;
}

void consume(Lexer &lexer, string_view expected_str)
{
	if(not try_consume(lexer, expected_str))
	{
		if(optional<char> ch = lexer.peek())
			throw LexingError("Expected \""s + expected_str + "\", got \"" + *ch + "\"");
		else
			throw LexingError("Expected \""s + expected_str + "\", got EOF");
	}
}

bool try_consume(Lexer &lexer, string_view expected_str)
{
	string_view remaining_str = lexer.remaining_string();
	if(remaining_str.length() < expected_str.length())
		return false;

	if(not remaining_str.starts_with(expected_str))
		return false;

	lexer.advance(expected_str.length());
	return true;
}


void skip_whitespace(Lexer &lexer)
{
	while(lexer.has_more() && is_whitespace(lexer.get()))
		lexer.advance();
}

static size_t comment_start_marker_length(CommentKind kind)
{
	switch(kind)
	{
		case CommentKind::NORMAL: return 2;  // "//" or "/*"
		case CommentKind::DOC: return 3;     // "///" or "/**"
		case CommentKind::SPECIAL: return 3; // "//!" or "/*!"
	}

	UNREACHABLE;
}

static optional<Comment> try_read_block_comment(Lexer &lexer)
{
	SourceLocation comment_start = lexer.location();
	if(not try_consume(lexer, "/*"))
		return nullopt;

	// Determine the kind of comment (NORMAL, DOC, SPECIAL)
	CommentKind kind = CommentKind::NORMAL;
	if(lexer.peek() == '*' and lexer.peek(1) != '/')
	{
		kind = CommentKind::DOC;
		lexer.advance();
	}
	else if(try_consume(lexer, "!"))
		kind = CommentKind::SPECIAL;

	while(lexer.has_more())
	{
		if(try_consume(lexer, "*/"))
		{
			string_view comment_text = lexer.substr_from(comment_start.pos);
			comment_text.remove_prefix(comment_start_marker_length(kind));
			comment_text.remove_suffix(2);
			return Comment{
				.start_loc = comment_start,
				.end_loc = lexer.location(),
				.text = comment_text,
				.style = CommentStyle::BLOCK,
				.kind = kind,
				.target = CommentTarget::NONE, // Needs to be set by someone else
			};
		}

		lexer.advance();
	}

	throw LexingError{"Unclosed block comment"};
}

static optional<Comment> try_read_line_comment(Lexer &lexer)
{
	SourceLocation comment_start = lexer.location();
	if(not try_consume(lexer, "//"))
		return nullopt;

	// Determine the kind of comment (NORMAL, DOC, SPECIAL)
	CommentKind kind = CommentKind::NORMAL;
	if(try_consume(lexer, "/"))
	{
		kind = CommentKind::DOC;
		lexer.advance();
	}
	else if(try_consume(lexer, "!"))
		kind = CommentKind::SPECIAL;

	SourceLocation comment_end = comment_start;
	while(lexer.has_more())
	{
		if(lexer.get() == '\n')
		{
			comment_end = lexer.location();
			lexer.advance();
			break;
		}
		lexer.advance();
	}

	string_view comment_text = lexer.substr_from(comment_start.pos);
	comment_text.remove_prefix(comment_start_marker_length(kind));

	return Comment{
		.start_loc = comment_start,
		.end_loc = comment_end,
		.text = comment_text,
		.style = CommentStyle::BLOCK,
		.kind = kind,
		.target = CommentTarget::NONE, // Needs to be set by someone else
	};
}

optional<Comment> try_read_comment(Lexer &lexer)
{
	if(optional<Comment> c = try_read_block_comment(lexer))
		return c;

	return try_read_line_comment(lexer);
}

void skip_whitespace_and_comments(Lexer &lexer)
{
	do skip_whitespace(lexer);
	while(try_read_comment(lexer));
}

optional<string_view> try_read_identifier(Lexer &lexer)
{
	if(!lexer.has_more())
		return nullopt;

	char ch = lexer.get();
	if(ch == '_' || is_alphabetic(ch))
	{
		size_t id_start_pos = lexer.global_byte_pos();
		do
			lexer.advance();
		while(
			lexer.has_more() &&
			(lexer.get() == '_' || is_alphabetic(lexer.get()) || is_digit(lexer.get()))
		);

		return lexer.substr_from(id_start_pos);
	}

	return nullopt;
}

optional<string_view> try_read_number(Lexer &lexer)
{
	size_t start_pos = lexer.global_byte_pos();
	while(lexer.has_more() && is_digit(lexer.get()))
		lexer.advance();

	string_view integer_str = lexer.substr_from(start_pos);
	if(integer_str.empty())
		return nullopt;

	return integer_str;
}

optional<Lexeme> try_read_punctuation(Lexer &lexer)
{
	switch(lexer.get())
	{
		case '.': lexer.advance(); return Lexeme::DOT;
		case ',': lexer.advance(); return Lexeme::COMMA;
		case ':':
		{
			lexer.advance(); 
			if(try_consume(lexer, "="))
				return Lexeme::COLON_EQ;

			return Lexeme::COLON;
		}
		case ';': lexer.advance(); return Lexeme::SEMICOLON;
		case '^': lexer.advance(); return Lexeme::CIRCUMFLEX;
		case '&': lexer.advance(); return Lexeme::AMPERSAND;
		case '?': lexer.advance(); return Lexeme::QUESTIONMARK;
		case '!':
		{
			lexer.advance();
			if(try_consume(lexer, "="))
				return Lexeme::NE;

			return Lexeme::BANG;
		}
		case '\'': lexer.advance(); return Lexeme::SINGLE_QUOTE;
		case '@': lexer.advance(); return Lexeme::AT;
		case '|': lexer.advance(); return Lexeme::BAR;
		case '(': lexer.advance(); return Lexeme::LEFT_PAREN;
		case ')': lexer.advance(); return Lexeme::RIGHT_PAREN;
		case '{': lexer.advance(); return Lexeme::LEFT_BRACE;
		case '}': lexer.advance(); return Lexeme::RIGHT_BRACE;
		case '[': lexer.advance(); return Lexeme::LEFT_BRACKET;
		case ']': lexer.advance(); return Lexeme::RIGHT_BRACKET;
		case '+': lexer.advance(); return Lexeme::PLUS;
		case '-':
		{
			lexer.advance();
			if(try_consume(lexer, ">"))
				return Lexeme::THIN_ARROW;

			return Lexeme::MINUS;
		}
		case '*': lexer.advance(); return Lexeme::STAR;
		case '/': lexer.advance(); return Lexeme::SLASH;
		case '=':
		{
			lexer.advance();
			if(try_consume(lexer, "="))
				return Lexeme::DOUBLE_EQ;
				
			return Lexeme::EQ;
		}
		case '<':
		{
			lexer.advance();
			if(try_consume(lexer, "="))
				return Lexeme::LE;
				
			return Lexeme::LT;
		}
		case '>':
		{
			lexer.advance();
			if(try_consume(lexer, "="))
				return Lexeme::GE;
				
			return Lexeme::GT;
		}

		default: return nullopt;
	}
}

static unordered_map<string_view, Lexeme> const KEYWORDS = {
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
	{"and", Lexeme::AND},
	{"or", Lexeme::OR},
	{"as", Lexeme::AS},
	{"mut", Lexeme::MUT},
	{"ref", Lexeme::REF},
	{"bare", Lexeme::BARE},
	{"typealias", Lexeme::TYPEALIAS},
	{"extern", Lexeme::EXTERN},
	{"size_of", Lexeme::SIZE_OF},
	{"make", Lexeme::MAKE},

	{"Never", Lexeme::TYPE_NEVER},
	{"bool", Lexeme::TYPE_BOOL},
	{"unit", Lexeme::TYPE_UNIT},
	{"i8", Lexeme::TYPE_I8},
	{"u8", Lexeme::TYPE_U8},
	{"i32", Lexeme::TYPE_I32},
	{"u32", Lexeme::TYPE_U32},
	{"isize", Lexeme::TYPE_ISIZE},
	{"usize", Lexeme::TYPE_USIZE},
};

optional<Token> next_token(Lexer &lexer)
{
	skip_whitespace_and_comments(lexer);
	if(!lexer.has_more())
		return nullopt;

	SourceLocation loc_begin = lexer.location();
	if(optional<string_view> int_text = try_read_number(lexer))
	{
		return Token{
			.kind = Lexeme::INT_LITERAL,
			.text = *int_text,
			.span = {loc_begin, lexer.location()},
		};
	}

	if(optional<Lexeme> tok_kind = try_read_punctuation(lexer))
	{
		return Token{
			.kind = *tok_kind,
			.text = lexer.substr_from(loc_begin.pos),
			.span = {loc_begin, lexer.location()},
		};
	}

	// C string literals
	if(try_consume(lexer, "c\""))
	{
		size_t string_start_pos = lexer.global_byte_pos();
		while(lexer.has_more())
		{
			if(lexer.get() == '"')
				break;

			lexer.advance();
		}
		string_view string_literal = lexer.substr_from(string_start_pos);

		if(!try_consume(lexer, "\""))
			throw LexingError("Missing closing quote");

		return Token{
			.kind = Lexeme::C_STRING_LITERAL,
			.text = string_literal,
			.span = {loc_begin, lexer.location()},
		};
	}

	if(optional<string_view> id = try_read_identifier(lexer))
	{
		if(auto keyword_it = KEYWORDS.find(*id); keyword_it != KEYWORDS.end())
		{
			return Token{
				.kind = keyword_it->second,
				.text = lexer.substr_from(loc_begin.pos),
				.span = {loc_begin, lexer.location()},
			};
		}

		return Token{
			.kind = Lexeme::IDENTIFIER,
			.text = *id,
			.span = {loc_begin, lexer.location()},
		};
	}

	throw LexingError("Invalid character: "s + lexer.get());
}
