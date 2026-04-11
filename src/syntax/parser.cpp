#include "syntax/ast.hpp"
#include "syntax/parser.hpp"

// The dtor of Module needs the complete type of SemaModule
#include "semantics/module.hpp"

namespace
{
	struct TokenRanger
	{
		explicit TokenRanger(Parser &parser) :
			parser(&parser)
		{
			first = parser.token_idx();
		}

		TokenRange get()
		{
			return TokenRange(first, TokenIdx(parser->token_idx().value - 1));
		}

		TokenIdx first;
		Parser *parser;
	};
}

[[noreturn]] static void throw_parse_error(string const &msg, TokenIdx tok_idx, Parser &parser)
{
	Token tok = parser.token_at(tok_idx);
	throw ParseError("|" + str(tok.span.begin) + "| error: " + msg);
}

PrecedingText get_preceding_text(TokenIdx token_idx, Parser const &parser)
{
	Token token = parser.token_at(token_idx);
	SourceLocation preceding_text_begin{.pos = 0, .line = 1, .col = 1};
	SourceLocation preceding_text_end = token.span.begin;
	if(token_idx.value > 0)
	{
		Token prev_token = parser.token_at(TokenIdx(token_idx.value - 1));
		preceding_text_begin = prev_token.span.end;
	}

	string_view preceding_text = parser.get_text(preceding_text_begin.pos, preceding_text_end.pos);
	return PrecedingText(
		preceding_text_begin,
		preceding_text_end,
		preceding_text
	);
}

Token consume(Parser &parser, Lexeme kind)
{
	Token actual_token = parser.get();
	if(actual_token.kind != kind)
		throw ParseError(str(actual_token.span.begin) + ": Expected "s + str(kind) + ", got " + str(parser.get().kind));

	return parser.next();
}

optional<Token> try_consume(Parser &parser, Lexeme kind)
{
	if(parser.get().kind != kind)
		return std::nullopt;

	return parser.next();
}

uint64_t parse_valid_uint(string_view integer_string)
{
	uint64_t integer;
	std::from_chars_result result = std::from_chars(integer_string.begin(), integer_string.end(), integer);
	assert(result.ec == std::errc());
	assert(result.ptr == integer_string.end());
	return integer;
}

//--------------------------------------------------------------------
// Parsing types
//--------------------------------------------------------------------
static Expr parse_prefix_expr(Parser &parser, Memory M);
static Type parse_prefix_type(Parser &parser, Memory M, bool parse_full_path = true);
static Type parse_union_type(Parser &parser, Memory M);

enum class IsLoneTypeArg
{
	YES,
	NO,
};

static GenericArg parse_generic_arg(IsLoneTypeArg is_lone, Parser &parser, Memory M)
{
	if(parser.get().kind == Lexeme::INT_LITERAL or parser.get().kind == Lexeme::MINUS)
		return parse_prefix_expr(parser, M);
	else if(try_consume(parser, Lexeme::LEFT_PAREN))
	{
		Expr expr = parse_expr(parser, M);
		consume(parser, Lexeme::RIGHT_PAREN);
		return expr;
	}
	else
	{
		if(is_lone == IsLoneTypeArg::YES)
		{
			// By setting `parse_full_path` to `false` we parse `Option'T.Some` as `Option'(T).Some` and
			// not as `Option'(T.Some)`, which I feel is more natural in practice
			return parse_prefix_type(parser, M, false);
		}
		else
			return parse_type(parser, M);
	}
}

static FixedArray<GenericArg>* parse_generic_arg_list(Parser &parser, Memory M)
{
	ListBuilder<GenericArg> type_args(M.temp);
	if(try_consume(parser, Lexeme::LEFT_PAREN))
	{
		while(parser.get().kind != Lexeme::RIGHT_PAREN)
		{
			type_args.append(parse_generic_arg(IsLoneTypeArg::NO, parser, M));
			if(parser.get().kind != Lexeme::RIGHT_PAREN)
				consume(parser, Lexeme::COMMA);
		}
		consume(parser, Lexeme::RIGHT_PAREN);
	}
	else
		type_args.append(parse_generic_arg(IsLoneTypeArg::YES, parser, M));

	return type_args.to_array(*M.main);
}

static Path parse_path(Parser &parser, Memory M)
{
	TokenRanger ranger(parser);
	consume(parser, Lexeme::IDENTIFIER);

	Path path;
	if(try_consume(parser, Lexeme::SINGLE_QUOTE))
		path.type_args = parse_generic_arg_list(parser, M);
	else
		path.type_args = alloc_fixed_array<GenericArg>(0, *M.main);

	if(try_consume(parser, Lexeme::DOT))
		path.child = M.main->alloc<Path>(parse_path(parser, M));

	path.range = ranger.get();
	return path;
}

static Type parse_prefix_type(Parser &parser, Memory M, bool parse_full_path)
{
	TokenRanger ranger(parser);
	Token tok = parser.next();
	switch(tok.kind)
	{
		case Lexeme::TYPE_NEVER: return BuiltinType(ranger.get(), BuiltinTypeDef::NEVER);
		case Lexeme::TYPE_BOOL:  return BuiltinType(ranger.get(), BuiltinTypeDef::BOOL);
		case Lexeme::TYPE_UNIT:  return BuiltinType(ranger.get(), BuiltinTypeDef::UNIT);
		case Lexeme::TYPE_I8:    return BuiltinType(ranger.get(), BuiltinTypeDef::I8);
		case Lexeme::TYPE_U8:    return BuiltinType(ranger.get(), BuiltinTypeDef::U8);
		case Lexeme::TYPE_I32:   return BuiltinType(ranger.get(), BuiltinTypeDef::I32);
		case Lexeme::TYPE_U32:   return BuiltinType(ranger.get(), BuiltinTypeDef::U32);
		case Lexeme::TYPE_ISIZE: return BuiltinType(ranger.get(), BuiltinTypeDef::ISIZE);
		case Lexeme::TYPE_USIZE: return BuiltinType(ranger.get(), BuiltinTypeDef::USIZE);

		case Lexeme::IDENTIFIER:
		{
			if(parse_full_path)
			{
				parser.rewind();
				return parse_path(parser, M);
			}
			else
			{
				return Path{
					.range = ranger.get(),
					.type_args = alloc_fixed_array<GenericArg>(0, *M.main),
					.child = nullptr,
				};
			}
		}

		case Lexeme::CIRCUMFLEX:
		{
			IsMutable mutability = try_consume(parser, Lexeme::MUT) ? IsMutable::YES : IsMutable::NO;
			Type pointee = parse_prefix_type(parser, M);
			return PointerType(
				ranger.get(),
				M.main->alloc<Type>(pointee),
				PointerType::SINGLE,
				mutability
			);
		}

		case Lexeme::LEFT_BRACKET:
		{
			if(try_consume(parser, Lexeme::BARE))
			{
				consume(parser, Lexeme::RIGHT_BRACKET);
				IsMutable mutability = try_consume(parser, Lexeme::MUT) ? IsMutable::YES : IsMutable::NO;
				Type pointee = parse_prefix_type(parser, M);
				return PointerType(
					ranger.get(),
					M.main->alloc<Type>(pointee),
					PointerType::MANY,
					mutability
				);
			}
			else
			{
				Expr count_arg = parse_expr(parser, M);
				consume(parser, Lexeme::RIGHT_BRACKET);
				Type element_type = parse_prefix_type(parser, M);
				return ArrayType(
					ranger.get(),
					M.main->alloc<Type>(element_type),
					M.main->alloc<Expr>(count_arg)
				);
			}
		}

		case Lexeme::QUESTIONMARK:
		{
			Type optional_type = parse_prefix_type(parser, M);
			FixedArray<GenericArg> *type_args = alloc_fixed_array<GenericArg>(1, *M.main);
			type_args->items[0] = optional_type;
				
			return Path(ranger.get(), type_args);
		}

		case Lexeme::BANG:
		{
			Type err_type = parse_prefix_type(parser, M);
			FixedArray<GenericArg> *type_args = alloc_fixed_array<GenericArg>(1, *M.main);
			type_args->items[0] = err_type;

			return Path(ranger.get(), type_args);
		}

		case Lexeme::PROC:
		{
			ListBuilder<Type> params(M.temp);
			consume(parser, Lexeme::LEFT_PAREN);
			while(parser.get().kind != Lexeme::RIGHT_PAREN)
			{
				params.append(parse_type(parser, M));
				if(parser.get().kind != Lexeme::RIGHT_PAREN)
					consume(parser, Lexeme::COMMA);
			}
			consume(parser, Lexeme::RIGHT_PAREN);
			consume(parser, Lexeme::THIN_ARROW);
			Type ret = parse_type(parser, M);

			return ProcTypeUnresolved(ranger.get(), M.main->alloc<Type>(ret), params.to_array(*M.main));
		}

		case Lexeme::BAR: return parse_union_type(parser, M);

		case Lexeme::STRUCT:
		{
			StructItem struct_ = parse_struct(parser, StructParseContext::INLINE, M);
			return InlineStructType(
				ranger.get(),
				M.main->alloc<StructItem>(struct_)
			);
		}

		default: throw_parse_error("Invalid token while parsing type: "s + str(tok.kind), ranger.first, parser);
	}
}

static Type parse_result_type(Parser &parser, Memory M)
{
	TokenRanger ranger(parser);
	Type type = parse_prefix_type(parser, M);
	if(optional<Token> bang_tok = try_consume(parser, Lexeme::BANG))
	{
		TokenRange bang_token_range{parser.prev_token_idx(), parser.prev_token_idx()};
		FixedArray<GenericArg> *type_args = alloc_fixed_array<GenericArg>(2, *M.main);
		type_args->items[0] = type;
		type_args->items[1] = parse_prefix_type(parser, M);

		type = Path(bang_token_range, type_args);
	}

	return type;
}

static Type parse_union_type(Parser &parser, Memory M)
{
	TokenRanger ranger(parser);
	Type type = parse_result_type(parser, M);
	if(parser.get().kind == Lexeme::BAR)
	{
		ListBuilder<Type> alternatives(M.temp);
		alternatives.append(type);
		while(try_consume(parser, Lexeme::BAR))
			alternatives.append(parse_result_type(parser, M));

		type = UnionTypeUnresolved(ranger.get(), alternatives.to_array(*M.main));
	}

	return type;
}

Type parse_type(Parser &parser, Memory M)
{
	return parse_union_type(parser, M);
}

//--------------------------------------------------------------------
// Expressions
//--------------------------------------------------------------------
namespace
{
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
}

enum class IsPrefix
{
	YES,
	NO,
};

static optional<OperatorInfo> get_operator_info(Lexeme tok, IsPrefix is_prefix = IsPrefix::NO)
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

		// Logical NOT
		case Lexeme::NOT:
			return OperatorInfo{.precedence = 4, .assoc = Associativity::LEFT};

		// Multiplication/division
		case Lexeme::STAR:
		case Lexeme::SLASH:
			return OperatorInfo{.precedence = 5, .assoc = Associativity::LEFT};

		// Addition
		case Lexeme::PLUS:
			return OperatorInfo{.precedence = 6, .assoc = Associativity::LEFT};

		case Lexeme::MINUS:
			if(is_prefix == IsPrefix::YES)
				// Unary minus
				return OperatorInfo{.precedence = 4, .assoc = Associativity::LEFT};
			else
				// Subtraction
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

static Expr parse_infix_expr(Parser &parser, Expr left, Memory M);

static Expr parse_expr(Parser &parser, OperatorInfo prev_op, Memory M)
{
	Expr expr = parse_prefix_expr(parser, M);
	for(;;)
	{
		optional<OperatorInfo> next_op = get_operator_info(parser.get().kind);
		if(!next_op)
			break;

		if(
			prev_op.tighter_than(*next_op) ||
			(prev_op.precedence == next_op->precedence && next_op->assoc == Associativity::LEFT)
		)
			break;

		expr = parse_infix_expr(parser, expr, M);
	}

	return expr;
}

static Expr parse_prefix_expr(Parser &parser, Memory M)
{
	TokenRanger ranger(parser);
	Token tok = parser.next();
	switch(tok.kind)
	{
		case Lexeme::IDENTIFIER:
		{
			parser.rewind();
			return parse_path(parser, M);
		}

		case Lexeme::LEFT_PAREN:
		{
			Expr sub_expr = parse_expr(parser, NO_PREVIOUS_OP, M);
			consume(parser, Lexeme::RIGHT_PAREN);
			return sub_expr;
		}

		case Lexeme::INT_LITERAL:
			return IntLiteralExpr(ranger.get(), parse_valid_uint(tok.text));

		case Lexeme::TRUE:
			return BoolLiteralExpr(ranger.get(), true);

		case Lexeme::FALSE:
			return BoolLiteralExpr(ranger.get(), false);

		case Lexeme::C_STRING_LITERAL:
			return StringLiteralExpr(ranger.get(), StringLiteralKind::C, tok.text);

		case Lexeme::NOT:
		{
			Expr sub_expr = parse_expr(parser, *get_operator_info(tok.kind), M);
			return UnaryExpr(
				ranger.get(),
				M.main->alloc<Expr>(sub_expr),
				UnaryOp::NOT
			);
		}

		case Lexeme::MINUS:
		{
			Expr sub_expr = parse_expr(parser, *get_operator_info(Lexeme::MINUS, IsPrefix::YES), M);
			return UnaryExpr(
				ranger.get(),
				M.main->alloc<Expr>(sub_expr),
				UnaryOp::NEG
			);
		}

		case Lexeme::AMPERSAND:
		{
			IsMutable mutability = try_consume(parser, Lexeme::MUT) ? IsMutable::YES : IsMutable::NO;
			Expr sub_expr = parse_expr(parser, *get_operator_info(tok.kind), M);
			return AddressOfExpr(
				ranger.get(),
				M.main->alloc<Expr>(sub_expr),
				mutability
			);
		}

		case Lexeme::SIZE_OF:
		{
			consume(parser, Lexeme::LEFT_PAREN);
			Type type = parse_type(parser, M);
			consume(parser, Lexeme::RIGHT_PAREN);

			return SizeOfExpr(ranger.get(), M.main->alloc<Type>(type));
		}

		case Lexeme::MAKE:
		{
			Expr init = parse_expr(parser, NO_PREVIOUS_OP, M);
			consume(parser, Lexeme::AT);
			Expr addr = parse_expr(parser, NO_PREVIOUS_OP, M);

			return MakeExpr(
				ranger.get(),
				M.main->alloc<Expr>(init),
				M.main->alloc<Expr>(addr)
			);
		}

		default:
			throw_parse_error("Invalid token while parsing expression: "s + str(tok.kind), ranger.first, parser);
	}
}

static Expr parse_infix_expr(Parser &parser, Expr left, Memory M)
{
	Token tok = parser.next();
	switch(tok.kind)
	{
		case Lexeme::DOT:
		{
			string_view member_name = consume(parser, Lexeme::IDENTIFIER).text;
			return MemberAccessExpr(
				TokenRange{left.token_range().first, parser.prev_token_idx()},
				M.main->alloc<Expr>(left),
				member_name
			);
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
			Expr right = parse_expr(parser, *get_operator_info(tok.kind), M);
			return BinaryExpr(
				TokenRange{left.token_range().first, parser.prev_token_idx()},
				M.main->alloc<Expr>(left),
				M.main->alloc<Expr>(right),
				BinaryOp(tok.kind)
			);
		}

		case Lexeme::COLON_EQ:
		{
			Expr right = parse_expr(parser, *get_operator_info(tok.kind), M);
			return AssignmentExpr(
				TokenRange{left.token_range().first, parser.prev_token_idx()},
				M.main->alloc<Expr>(left),
				M.main->alloc<Expr>(right)
			);
		}

		case Lexeme::LEFT_PAREN:
		{
			ListBuilder<Argument> args(M.temp);
			while(parser.get().kind != Lexeme::RIGHT_PAREN)
			{
				TokenRanger arg_ranger(parser);
				Argument arg;
				if(try_consume(parser, Lexeme::DOT))
				{
					arg.name = consume(parser, Lexeme::IDENTIFIER).text;

					// TODO Let's try switching to `.arg: default_val`
					consume(parser, Lexeme::EQ);
				}

				arg.expr = parse_expr(parser, NO_PREVIOUS_OP, M);
				arg.range = arg_ranger.get();
				args.append(arg);

				if(parser.get().kind != Lexeme::RIGHT_PAREN)
					consume(parser, Lexeme::COMMA);
			}
			consume(parser, Lexeme::RIGHT_PAREN);

			return CallExpr(
				TokenRange{left.token_range().first, parser.prev_token_idx()},
				M.main->alloc<Expr>(left),
				args.to_array(*M.main)
			);
		}

		case Lexeme::CIRCUMFLEX:
			return DerefExpr(
				TokenRange{left.token_range().first,
				parser.prev_token_idx()},
				M.main->alloc<Expr>(left)
			);

		case Lexeme::LEFT_BRACKET:
		{
			Expr index = parse_expr(parser, NO_PREVIOUS_OP, M);
			consume(parser, Lexeme::RIGHT_BRACKET);
			return IndexExpr(
				TokenRange{left.token_range().first, parser.prev_token_idx()},
				M.main->alloc<Expr>(left),
				M.main->alloc<Expr>(index)
			);
		}

		case Lexeme::AS:
		{
			Type target_type = parse_type(parser, M);
			return AsExpr(
				TokenRange{left.token_range().first, parser.prev_token_idx()},
				M.main->alloc<Expr>(left),
				M.main->alloc<Type>(target_type)
			);
		}

		case Lexeme::QUESTIONMARK:
		{
			assert(!"TODO");
			/*optional<Stmt> on_failure;
			optional<Pattern> error_capture;
			if(try_consume(parser, Lexeme::ELSE))
			{
				if(parser.tok().kind != Lexeme::LEFT_BRACE)
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
			return expr;*/
		}

		default: UNREACHABLE;
	}
}

Expr parse_expr(Parser &parser, Memory M)
{
	return parse_expr(parser, NO_PREVIOUS_OP, M);
}

//--------------------------------------------------------------------
// Patterns
//--------------------------------------------------------------------
static Pattern parse_primary_pattern(Parser &parser, Memory M)
{
	TokenRanger ranger(parser);
	switch(parser.get().kind)
	{
		case Lexeme::LET:
		{
			parser.next();
			IsMutable mutability = IsMutable::NO;
			if(try_consume(parser, Lexeme::MUT))
				mutability = IsMutable::YES;

			string_view ident = consume(parser, Lexeme::IDENTIFIER).text;
			return Pattern(VarPatternUnresolved(ranger.get(), ident, mutability), nullptr);
		}

		case Lexeme::IDENTIFIER:
		{
			Token ident_token = parser.get();
			if(ident_token.text == "_")
			{
				parser.next();
				return Pattern(WildcardPattern(ranger.get()), nullptr);
			}

			Type ctor = parse_type(parser, M);

			bool has_parens = false;
			ListBuilder<PatternArgument> args(M.temp);
			if(try_consume(parser, Lexeme::LEFT_PAREN))
			{
				has_parens = true;
				while(parser.get().kind != Lexeme::RIGHT_PAREN)
				{
					args.append(PatternArgument{
						.pattern = parse_pattern(parser, M),
						.param_name = {},
					});
					if(parser.get().kind != Lexeme::RIGHT_PAREN)
						consume(parser, Lexeme::COMMA);
				}
				consume(parser, Lexeme::RIGHT_PAREN);
			}

			return Pattern(
				ConstructorPattern(
					ranger.get(),
					M.main->alloc<Type>(ctor),
					args.to_array(*M.main),
					has_parens
				),
				nullptr
			);
		}

		default:
			throw_parse_error("Invalid lexeme while parsing pattern", ranger.first, parser);
	}
}

Pattern parse_pattern(Parser &parser, Memory M)
{
	TokenRanger ranger(parser);
	Pattern p = parse_primary_pattern(parser, M);
	for(;;)
	{
		if(try_consume(parser, Lexeme::CIRCUMFLEX))
			p = Pattern(DerefPattern(ranger.get(), M.main->alloc<Pattern>(p)), nullptr);
		else if(try_consume(parser, Lexeme::AMPERSAND))
		{
			IsMutable mutability = IsMutable::NO;
			if(try_consume(parser, Lexeme::MUT))
				mutability = IsMutable::YES;

			p = Pattern(AddressOfPattern(ranger.get(), M.main->alloc<Pattern>(p), mutability), nullptr);
		}
		else if(try_consume(parser, Lexeme::COLON))
			p.provided_type = M.main->alloc<Type>(parse_type(parser, M));
		else
			break;
	}

	return p;
}

//--------------------------------------------------------------------
// Statements
//--------------------------------------------------------------------
static BlockStmt parse_block_stmt(Parser &parser, Memory M)
{
	TokenRanger ranger(parser);
	ListBuilder<Stmt> stmts(M.temp);

	consume(parser, Lexeme::LEFT_BRACE);
	for(;;)
	{
		if(parser.get().kind == Lexeme::RIGHT_BRACE)
			break;

		stmts.append(parse_stmt(parser, M));
	}
	consume(parser, Lexeme::RIGHT_BRACE);

	return BlockStmt(ranger.get(), stmts.to_array(*M.main));
}

Stmt parse_stmt(Parser &parser, Memory M)
{
	TokenRanger ranger(parser);
	switch(parser.get().kind)
	{
		case Lexeme::LET:
		{
			Pattern lhs = parse_pattern(parser, M);
			consume(parser, Lexeme::EQ);
			Expr init_expr = parse_expr(parser, M);
			consume(parser, Lexeme::SEMICOLON);

			return LetStmt(ranger.get(), M.main->alloc<Pattern>(lhs), M.main->alloc<Expr>(init_expr));
		}

		case Lexeme::RETURN:
		{
			parser.next();
			Expr *NULLABLE ret_expr = nullptr;
			if(!try_consume(parser, Lexeme::SEMICOLON))
			{
				ret_expr = M.main->alloc<Expr>(parse_expr(parser, M));
				consume(parser, Lexeme::SEMICOLON);
			}

			return ReturnStmt(ranger.get(), ret_expr);
		}

		case Lexeme::IF:
		{
			parser.next();
			Expr cond = parse_expr(parser, M);
			Stmt then = parse_block_stmt(parser, M);
			Stmt *NULLABLE else_ = nullptr;
			if(try_consume(parser, Lexeme::ELSE))
				else_ = M.main->alloc<Stmt>(parse_block_stmt(parser, M));

			return IfStmt(
				ranger.get(),
				M.main->alloc<Expr>(cond),
				M.main->alloc<Stmt>(then),
				else_
			);
		}

		case Lexeme::WHILE:
		{
			parser.next();
			Expr cond = parse_expr(parser, M);
			Stmt body = parse_block_stmt(parser, M);

			return WhileStmt(ranger.get(), M.main->alloc<Expr>(cond), M.main->alloc<Stmt>(body));
		}

		case Lexeme::MATCH:
		{
			parser.next();
			Expr subject = parse_expr(parser, M);
			consume(parser, Lexeme::LEFT_BRACE);
			ListBuilder<MatchArm> arms(M.temp);
			while(parser.get().kind != Lexeme::RIGHT_BRACE)
			{
				consume(parser, Lexeme::CASE);

				Pattern capture = parse_pattern(parser, M);
				Stmt body = parse_block_stmt(parser, M);

				arms.append(MatchArm(capture, body));
			}
			consume(parser, Lexeme::RIGHT_BRACE);

			return MatchStmt(ranger.get(), M.main->alloc<Expr>(subject), arms.to_array(*M.main));
		}

		case Lexeme::STRUCT:
		{
			StructItem struct_ = parse_struct(parser, StructParseContext::TOP_LEVEL, M);
			return DeclStmt(ranger.get(), M.main->alloc<StructItem>(struct_));
		}

		default:
		{
			Expr expr = parse_expr(parser, M);
			consume(parser, Lexeme::SEMICOLON);
			return ExprStmt(ranger.get(), M.main->alloc<Expr>(expr));
		}
	}
}

//--------------------------------------------------------------------
// Top-level items
//--------------------------------------------------------------------
static GenericParameter parse_type_param(Parser &parser, Memory M)
{
	TokenRanger ranger(parser);
	if(try_consume(parser, Lexeme::LET))
	{
		string_view param_name = consume(parser, Lexeme::IDENTIFIER).text;
		consume(parser, Lexeme::COLON);
		Type param_type = parse_type(parser, M);

		return GenericParameter{
			.range = ranger.get(),
			.name = param_name,
			.kind = GenericValueParameter{
				.type = M.main->alloc<Type>(param_type),
			},
		};
	}
	else
	{
		string_view param_name = consume(parser, Lexeme::IDENTIFIER).text;
		return GenericParameter{
			.range = ranger.get(),
			.name = param_name,
			.kind = GenericTypeParameter(),
		};
	}
}

static FixedArray<GenericParameter>* parse_type_param_list(Parser &parser, Memory M)
{
	ListBuilder<GenericParameter> type_params(M.temp);
	if(try_consume(parser, Lexeme::SINGLE_QUOTE))
	{
		if(try_consume(parser, Lexeme::LEFT_PAREN))
		{
			while(parser.get().kind != Lexeme::RIGHT_PAREN)
			{
				type_params.append(parse_type_param(parser, M));
				if(parser.get().kind != Lexeme::RIGHT_PAREN)
					consume(parser, Lexeme::COMMA);
			}
			consume(parser, Lexeme::RIGHT_PAREN);
		}
		else
			type_params.append(parse_type_param(parser, M));
	}

	return type_params.to_array(*M.main);
}

ProcItem parse_proc(Parser &parser, Memory M)
{
	TokenRanger ranger(parser);
	consume(parser, Lexeme::PROC);

	ProcItem proc;
	proc.name = consume(parser, Lexeme::IDENTIFIER).text;
	if(try_consume(parser, Lexeme::DOT))
	{
		proc.receiver_name = proc.name;
		proc.name = consume(parser, Lexeme::IDENTIFIER).text;
	}

	proc.type_params = parse_type_param_list(parser, M);

	// Parse parameters
	consume(parser, Lexeme::LEFT_PAREN);
	ListBuilder<Parameter> params(M.temp);
	while(parser.get().kind != Lexeme::RIGHT_PAREN)
	{
		TokenRanger param_ranger(parser);
		string_view param_name = consume(parser, Lexeme::IDENTIFIER).text;

		Parameter param;
		if(params.empty() && proc.receiver_name.size())
		{
			if(param_name != "self")
				throw_parse_error("First parameter of a member proc must be named 'self'", param_ranger.first, parser);
		}
		else
		{
			consume(parser, Lexeme::COLON);
			param.type = M.main->alloc<Type>(parse_type(parser, M));

			if(try_consume(parser, Lexeme::EQ))
				param.default_value = M.main->alloc<Expr>(parse_expr(parser, M));
		}

		param.range = param_ranger.get();
		params.append(param);

		if(parser.get().kind != Lexeme::RIGHT_PAREN)
			consume(parser, Lexeme::COMMA);
	}

	consume(parser, Lexeme::RIGHT_PAREN);
	proc.params = params.to_array(*M.main);

	// Parse return type
	if(try_consume(parser, Lexeme::THIN_ARROW))
		proc.ret_type = M.main->alloc<Type>(parse_type(parser, M));
	else
		proc.ret_type = M.main->alloc<Type>(BuiltinType(UNKNOWN_TOKEN_RANGE, BuiltinTypeDef::UNIT));

	// Parse body
	if(!try_consume(parser, Lexeme::SEMICOLON))
		proc.body = M.main->alloc<Stmt>(parse_block_stmt(parser, M));

	proc.range = ranger.get();
	return proc;
}

static void compute_struct_member_indices(StructItem *struct_)
{
	optional<int> prev_var_member_idx;
	optional<int> prev_case_member_idx;
	for(size_t member_idx = 0; member_idx < struct_->members->count; member_idx++)
	{
		Member const &m = struct_->members->items[member_idx];
		m | match
		{
			[&](VarMember const&)
			{
				struct_->num_var_members += 1;
				if(struct_->first_case_member_idx)
				{
					if(not struct_->first_trailing_var_member_idx)
						struct_->first_trailing_var_member_idx = member_idx;
				}
				else
				{
					if(not struct_->first_initial_var_member_idx)
						struct_->first_initial_var_member_idx = member_idx;
				}

				if(prev_var_member_idx)
					std::get<VarMember>(struct_->members->items[*prev_var_member_idx]).next_var_member_idx = member_idx;
				prev_var_member_idx = member_idx;
			},
			[&](CaseMember const&)
			{
				struct_->num_case_members += 1;
				if(not struct_->first_case_member_idx)
					struct_->first_case_member_idx = member_idx;

				if(prev_case_member_idx)
					std::get<CaseMember>(struct_->members->items[*prev_case_member_idx]).next_case_member_idx = member_idx;
				prev_case_member_idx = member_idx;
			},
			[&](StructMember const&) {},
		};
	}
}

StructItem parse_struct(Parser &parser, StructParseContext struct_context, Memory M)
{
	TokenRanger ranger(parser);

	if(struct_context == StructParseContext::TOP_LEVEL)
		consume(parser, Lexeme::STRUCT);

	StructItem struct_;
	struct_.name = consume(parser, Lexeme::IDENTIFIER).text;
	struct_.type_params = parse_type_param_list(parser, M);

	// Parse members
	ListBuilder<Member> members(M.temp);
	if(try_consume(parser, Lexeme::LEFT_BRACE))
	{
		while(parser.get().kind != Lexeme::RIGHT_BRACE)
		{
			if(try_consume(parser, Lexeme::CASE))
			{
				bool is_implicit = (bool)try_consume(parser, Lexeme::IMPLICIT);
				StructItem *case_member = M.main->alloc<StructItem>(
					parse_struct(parser, StructParseContext::CASE_MEMBER, M)
				);
				case_member->is_implicit = is_implicit;
				members.append(CaseMember(case_member, nullopt));
				if(parser.get().kind != Lexeme::RIGHT_BRACE)
					consume(parser, Lexeme::COMMA);
			}
			else if(try_consume(parser, Lexeme::STRUCT))
			{
				StructItem *struct_member = M.main->alloc<StructItem>(
					parse_struct(parser, StructParseContext::INLINE, M)
				);
				members.append(StructMember(struct_member));
				if(parser.get().kind != Lexeme::RIGHT_BRACE)
					consume(parser, Lexeme::COMMA);
			}
			else
			{
				TokenRanger member_ranger(parser);
				consume(parser, Lexeme::IDENTIFIER);
				consume(parser, Lexeme::COLON);

				Parameter member;
				member.type = M.main->alloc<Type>(parse_type(parser, M));
				if(try_consume(parser, Lexeme::EQ))
					member.default_value = M.main->alloc<Expr>(parse_expr(parser, M));

				// Comma needs to be part of the member's token range so that trailing comments are
				// properly attached
				if(parser.get().kind != Lexeme::RIGHT_BRACE)
					consume(parser, Lexeme::COMMA);

				member.range = member_ranger.get();
				members.append(VarMember(member, nullopt));
			}
		}
		consume(parser, Lexeme::RIGHT_BRACE);
	}
	else
	{
		struct_.ctor_without_parens = true;
		if(struct_context == StructParseContext::TOP_LEVEL)
			consume(parser, Lexeme::SEMICOLON);
	}

	if(struct_context == StructParseContext::CASE_MEMBER)
		struct_.is_case_member = true;

	struct_.members = members.to_array(*M.main);
	struct_.range = ranger.get();
	compute_struct_member_indices(&struct_);

	return struct_;
}

TopLevelItem parse_top_level_item(Parser &parser, Memory M)
{
	switch(parser.get().kind)
	{
		case Lexeme::PROC:
			return parse_proc(parser, M);

		case Lexeme::STRUCT:
			return parse_struct(parser, StructParseContext::TOP_LEVEL, M);

		case Lexeme::TYPEALIAS:
		{
			TokenRanger ranger(parser);
			parser.next();

			AliasItem alias;
			alias.name = consume(parser, Lexeme::IDENTIFIER).text;

			alias.type_params = parse_type_param_list(parser, M);
			consume(parser, Lexeme::EQ);
			alias.aliased_type = M.main->alloc<Type>(parse_type(parser, M));
			consume(parser, Lexeme::SEMICOLON);

			alias.range = ranger.get();
			return alias;
		} break;

		case Lexeme::EXTERN:
		{
			TokenRanger ranger(parser);
			parser.next();

			if(parser.get().kind == Lexeme::PROC)
			{
				ProcItem proc = parse_proc(parser, M);
				proc.is_extern = true;
				proc.range = ranger.get();
				return proc;
			}

			if(parser.get().kind == Lexeme::STRUCT)
			{
				StructItem struct_ = parse_struct(parser, StructParseContext::TOP_LEVEL, M);
				struct_.is_extern = true;
				struct_.range = ranger.get();
				return struct_;
			}

			throw_parse_error("Expected proc or struct declaration after extern", ranger.first, parser);
		}

		default:
			throw_parse_error("Invalid token while parsing top-level item: "s + str(parser.get().kind), parser.token_idx(), parser);
	}
}

Module parse_module(string_view source, Memory M)
{
	// Without this clang (incorrectly) complains that #include "semantics/module.hpp" is not used
	(void)sizeof(SemaModule);

	Module mod{
		.parser = Parser(source),
		.items = ListBuilder<TopLevelItem>(*M.main),
	};

	while(mod.parser.get().kind != Lexeme::END)
	{
		TopLevelItem item = parse_top_level_item(mod.parser, M);
		mod.items.append(item);
	}

	return mod;
}

//--------------------------------------------------------------------
// Working with comments
//--------------------------------------------------------------------
static void update_comment_target(CommentTarget &current, CommentTarget new_)
{
	if(current == CommentTarget::NONE or current == new_)
		current = new_;
	else
		current = CommentTarget::AMBIGUOUS;
}

static bool is_multiline_comment(Comment const &comment)
{
	return comment.start_loc.line < comment.end_loc.line;
}

vector<Comment> parse_comments(PrecedingText text)
{
	vector<Comment> comments;
	Lexer lexer(text.text, text.start_loc);
	skip_whitespace(lexer);
	while(optional<Comment> comment = try_read_comment(lexer))
	{
		comments.push_back(*comment);
		skip_whitespace(lexer);
	}

	Comment const *prev_comment = nullptr;
	for(Comment &comment: comments)
	{
		comment.target = CommentTarget::NONE;
		if(prev_comment)
		{
			if(comment.start_loc.line == prev_comment->end_loc.line)
			{
				if(prev_comment->target != CommentTarget::NONE)
				{
					if(is_multiline_comment(comment))
						comment.target = CommentTarget::AMBIGUOUS;
					else
						comment.target = prev_comment->target;
				}
			}
			else if(comment.start_loc.line == prev_comment->end_loc.line + 1)
			{
				if(prev_comment->target == CommentTarget::PREVIOUS_TOKEN)
					comment.target = CommentTarget::AMBIGUOUS;
				else
					comment.target = prev_comment->target;
			}
		}
		else if(comment.start_loc.line == text.start_loc.line)
		{
			// Comments that start on the same line as the PrecedingText are usually attached to the
			// previous token.
			//
			//   let a = 0; // Attaches to the previous token
			//
			// Two exceptions:
			// - Multi-line comments in this scenario are always AMBIGUOUS
			// - If the PrecedingText starts at the beginning of the file, then there is no previous
			//   token

			bool comment_starts_at_beginning_of_file =
				text.start_loc.line == 1 and text.start_loc.col == 1
				and comment.start_loc.col == text.start_loc.col;

			if(not comment_starts_at_beginning_of_file)
			{
				if(is_multiline_comment(comment))
					comment.target = CommentTarget::AMBIGUOUS;
				else
					comment.target = CommentTarget::PREVIOUS_TOKEN;
			}
		}

		prev_comment = &comment;
	}

	Comment const *next_comment = nullptr;
	for(Comment &comment: comments | std::ranges::views::reverse)
	{
		if(next_comment)
		{
			if(next_comment->start_loc.line - comment.end_loc.line <= 1)
			{
				if(comment.target != CommentTarget::PREVIOUS_TOKEN)
					update_comment_target(*comment.target, *next_comment->target);
			}
		}
		else if(text.end_loc.line - comment.end_loc.line <= 1)
		{
			// If we get here, the comment is on the last line of the PrecedingText. Usually, this
			// means it will be attached to the next Token.
			// However, if the comment is also on the *first* line of the PrecedingText, then we
			// keep it attached to the previous Token. This ensures that trailing comments are
			// attached to the previous Token on the same line, and not the next Token on the
			// following line.
			//
			//   let a = 0; // Attached to previous Token
			//   let b = 0;
			//
			// Still, we only do this for single line comments.
			//
			//   let a = 0; /*
			//      still ambiguous
			//   */
			//   let b = 0;
			bool keep_attached_to_previous_token =
				comment.start_loc.line == text.start_loc.line and
				not is_multiline_comment(comment);

			if(comment.target == CommentTarget::PREVIOUS_TOKEN and keep_attached_to_previous_token) {
				// do nothing
			}
			else
				update_comment_target(*comment.target, CommentTarget::NEXT_TOKEN);
		}

		next_comment = &comment;
	}

	return comments;
}

vector<string_view> extract_clean_lines(Comment comment)
{
	vector<string_view> lines;
	switch(comment.style)
	{
		case CommentStyle::BLOCK:
		{
			for(string_view line: split_lines(comment.text))
			{
				/* In block comments like this,
				 * remove any leading star from the
				 * beginning of a line.
				 */
				size_t leading_star_pos = comment.start_loc.col;
				if(leading_star_pos < line.length() and line[leading_star_pos] == '*')
				{
					string_view indent = line.substr(0, comment.start_loc.col - 1);
					if(is_whitespace(indent))
						line.remove_prefix(leading_star_pos + 1);
				}
				lines.push_back(trimmed(line));
			}
		} break;
		case CommentStyle::LINE:
		{
			lines.push_back(trimmed(comment.text));
		} break;
	}

	return lines;
}

//--------------------------------------------------------------------
// OptionGatherer
//--------------------------------------------------------------------
static std::pair<string_view, string_view> parse_option(string_view line)
{
	Lexer lexer(line);
	optional<string_view> name = try_read_identifier(lexer);
	if(not name)
		throw std::runtime_error("Expected the name of a directive");

	skip_whitespace(lexer);
	consume(lexer, ":");
	string_view value = trimmed(lexer.remaining_string());
	return std::pair(*name, value);
}

void parse_options_from_comment(Comment const &comment, OptionSet &options)
{
	if(comment.kind == CommentKind::SPECIAL)
	{
		vector<string_view> lines = extract_clean_lines(comment);
		for(string_view line: lines)
		{
			if(not is_whitespace(line))
			{

				auto res = options.opts.insert(parse_option(line));
				if(not res.second)
					throw std::runtime_error("Option already defined: "s + res.first->first);
			}
		}
	}
}
