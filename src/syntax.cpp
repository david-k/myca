#include <charconv>
#include <ranges>
#include <sstream>
#include <unordered_map>

#include "syntax.hpp"
#include "semantics.hpp"
#include "utils.hpp"


using std::unordered_map;
using namespace std::string_literals;
using namespace std::string_view_literals;


//==============================================================================
// Printing
//==============================================================================
static constexpr int INDENT_WIDTH = 4;

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

		case Lexeme::TYPEALIAS: return "TYPEALIAS";
		case Lexeme::EXTERN: return "EXTERN";

		case Lexeme::SIZE_OF: return "SIZE_OF";
		case Lexeme::MAKE: return "MAKE";

		case Lexeme::END: return "END_OF_FILE";
	}

	UNREACHABLE;
}

string_view str(BuiltinTypeDef t)
{
	switch(t)
	{
		case BuiltinTypeDef::NEVER: return "Never";
		case BuiltinTypeDef::UNIT: return "Unit";
		case BuiltinTypeDef::BOOL: return "bool";
		case BuiltinTypeDef::I8: return "i8";
		case BuiltinTypeDef::U8: return "u8";
		case BuiltinTypeDef::I32: return "i32";
		case BuiltinTypeDef::U32: return "u32";
		case BuiltinTypeDef::ISIZE: return "isize";
		case BuiltinTypeDef::USIZE: return "usize";
	}

	UNREACHABLE;
}

string_view name_of(Path const &path, Module const *mod)
{
	Token const &tok = mod->parser.token_at(path.range.first);
	return mod->parser.get_text(tok.span);
}

string_view name_of(Parameter const &param, Module const *mod)
{
	Token const &tok = mod->parser.token_at(param.range.first);
	assert(tok.kind == Lexeme::IDENTIFIER);
	return mod->parser.get_text(tok.span);
}

void print_type_args(FixedArray<Type> const *type_args, Module const &mod, std::ostream &os)
{
	if(type_args->count)
	{
		os << "'(";
		print(type_args->head(), mod, os);
		for(Type const &arg: type_args->tail())
		{
			os << ", ";
			print(arg, mod, os);
		}
		os << ")";
	}
}

void print(Path const &path, Module const &mod, std::ostream &os)
{
	os << name_of(path, &mod);
	print_type_args(path.type_args, mod, os);

	if(path.child)
	{
		os << ".";
		print(*path.child, mod, os);
	}
}

static void print_proc_type(FixedArray<Type> const *params, Type const *ret_type, Module const &mod, std::ostream &os)
{
	os << "proc(";
	if(params->count)
	{
		print(params->head(), mod, os);
		for(Type const &param: params->tail())
		{
			os << ", ";
			print(param, mod, os);
		}
	}
	os << ") -> ";
	print(*ret_type, mod, os);
}

void print_decl_container(DeclContainerInst cont, Module const &mod, std::ostream &os)
{
	if(cont.decl_parent())
	{
		print_decl_container(*cont.decl_parent(), mod, os);
		os << ".";
	}

	cont | match
	{
		[&](StructInstance *inst)
		{
			os << inst->struct_()->name;
		},
		[&](ProcInstance *inst)
		{
			os << inst->proc()->name;
		},
	};

	print_type_args(cont.type_args().args, mod, os);
}

void print(VarType const &var, std::ostream &os)
{
	var | match
	{
		[&](TypeParameterVar v)
		{
			os << v.def->name;
		},
		[&](TypeDeductionVar v)
		{
			os << "?_" << v.id;
		},
	};
}

void print(Type const &type, Module const &mod, std::ostream &os)
{
	type | match
	{
		[&](BuiltinType const &t)
		{
			os << str(t.builtin);
		},
		[&](KnownIntType const &t)
		{
			os << "$KnownInt(" << t.low << "," << t.high << ")";
		},
		[&](PointerType const &t)
		{
			if(t.kind == PointerType::SINGLE)
				os << "^";
			else
				os << "[^]";

			if(t.mutability == IsMutable::YES)
				os << "mut ";

			print(*t.pointee, mod, os);
		},
		[&](ProcType const &t)
		{
			print_proc_type(t.inst->params, t.inst->ret, mod, os);
		},
		[&](ProcTypeUnresolved const &t)
		{
			print_proc_type(t.params, t.ret, mod, os);
		},
		[&](StructType const &t)
		{
			print_decl_container(t.inst, mod, os);
		},
		[&](UnionTypeUnresolved const &t)
		{
			print(t.alternatives->head(), mod, os);
			for(Type const &alt: t.alternatives->tail())
			{
				os << " | ";
				print(alt, mod, os);
			}
		},
		[&](UnionType const &t)
		{
			vector<Type const*> const &alts = t.inst->alternatives();
			if(alts.size() == 1)
			{
				os << "(";
				print(*alts.front(), mod, os);
				os << " |)";
			}
			else
			{
				print(*alts.front(), mod, os);
				for(Type const *alt: t.inst->alternatives() | std::views::drop(1))
				{
					os << " | ";
					print(*alt, mod, os);
				}
			}
		},
		[&](Path const &path)
		{
			print(path, mod, os);
		},
		[&](VarType const &t)
		{
			print(t, os);
		},
		[&](InlineStructType const&)
		{
			assert(!"[TODO] print(Type): InlineStructType");
		},
	};
}

string str(Type const &type, Module const &mod)
{
	std::stringstream ss;
	print(type, mod, ss);
	return std::move(ss).str();
}

void print(Argument const &arg, Module const &mod, std::ostream &os)
{
	if(arg.name.size())
		os << "." << arg.name << "=";

	print(arg.expr, mod, os);
}

void print(Expr const &expr, Module const &mod, std::ostream &os)
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
			switch(e.kind)
			{
				case StringLiteralKind::C: os << "c\"" << e.value << '"'; break;
			}
		},
		[&](UnaryExpr const &e)
		{
			os << "(";
			switch(e.op)
			{
				case UnaryOp::NOT: os << "not "; break;
				case UnaryOp::NEG: os << "-"; break;
			}
			print(*e.sub, mod, os);
			os << ")";
		},
		[&](BinaryExpr const &e)
		{
			os << "(";
			print(*e.left, mod, os);
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
			print(*e.right, mod, os);
			os << ")";
		},
		[&](AddressOfExpr const &e)
		{
			os << "&";
			if(e.mutability == IsMutable::YES)
				os << "mut ";

			os << "(";
			print(*e.object, mod, os);
			os << ")";
		},
		[&](DerefExpr const &e)
		{
			os << "(";
			print(*e.addr, mod, os);
			os << ")^";
		},
		[&](IndexExpr const &e)
		{
			os << "(";
			print(*e.addr, mod, os);
			os << ")^";
		},
		[&](MemberAccessExpr const &e)
		{
			print(*e.object, mod, os);
			os << "." << e.member;
		},
		[&](AssignmentExpr const &e)
		{
			os << "(";
			print(*e.lhs, mod, os);
			os << " = ";
			print(*e.rhs, mod, os);
			os << ")";
		},
		[&](AsExpr const &e)
		{
			os << "(";
			print(*e.src_expr, mod, os);
			os << " as ";
			print(*e.target_type, mod, os);
			os << ")";
		},
		[&](ConstructorExpr const &e)
		{
			print(*e.ctor, mod, os);
		},
		[&](ProcExpr const &e)
		{
			os << e.inst->proc()->name;
			print_type_args(e.inst->type_args().args, mod, os);
		},
		[&](CallExpr const &e)
		{
			print(*e.callable, mod, os);
			os << "(";
			if(e.args->count)
			{
				print(e.args->head(), mod, os);
				for(Argument const &arg: e.args->tail())
				{
					os << ", ";
					print(arg, mod, os);
				}
			}
			os << ")";
		},
		[&](SizeOfExpr const &e)
		{
			os << "size_of(";
			print(*e.subject, mod, os);
			os << ")";
		},
		[&](MakeExpr const &e)
		{
			os << "make ";
			print(*e.init, mod, os);
			os << " @ ";
			print(*e.addr, mod, os);
		},
		[&](UnionInitExpr const &e)
		{
			os << "$InitUnion'(";
			print(*e.type, mod, os);
			os << ")(";
			print(*e.alt_expr, mod, os);
			os << ")";
		},
		[&](VarExpr const &e)
		{
			os << e.var->name;
		},
		[&](Path const &p)
		{
			print(p, mod, os);
		}
	};
}

void print(PatternArgument const &arg, Module const &mod, std::ostream &os)
{
	if(arg.param_name.size())
		os << "." << arg.param_name << "=";

	print(arg.pattern, mod, os);
}

void print(Pattern const &pattern, Module const &mod, std::ostream &os)
{
	pattern | match
	{
		[&](VarPatternUnresolved const &p)
		{
			if(p.mutability == IsMutable::YES)
				os << "mut ";
			os << p.name;
		},
		[&](VarPattern const &p)
		{
			if(p.var->mutability == IsMutable::YES)
				os << "mut ";
			os << p.var->name;

			if(p.var->type and not pattern.provided_type)
			{
				os << ": ";
				print(*p.var->type, mod, os);
			}
		},
		[&](DerefPattern const &p)
		{
			print(*p.sub, mod, os);
			os << " ^";
		},
		[&](AddressOfPattern const &p)
		{
			print(*p.sub, mod, os);
			os << " &";
			if(p.mutability == IsMutable::YES)
				os << "mut";
		},
		[&](ConstructorPattern const &p)
		{
			print(*p.ctor, mod, os);
			os << "(";
			if(p.args->count)
			{
				print(p.args->head(), mod, os);
				for(PatternArgument const &arg: p.args->tail())
				{
					os << ", ";
					print(arg, mod, os);
				}
			}
			os << ")";
		},
		[&](WildcardPattern const&)
		{
			os << "_";
		}
	};

	if(pattern.provided_type)
	{
		os << ": ";
		print(*pattern.provided_type, mod, os);
	}
}

void print(StructItem const &struct_, bool is_top_level, int indent, Module const &mod, std::ostream &os);

void print(Stmt const &stmt, int indent, Module const &mod, std::ostream &os)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ');
			os << "let ";
			print(*s.lhs, mod, os);
			if(s.init_expr)
			{
				os << " = ";
				print(*s.init_expr, mod, os);
			}

			os << ";";
		},
		[&](ExprStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ');
			print(*s.expr, mod, os);
			os << ";";
		},
		[&](BlockStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ') << "{\n";
			for(Stmt const &child_stmt: *s.stmts)
			{
				print(child_stmt, indent+1, mod, os);
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
				print(*s.ret_expr, mod, os);
			}
			os << ";";
		},
		[&](IfStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ') << "if ";
			print(*s.condition, mod, os);
			os << "\n";
			print(*s.then, indent, mod, os);
			if(s.else_)
			{
				os << "\n" << string(indent*INDENT_WIDTH, ' ') << "else\n";
				print(*s.else_, indent, mod, os);
			}
		},
		[&](WhileStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ') << "while ";
			print(*s.condition, mod, os);
			os << "\n";
			print(*s.body, indent, mod, os);
		},
		[&](DeclStmt const &s)
		{
			print(*s.item, true, indent, mod, os);
		},
		[&](MatchStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ') << "match ";
			print(*s.expr, mod, os);
			os << "\n";
			os << string(indent*INDENT_WIDTH, ' ') << "{\n";
			for(MatchArm const &arm: *s.arms)
			{
				os << string((indent+1)*INDENT_WIDTH, ' ') << "case ";
				print(arm.capture, mod, os);
				os << "\n";
				print(arm.stmt, indent+1, mod, os);
				os << "\n";
			}
			os << string(indent*INDENT_WIDTH, ' ') << "}";
		},
	};
}

void print(Stmt const &stmt, Module const &mod, std::ostream &os)
{
	print(stmt, 0, mod, os);
}

void print(FixedArray<TypeParameter> const *type_params, std::ostream &os)
{
	if(type_params->count)
	{
		os << "'(";
		os << type_params->head().name;
		for(TypeParameter const &param: type_params->tail())
			os << ", " << param.name;
		os << ")";
	}
}

void print(Parameter const &param, Module const &mod, std::ostream &os)
{
	os << name_of(param, &mod);
	if(param.type)
	{
		os << ": ";
		print(*param.type, mod, os);
	}
	if(param.default_value)
	{
		os << " = ";
		if(Expr *default_value = param.default_value.try_get_expr())
			print(*default_value, mod, os);
		else
			os << "<EXPR_PENDING>";
	}
}

void print(Member const &member, int indent, Module const &mod, std::ostream &os)
{
	member | match
	{
		[&](VarMember const &var_member)
		{
			os << string(indent*INDENT_WIDTH, ' ') << name_of(var_member.var, &mod) << ": ";
			print(*var_member.var.type, mod, os);
			if(var_member.var.default_value)
			{
				os << " = ";
				if(Expr *default_value = var_member.var.default_value.try_get_expr())
					print(*default_value, mod, os);
				else
					os << "<EXPR_PENDING>";
			}
		},
		[&](CaseMember case_member)
		{
			print(*case_member.struct_, false, indent, mod, os);
		},
		[&](StructMember struct_member)
		{
			print(*struct_member.struct_, true, indent, mod, os);
		},
	};

	os << ",";
}

void print(StructItem const &struct_, bool is_top_level, int indent, Module const &mod, std::ostream &os)
{
	if(struct_.is_extern)
		os << string(indent*INDENT_WIDTH, ' ') << "extern ";

	if(is_top_level)
		os << string(indent*INDENT_WIDTH, ' ') << "struct ";
	else
		os << string(indent*INDENT_WIDTH, ' ') << "case ";

	os << struct_.name;
	print(struct_.type_params, os);

	if(struct_.members->count)
	{
		os << "\n";
		os << string(indent*INDENT_WIDTH, ' ') << "{\n";
		print(struct_.members->head(), indent+1, mod, os);
		for(Member const &member: struct_.members->tail())
		{
			os << "\n";
			print(member, indent+1, mod, os);
		}
		os << "\n" << string(indent*INDENT_WIDTH, ' ') << "}";
	}
	else
		os << " {}";
}

void print(TopLevelItem const &item, Module const &mod, std::ostream &os)
{
	item | match
	{
		[&](ProcItem const &proc)
		{
			if(proc.is_extern)
				os << "extern ";

			os << "proc " << proc.name;
			print(proc.type_params, os);
			os << "(";
			if(proc.params->count)
			{
				print(proc.params->head(), mod, os);
				for(Parameter const &param: proc.params->tail())
				{
					os << ", ";
					print(param, mod, os);
				}
			}
			os << ")";

			if(proc.ret_type)
			{
				os << " -> ";
				print(*proc.ret_type, mod, os);
			}

			if(proc.body)
			{
				os << "\n";
				print(*proc.body, mod, os);
			}
			else
				os << ";";
		},
		[&](StructItem const &struct_)
		{
			print(struct_, true, 0, mod, os);
		},
		[&](AliasItem const &alias)
		{
			os << "typealias " << alias.name;
			print(alias.type_params, os);
			os << " = ";
			print(*alias.aliased_type, mod, os);
			os << ";";
		},
	};
}

void print(Module const &mod, std::ostream &os)
{
	for(TopLevelItem const &item: to_range(mod.items.list()))
	{
		print(item, mod, os);
		os << "\n\n";
	}
}


//==============================================================================
// Misc
//==============================================================================
TokenRange token_range_of(Type const &type)
{
	return type | match
	{
		[](VarType const &t)
		{
			return t | match
			{
				[](TypeParameterVar var) { return var.range; },
				[](TypeDeductionVar) { return UNKNOWN_TOKEN_RANGE; },
			};
		},
		[](KnownIntType const&) { return UNKNOWN_TOKEN_RANGE; },
		[](auto const &t) { return t.range; },
	};
}

TokenRange token_range_of(Pattern const &pattern)
{
	return pattern | match
	{
		[](auto const &p) { return p.range; },
	};
}

TokenRange token_range_of(Expr const &expr)
{
	return expr | match
	{
		[](auto const &p) { return p.range; },
	};
}


size_t ProcType::param_count() const
{
	return callable | match
	{
		[&](ProcInstance *proc) { return proc->get_param_count(); },
		[&](StructInstance *struct_) { return struct_->get_param_count(); },
	};
}

Type* ProcType::param_type_at(size_t idx) const
{
	return callable | match
	{
		[&](ProcInstance *proc) { return proc->get_param_type_at(idx); },
		[&](StructInstance *struct_) { return struct_->get_ctor_param_type_at(idx); },
	};
}

string_view ProcType::param_name_at(size_t idx) const
{
	return callable | match
	{
		[&](ProcInstance *proc) { return proc->get_param_name_at(idx); },
		[&](StructInstance *struct_) { return struct_->get_ctor_param_name_at(idx); },
	};
}

DefaultValueExpr ProcType::param_default_value_at(size_t idx) const
{
	return callable | match
	{
		[&](ProcInstance *proc) { return proc->get_param_default_value(idx); },
		[&](StructInstance *struct_) { return struct_->get_ctor_param_default_value(idx); },
	};
}


//==============================================================================
// Lexing
//==============================================================================
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

void consume(Lexer &lexer, string_view expected_str)
{
	if(not try_consume(lexer, expected_str))
		throw LexingError("Unexpected input");
}


static bool is_alphabetic(char ch)
{
	return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
}

static bool is_digit(char ch)
{
	return ch >= '0' && ch <= '9';
}


static bool is_whitespace(char ch)
{
	return ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r';
}

void skip_whitespace(Lexer &lexer)
{
	while(lexer.has_more() && is_whitespace(lexer.get()))
		lexer.advance();
}

static bool skip_comment(Lexer &lexer)
{
	if(try_consume(lexer, "/*"))
	{
		while(lexer.has_more())
		{
			if(try_consume(lexer, "*/"))
				return true;

			lexer.advance();
		}

		throw LexingError{"Unclosed multi-line comment"};
	}

	if(try_consume(lexer, "//"))
	{
		while(lexer.has_more())
		{
			if(try_consume(lexer, "\n"))
				return true;

			lexer.advance();
		}
	}

	return false;
}

static void skip_whitespace_and_comments(Lexer &lexer)
{
	do skip_whitespace(lexer);
	while(skip_comment(lexer));
}

optional<uint64_t> try_read_number(Lexer &lexer)
{
	size_t start_pos = lexer.local_byte_pos();
	while(lexer.has_more() && is_digit(lexer.get()))
		lexer.advance();

	string_view integer_str = lexer.substr_from(start_pos);
	if(integer_str.empty())
		return nullopt;

	uint64_t integer;
	std::from_chars_result result = std::from_chars(integer_str.begin(), integer_str.end(), integer);
	assert(result.ec == std::errc());
	assert(result.ptr == integer_str.end());

	return integer;
}

optional<string_view> try_read_identifier(Lexer &lexer)
{
	if(!lexer.has_more())
		return nullopt;

	char ch = lexer.get();
	if(ch == '_' || is_alphabetic(ch))
	{
		size_t id_start_pos = lexer.local_byte_pos();
		do
			lexer.advance();
		while(lexer.has_more() && (lexer.get() == '_' || is_alphabetic(lexer.get()) || is_digit(lexer.get())));

		return lexer.substr_from(id_start_pos);
	}

	return nullopt;
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
		case '!': lexer.advance(); return Lexeme::BANG;
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
	if(optional<uint64_t> int_val = try_read_number(lexer))
	{
		return Token{
			.kind = Lexeme::INT_LITERAL,
			.value = {.int_val = *int_val},
			.span = {loc_begin, lexer.location()},
		};
	}

	if(optional<Lexeme> tok_kind = try_read_punctuation(lexer))
	{
		return Token{
			.kind = *tok_kind,
			.span = {loc_begin, lexer.location()},
		};
	}

	// C string literals
	if(try_consume(lexer, "c\""))
	{
		size_t string_start_pos = lexer.local_byte_pos();
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
			.value = {.str_val = string_literal},
			.span = {loc_begin, lexer.location()},
		};
	}

	if(optional<string_view> id = try_read_identifier(lexer))
	{
		if(auto keyword_it = KEYWORDS.find(*id); keyword_it != KEYWORDS.end())
		{
			return Token{
				.kind = keyword_it->second,
				.span = {loc_begin, lexer.location()},
			};
		}

		return Token{
			.kind = Lexeme::IDENTIFIER,
			.value = {.str_val = *id},
			.span = {loc_begin, lexer.location()},
		};
	}

	throw LexingError("Invalid character: "s + lexer.get());
}


//==============================================================================
// Parsing
//==============================================================================
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

static Token const& consume(Parser &parser, Lexeme kind)
{
	Token const &actual_token = parser.get();
	if(actual_token.kind != kind)
		throw ParseError(str(actual_token.span.begin) + ": Expected "s + str(kind) + ", got " + str(parser.get().kind));

	return parser.next();
}

static string_view consume_identifier(Parser &parser)
{
	Token const &tok = consume(parser, Lexeme::IDENTIFIER);
	return tok.value.str_val;
}

static optional<Token> try_consume(Parser &parser, Lexeme kind)
{
	if(parser.get().kind != kind)
		return std::nullopt;

	return parser.next();
}

[[noreturn]] static void throw_parse_error(string const &msg, TokenIdx tok_idx, Parser &parser)
{
	Token const &tok = parser.token_at(tok_idx);
	throw ParseError("|" + str(tok.span.begin) + "| error: " + msg);
}


//--------------------------------------------------------------------
// Types
//--------------------------------------------------------------------
static Type parse_prefix_type(Parser &parser, Memory M, bool parse_full_path = true);
static Type parse_union_type(Parser &parser, Memory M);

enum class StructParseContext
{
	TOP_LEVEL,
	CASE_MEMBER,
	INLINE,
};

static StructItem parse_struct(Parser &parser, StructParseContext struct_context, Memory M);

static FixedArray<Type>* parse_type_arg_list(Parser &parser, Memory M)
{
	ListBuilder<Type> type_args(M.temp);
	if(try_consume(parser, Lexeme::LEFT_PAREN))
	{
		while(parser.get().kind != Lexeme::RIGHT_PAREN)
		{
			type_args.append(parse_type(parser, M));
			if(parser.get().kind != Lexeme::RIGHT_PAREN)
				consume(parser, Lexeme::COMMA);
		}
		consume(parser, Lexeme::RIGHT_PAREN);
	}
	else
	{
		// By setting `parse_full_path` to `false` we parse `Option'T.Some` as `Option'(T).Some` and
		// not as `Option'(T.Some)`, which I feel is more natural in practice
		type_args.append(parse_prefix_type(parser, M, false));
	}

	return type_args.to_array(*M.main);
}

static Path parse_path(Parser &parser, Memory M)
{
	TokenRanger ranger(parser);
	consume(parser, Lexeme::IDENTIFIER);

	Path path;
	if(try_consume(parser, Lexeme::SINGLE_QUOTE))
		path.type_args = parse_type_arg_list(parser, M);
	else
		path.type_args = alloc_fixed_array<Type>(0, *M.main);

	if(try_consume(parser, Lexeme::DOT))
		path.child = M.main->alloc<Path>(parse_path(parser, M));

	path.range = ranger.get();
	return path;
}

static Type parse_prefix_type(Parser &parser, Memory M, bool parse_full_path)
{
	TokenRanger ranger(parser);
	Token const &tok = parser.next();
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
					.type_args = alloc_fixed_array<Type>(0, *M.main),
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
			consume(parser, Lexeme::CIRCUMFLEX);
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

		case Lexeme::QUESTIONMARK:
		{
			Type optional_type = parse_prefix_type(parser, M);
			FixedArray<Type> *type_args = alloc_fixed_array<Type>(1, *M.main);
			type_args->items[0] = optional_type;
				
			return Path(ranger.get(), type_args);
		}

		case Lexeme::BANG:
		{
			Type err_type = parse_prefix_type(parser, M);
			FixedArray<Type> *type_args = alloc_fixed_array<Type>(1, *M.main);
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
		FixedArray<Type> *type_args = alloc_fixed_array<Type>(2, *M.main);
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
	Expr parse_prefix_expr(Parser &parser, Memory M);
	Expr parse_infix_expr(Parser &parser, Expr left, Memory M);

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

	Expr parse_expr(Parser &parser, OperatorInfo prev_op, Memory M)
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

	Expr parse_prefix_expr(Parser &parser, Memory M)
	{
		TokenRanger ranger(parser);
		Token const &tok = parser.next();
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
				return IntLiteralExpr(ranger.get(), tok.value.int_val);

			case Lexeme::TRUE:
				return BoolLiteralExpr(ranger.get(), true);

			case Lexeme::FALSE:
				return BoolLiteralExpr(ranger.get(), false);

			case Lexeme::C_STRING_LITERAL:
				return StringLiteralExpr(ranger.get(), StringLiteralKind::C, tok.value.str_val);

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
				Expr sub_expr = parse_expr(parser, *get_operator_info(Lexeme::UNARY_MINUS), M);
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

	Expr parse_infix_expr(Parser &parser, Expr left, Memory M)
	{
		Token tok = parser.next();
		switch(tok.kind)
		{
			case Lexeme::DOT:
			{
				string_view member_name = consume_identifier(parser);
				return MemberAccessExpr(
					TokenRange{token_range_of(left).first, parser.prev_token_idx()},
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
					TokenRange{token_range_of(left).first, parser.prev_token_idx()},
					M.main->alloc<Expr>(left),
					M.main->alloc<Expr>(right),
					BinaryOp(tok.kind)
				);
			}

			case Lexeme::COLON_EQ:
			{
				Expr right = parse_expr(parser, *get_operator_info(tok.kind), M);
				return AssignmentExpr(
					TokenRange{token_range_of(left).first, parser.prev_token_idx()},
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
						arg.name = consume_identifier(parser);

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
					TokenRange{token_range_of(left).first, parser.prev_token_idx()},
					M.main->alloc<Expr>(left),
					args.to_array(*M.main)
				);
			}

			case Lexeme::CIRCUMFLEX:
				return DerefExpr(
					TokenRange{token_range_of(left).first,
					parser.prev_token_idx()},
					M.main->alloc<Expr>(left)
				);

			case Lexeme::LEFT_BRACKET:
			{
				Expr index = parse_expr(parser, NO_PREVIOUS_OP, M);
				consume(parser, Lexeme::RIGHT_BRACKET);
				return IndexExpr(
					TokenRange{token_range_of(left).first, parser.prev_token_idx()},
					M.main->alloc<Expr>(left),
					M.main->alloc<Expr>(index)
				);
			}

			case Lexeme::AS:
			{
				Type target_type = parse_type(parser, M);
				return AsExpr(
					TokenRange{token_range_of(left).first, parser.prev_token_idx()},
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

} // anonymous namespace


Expr parse_expr(Parser &parser, Memory M)
{
	return parse_expr(parser, NO_PREVIOUS_OP, M);
}


//--------------------------------------------------------------------
// Patterns
//--------------------------------------------------------------------
static Pattern parse_pattern(Parser &parser, Memory M);

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

			string_view ident = consume_identifier(parser);
			return Pattern(VarPatternUnresolved(ranger.get(), ident, mutability), nullptr);
		}

		case Lexeme::IDENTIFIER:
		{
			Token const &ident_token = parser.get();
			if(parser.get_text(ident_token.span) == "_")
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

static Pattern parse_pattern(Parser &parser, Memory M)
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
static Stmt parse_stmt(Parser &parser, Memory M);

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

static Stmt parse_stmt(Parser &parser, Memory M)
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
static TypeParameter parse_type_param(Parser &parser)
{
	string_view type_var_name = consume_identifier(parser);
	return TypeParameter{
		.range = {parser.prev_token_idx(), parser.prev_token_idx()},
		.name = type_var_name,
	};
}

static FixedArray<TypeParameter>* parse_type_param_list(Parser &parser, Memory M)
{
	ListBuilder<TypeParameter> type_params(M.temp);
	if(try_consume(parser, Lexeme::SINGLE_QUOTE))
	{
		if(try_consume(parser, Lexeme::LEFT_PAREN))
		{
			while(parser.get().kind != Lexeme::RIGHT_PAREN)
			{
				type_params.append(parse_type_param(parser));
				if(parser.get().kind != Lexeme::RIGHT_PAREN)
					consume(parser, Lexeme::COMMA);
			}
			consume(parser, Lexeme::RIGHT_PAREN);
		}
		else
			type_params.append(parse_type_param(parser));
	}

	return type_params.to_array(*M.main);
}

static ProcItem parse_proc(Parser &parser, Memory M)
{
	TokenRanger ranger(parser);
	consume(parser, Lexeme::PROC);

	ProcItem proc;
	proc.name = consume_identifier(parser);
	if(try_consume(parser, Lexeme::DOT))
	{
		proc.receiver_name = proc.name;
		proc.name = consume_identifier(parser);
	}

	proc.type_params = parse_type_param_list(parser, M);

	// Parse parameters
	consume(parser, Lexeme::LEFT_PAREN);
	ListBuilder<Parameter> params(M.temp);
	while(parser.get().kind != Lexeme::RIGHT_PAREN)
	{
		TokenRanger param_ranger(parser);
		string_view param_name = consume_identifier(parser);

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

static StructItem parse_struct(Parser &parser, StructParseContext struct_context, Memory M)
{
	TokenRanger ranger(parser);

	if(struct_context == StructParseContext::TOP_LEVEL)
		consume(parser, Lexeme::STRUCT);

	StructItem struct_;
	struct_.name = consume_identifier(parser);
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
			}
			else if(try_consume(parser, Lexeme::STRUCT))
			{
				StructItem *struct_member = M.main->alloc<StructItem>(
					parse_struct(parser, StructParseContext::INLINE, M)
				);
				members.append(StructMember(struct_member));
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

				member.range = member_ranger.get();
				members.append(VarMember(member, nullopt));
			}

			if(parser.get().kind != Lexeme::RIGHT_BRACE)
				consume(parser, Lexeme::COMMA);
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
			alias.name = consume_identifier(parser);

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
