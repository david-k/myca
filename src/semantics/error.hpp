#pragma once

#include "syntax/ast.hpp"

using ThrowExprErrorFn = void (*)(Expr const &expr, string const &reason, Module const &mod);
using ThrowPatternErrorFn = void (*)(Pattern const &pattern, string const &reason, Module const &mod);

class LazyErrorMsg
{
public:
	LazyErrorMsg(Expr const *expr, ThrowExprErrorFn fn) :
		m_error_fns(std::pair(expr, fn)) {}

	LazyErrorMsg(Pattern const *pattern, ThrowPatternErrorFn fn) :
		m_error_fns(std::pair(pattern, fn)) {}

	[[noreturn]] void throw_error(string const &reason, Module const &mod) const
	{
		m_error_fns | match
		{
			[&](std::pair<Expr const*, ThrowExprErrorFn> expr_fn)
			{
				expr_fn.second(*expr_fn.first, reason, mod);
			},
			[&](std::pair<Pattern const*, ThrowPatternErrorFn> pattern_fn)
			{
				pattern_fn.second(*pattern_fn.first, reason, mod);
			},
		};

		assert(!"ThrowErrorFn returned");
	}

private:
	variant <
		std::pair<Expr const*, ThrowExprErrorFn>,
		std::pair<Pattern const*, ThrowPatternErrorFn>
	> m_error_fns;
};

inline string mk_error_msg(string const &msg, TokenIdx tok_idx, Module const *mod)
{
	if(tok_idx.value == INVALID_TOKEN_IDX.value)
		return "error: " + msg;

	Token const &tok = mod->parser.token_at(tok_idx);
	return "|" + str(tok.span.begin) + "| error: " + msg;
}

[[noreturn]] inline void throw_sem_error(string const &msg, TokenIdx tok_idx, Module const *mod)
{
	throw ParseError(mk_error_msg(msg, tok_idx, mod));
}
