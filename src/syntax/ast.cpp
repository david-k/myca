#include "syntax/ast.hpp"

size_t ArrayType::count() const
{
	assert(count_arg);
	return std::get<IntLiteralExpr>(*count_arg).value;
}
