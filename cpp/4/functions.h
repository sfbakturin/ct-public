#pragma once

#include "LN.h"
#include "macros.h"

#include <stack>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

void add(std::stack< LN > &expression)
{
	BINARY;
	LN res = left + right;
	expression.push(std::move(res));
}

void sub(std::stack< LN > &expression)
{
	BINARY;
	LN res = left - right;
	expression.push(std::move(res));
}

void mul(std::stack< LN > &expression)
{
	BINARY;
	LN res = left * right;
	expression.push(std::move(res));
}

void div(std::stack< LN > &expression)
{
	BINARY;
	LN res = left / right;
	expression.push(std::move(res));
}

void mod(std::stack< LN > &expression)
{
	BINARY;
	LN res = left % right;
	expression.push(std::move(res));
}

void uadd(std::stack< LN > &expression)
{
	BINARY;
	left += right;
	expression.push(std::move(left));
}

void usub(std::stack< LN > &expression)
{
	BINARY;
	left -= right;
	expression.push(std::move(left));
}

void umul(std::stack< LN > &expression)
{
	BINARY;
	left *= right;
	expression.push(std::move(left));
}

void udiv(std::stack< LN > &expression)
{
	BINARY;
	left /= right;
	expression.push(std::move(left));
}

void less(std::stack< LN > &expression)
{
	BINARY;
	BOOLEAN(left < right);
}

void less_equals(std::stack< LN > &expression)
{
	BINARY;
	BOOLEAN(left <= right);
}

void equals(std::stack< LN > &expression)
{
	BINARY;
	BOOLEAN(left == right);
}

void greater_equals(std::stack< LN > &expression)
{
	BINARY;
	BOOLEAN(left >= right);
}

void greater(std::stack< LN > &expression)
{
	BINARY;
	BOOLEAN(left > right);
}

void not_equals(std::stack< LN > &expression)
{
	BINARY;
	BOOLEAN(left != right);
}

void minus(std::stack< LN > &expression)
{
	UNARY;
	LN res = -unary;
	expression.push(std::move(res));
}

void sqrt(std::stack< LN > &expression)
{
	UNARY;
	LN res = ~unary;
	expression.push(std::move(res));
}