#include "LN.h"

#include "error.h"
#include "functions.h"

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define ZERO '\0'

LN::LN(const long long int expr)
{
	if (expr < 0)
	{
		this->isNegate = true;
	}

	if (expr == 0)
	{
		this->isZero = true;
	}

	this->length = LN::getLength(expr);
	char *expression = (char *)malloc(sizeof(char) * this->length);
	if (check(expression))
	{
		throw EXCEPTION_MALLOC;
	}
	sprintf(expression, "%llu", std::abs(expr));
	this->data = (char *)malloc(sizeof(char) * (this->length + 1));
	if (check(this->data))
	{
		free(expression);
		throw EXCEPTION_MALLOC;
	}
	size_t itDigit = 0, itExpr = this->length - 1;

	while (itDigit != this->length)
	{
		this->data[itDigit++] = expression[itExpr];
		if (itExpr == 0)
		{
			break;
		}
		itExpr--;
	}

	this->data[this->length] = ZERO;

	free(expression);
}

LN::LN(const char *expr)
{
	if (expr[0] == '-')
	{
		this->isNegate = true;
	}

	const size_t position = LN::getStart(expr);
	this->length = std::strlen(expr) - position;
	this->data = (char *)malloc(sizeof(char) * (this->length + 1));
	if (check(this->data))
	{
		throw EXCEPTION_MALLOC;
	}
	size_t itDigit = 0, itExpr = std::strlen(expr) - 1;

	while (itDigit != this->length)
	{
		this->data[itDigit++] = expr[itExpr];
		if (itExpr == 0)
		{
			break;
		}
		itExpr--;
	}

	this->data[this->length] = ZERO;

	if (LN::checkNaN(this->data))
	{
		this->isNaN = true;
	}

	if (LN::checkZero(this->data))
	{
		this->isZero = true;
	}
}

LN::LN(const std::string_view expr)
{
	if (expr[0] == '-')
	{
		this->isNegate = true;
	}

	const size_t position = LN::getStart(expr);
	this->length = expr.size() - position;
	this->data = (char *)malloc(sizeof(char) * (this->length + 1));
	if (check(this->data))
	{
		throw EXCEPTION_MALLOC;
	}
	size_t itDigit = 0, itExpr = expr.size() - 1;

	while (itDigit != this->length)
	{
		this->data[itDigit++] = expr[itExpr];
		if (itExpr == 0)
		{
			break;
		}
		itExpr--;
	}

	this->data[this->length] = ZERO;

	if (LN::checkNaN(this->data))
	{
		this->isNaN = true;
	}

	if (LN::checkZero(this->data))
	{
		this->isZero = true;
	}
}

LN::LN(const LN &right)
{
	this->isNaN = right.isNaN;
	this->isZero = right.isZero;
	this->isNegate = right.isNegate;
	this->length = right.length;
	this->data = (char *)malloc(sizeof(char) * (this->length + 1));
	if (check(this->data))
	{
		throw EXCEPTION_MALLOC;
	}
	LN::copyElements(right.data, this->data, this->length);
}

void LN::operator=(const LN &right)
{
	this->isNaN = right.isNaN;
	this->isZero = right.isZero;
	this->isNegate = right.isNegate;
	this->length = right.length;
	this->data = (char *)malloc(sizeof(char) * (this->length + 1));
	if (check(this->data))
	{
		throw EXCEPTION_MALLOC;
	}
	LN::copyElements(right.data, this->data, this->length);
}

LN::~LN()
{
	free(this->data);
	this->data = NULL;
	this->length = 0;
	this->isNegate = false;
	this->isZero = false;
	this->isNaN = false;
}

LN LN::operator+(const LN &right) const
{
	if ((*this).isNaN || right.isNaN)
	{
		return LN("NaN");
	}

	if ((*this).isNegate && right.isNegate)
	{
		return -((-(*this)) + (-right));
	}

	if ((*this).isNegate && !right.isNegate)
	{
		return right - (-(*this));
	}

	if (!(*this).isNegate && right.isNegate)
	{
		return (*this) - (-right);
	}

	const size_t maximumLength = std::max((*this).length, right.length);
	char *digit = LN::getArray(maximumLength + 1);
	if (check(digit))
	{
		throw EXCEPTION_MALLOC;
	}
	int overflow = 0;

	for (size_t i = 0; i < maximumLength; i++)
	{
		const int upper = (i < (*this).length ? LN::convertToInt((*this).data[i]) : 0);
		const int down = (i < right.length ? LN::convertToInt(right.data[i]) : 0);
		const int answer = upper + down + overflow;
		if (answer > 9)
		{
			digit[i] = LN::convertToChar(answer - 10);
			overflow = 1;
		}
		else
		{
			digit[i] = LN::convertToChar(answer);
			overflow = 0;
		}
	}

	size_t size = maximumLength;

	if (overflow != 0)
	{
		char *over_digit = (char *)realloc(digit, (maximumLength + 2));
		if (check(over_digit))
		{
			free(digit);
			throw EXCEPTION_MALLOC;
		}
		digit = over_digit;
		digit[maximumLength] = LN::convertToChar(overflow);
		digit[maximumLength + 1] = ZERO;
		size += 2;
	}
	else
	{
		digit[maximumLength] = ZERO;
		size += 1;
	}

	LN::reverse(digit, size - 1);

	LN result(digit);

	free(digit);

	return result;
}

LN LN::operator-(const LN &right) const
{
	if ((*this).isNaN || right.isNaN)
	{
		return LN("NaN");
	}

	if (right.isNegate)
	{
		return (*this) + (-right);
	}

	if ((*this).isNegate)
	{
		return -((-(*this)) + right);
	}

	if ((*this) < right)
	{
		return -(right - (*this));
	}

	char *digit = LN::getArray((*this).length + 2);
	if (check(digit))
	{
		throw EXCEPTION_MALLOC;
	}

	for (size_t i = 0; i < (*this).length; i++)
	{
		const int upper = (i < (*this).length ? LN::convertToInt((*this).data[i]) : 0);
		const int down = (i < right.length ? LN::convertToInt(right.data[i]) : 0);

		if (upper < down)
		{
			size_t position = 0;

			for (size_t j = i + 1; j < (*this).length; j++)
			{
				const int overflow = LN::convertToInt((*this).data[j]) - 1;

				if (overflow >= 0)
				{
					position = j;
					(*this).data[j] = LN::convertToChar(overflow);
					break;
				}
			}

			for (size_t j = i + 1; j < position; j++)
			{
				(*this).data[j] = LN::convertToChar(9);
			}

			const int answer = (10 + upper) - down;
			digit[i] = LN::convertToChar(answer);
		}
		else
		{
			const int answer = upper - down;
			digit[i] = LN::convertToChar(answer);
		}
	}

	digit[(*this).length + 1] = ZERO;

	LN::reverse(digit, (*this).length + 1);

	LN result(digit);

	free(digit);

	return result;
}

LN LN::operator*(const LN &right) const
{
	if ((*this).isNaN || right.isNaN)
	{
		return LN("NaN");
	}

	if ((*this).isZero || right.isZero)
	{
		return LN();
	}

	const size_t maximumLength = std::max((*this).length, right.length);
	char *digit = LN::getArray(2 * maximumLength + 2);
	if (check(digit))
	{
		throw EXCEPTION_MALLOC;
	}
	int overflow = 0;

	for (size_t i = 0; i < maximumLength; i++)
	{
		size_t index = i;
		const int upper = (right.length > i ? LN::convertToInt(right.data[i]) : 0);

		if (upper == 0)
		{
			continue;
		}

		for (size_t j = 0; j < maximumLength; j++)
		{
			const int down = ((*this).length > j ? LN::convertToInt((*this).data[j]) : 0);
			const int answer = (upper * down) + overflow + LN::convertToInt(digit[index]);
			if (answer > 9)
			{
				digit[index++] = LN::convertToChar(answer % 10);
				overflow = ((answer - (answer % 10)) / 10);
			}
			else
			{
				digit[index++] = LN::convertToChar(answer);
				overflow = 0;
			}
		}

		if (overflow != 0)
		{
			digit[index] = LN::convertToChar(overflow);
			overflow = 0;
		}

		/*const int numbR = (right.length > i ? LN::convertToInt(right.data[i]) : 0);
		if (numbR == 0)
		{
			continue;
		}
		size_t index = i;
		char *digit = LN::getArray(2 * maximumLength + 2);
		if (check(digit))
		{
			std::cout << "LN c = a * b" << std::endl;
			throw EXCEPTION_MALLOC;
		}
		int overflow = 0;

		for (size_t j = 0; j < maximumLength; j++)
		{
			const int numbL = ((*this).length > j ? LN::convertToInt((*this).data[j]) : 0);
			const int numbM = (numbL * numbR) + overflow;
			if (numbM > 9)
			{
				digit[index++] = LN::convertToChar(numbM % 10);
				overflow = ((numbM - (numbM % 10)) / 10);
			}
			else
			{
				digit[index++] = LN::convertToChar(numbM);
				overflow = 0;
			}
		}

		if (overflow != 0)
		{
			digit[index] = LN::convertToChar(overflow);
		}

		digit[2 * maximumLength + 1] = ZERO;

		LN::reverse(digit, 2 * maximumLength + 1);

		result = result + LN(digit);

		free(digit);*/
	}

	digit[2 * maximumLength + 1] = ZERO;

	LN::reverse(digit, 2 * maximumLength + 1);

	LN result(digit);

	result.isNegate = (*this).isNegate ^ right.isNegate;

	free(digit);

	return result;
}

LN LN::operator/(const LN &right) const
{
	if ((*this).isNaN || right.isNaN || right.isZero)
	{
		return LN("NaN");
	}

	if ((*this).isZero)
	{
		return LN();
	}

	if (right == LN(1))
	{
		return (*this);
	}

	const bool negateL = (*this).isNegate;
	const bool negateR = right.isNegate;

	(*this).isNegate = false;
	right.isNegate = false;

	const size_t maximumLength = std::max((*this).length, right.length);
	char *digit = LN::getArray(maximumLength + 2);
	if (check(digit))
	{
		throw EXCEPTION_MALLOC;
	}

	for (size_t i = 1; i < maximumLength + 1; i++)
	{
		char last = '0';

		for (char j = '1'; j <= '9'; j++)
		{
			digit[i] = j;
			digit[maximumLength + 1] = ZERO;

			LN ov(digit);

			if (ov * right <= (*this))
			{
				last = j;
			}
			else
			{
				break;
			}

			ov.~LN();
		}

		digit[i] = last;
	}

	if (negateL ^ negateR)
	{
		digit[0] = '-';
	}

	(*this).isNegate = negateL;
	right.isNegate = negateR;

	digit[maximumLength + 1] = ZERO;

	LN result(digit);

	free(digit);

	return result;
}

LN LN::operator%(const LN &right) const
{
	if ((*this).isNaN || right.isNaN || right.isZero)
	{
		return LN("NaN");
	}

	LN answer = (*this) - (((*this) / right) * right);
	if (((*this).isNegate && !right.isNegate) || ((*this).isNegate && right.isNegate))
	{
		answer.isNegate = true;
	}

	return answer;
}

LN LN::operator~() const
{
	if ((*this).isZero)
	{
		return LN();
	}

	if ((*this).isNaN || (*this).isNegate)
	{
		return LN("NaN");
	}

	const size_t maximumLength = (*this).length / 2 + 1;
	char *digit = LN::getArray(maximumLength + 2);
	if (check(digit))
	{
		throw EXCEPTION_MALLOC;
	}

	for (size_t i = 1; i < maximumLength + 1; i++)
	{
		char last = '0';

		for (char j = '1'; j <= '9'; j++)
		{
			digit[i] = j;
			digit[maximumLength + 1] = ZERO;

			LN ov(digit);

			if (ov * ov <= (*this))
			{
				last = j;
			}
			else
			{
				break;
			}

			ov.~LN();
		}

		digit[i] = last;
	}

	digit[maximumLength + 1] = ZERO;

	LN result(digit);

	free(digit);

	return result;
}

LN LN::operator-() const
{
	if (this->isNaN)
	{
		return LN("NaN");
	}
	LN unary(*this);
	unary.isNegate = !unary.isNegate;
	return unary;
}

bool LN::operator<(const LN &right) const
{
	if ((*this).isNaN || right.isNaN)
	{
		return false;
	}

	if ((*this).isNegate && !right.isNegate)
	{
		return true;
	}

	if (!(*this).isNegate && right.isNegate)
	{
		return false;
	}

	if (!(*this).isNegate && !right.isNegate && (*this).length < right.length)
	{
		return true;
	}

	if (!(*this).isNegate && !right.isNegate && (*this).length > right.length)
	{
		return false;
	}

	if ((*this).isNegate && right.isNegate && (*this).length > right.length)
	{
		return true;
	}

	if ((*this).isNegate && right.isNegate && (*this).length < right.length)
	{
		return false;
	}

	if ((*this).isNegate && right.isNegate)
	{
		LN::reverse((*this).data, (*this).length);
		LN::reverse(right.data, right.length);
		const bool flag = (std::strcmp((*this).data, right.data) > 0);
		LN::reverse((*this).data, (*this).length);
		LN::reverse(right.data, right.length);
		return flag;
	}
	else
	{
		LN::reverse((*this).data, (*this).length);
		LN::reverse(right.data, right.length);
		const bool flag = (std::strcmp((*this).data, right.data) < 0);
		LN::reverse((*this).data, (*this).length);
		LN::reverse(right.data, right.length);
		return flag;
	}
}

bool LN::operator<=(const LN &right) const
{
	return (*this < right) || (*this == right);
}

bool LN::operator==(const LN &right) const
{
	if ((*this).isNaN || right.isNaN)
	{
		return false;
	}

	LN::reverse((*this).data, (*this).length);
	LN::reverse(right.data, right.length);

	const bool flag =
		(((*this).isZero && right.isZero) || ((*this).isNegate == right.isNegate && std::strcmp((*this).data, right.data) == 0));

	LN::reverse((*this).data, (*this).length);
	LN::reverse(right.data, right.length);

	return flag;
}

bool LN::operator!=(const LN &right) const
{
	return !(*this == right);
}

bool LN::operator>=(const LN &right) const
{
	return (*this > right) || (*this == right);
}

bool LN::operator>(const LN &right) const
{
	if ((*this).isNaN || right.isNaN)
	{
		return false;
	}

	if ((*this).isNegate && !right.isNegate)
	{
		return false;
	}

	if (!(*this).isNegate && right.isNegate)
	{
		return true;
	}

	if (!(*this).isNegate && !right.isNegate && (*this).length < right.length)
	{
		return false;
	}

	if (!(*this).isNegate && !right.isNegate && (*this).length > right.length)
	{
		return true;
	}

	if ((*this).isNegate && right.isNegate && (*this).length > right.length)
	{
		return false;
	}

	if ((*this).isNegate && right.isNegate && (*this).length < right.length)
	{
		return true;
	}

	if ((*this).isNegate && right.isNegate)
	{
		LN::reverse((*this).data, (*this).length);
		LN::reverse(right.data, right.length);
		const bool flag = (std::strcmp((*this).data, right.data) < 0);
		LN::reverse((*this).data, (*this).length);
		LN::reverse(right.data, right.length);
		return flag;
	}
	else
	{
		LN::reverse((*this).data, (*this).length);
		LN::reverse(right.data, right.length);
		const bool flag = (std::strcmp((*this).data, right.data) > 0);
		LN::reverse((*this).data, (*this).length);
		LN::reverse(right.data, right.length);
		return flag;
	}
}

void LN::operator+=(const LN &right)
{
	(*this) = (*this) + right;
}

void LN::operator-=(const LN &right)
{
	(*this) = (*this) - right;
}

void LN::operator*=(const LN &right)
{
	(*this) = (*this) * right;
}

void LN::operator/=(const LN &right)
{
	(*this) = (*this) / right;
}

size_t LN::getStart(const char *str)
{
	size_t x;
	for (x = 0; (str[x] != '1' && str[x] != '2' && str[x] != '3' && str[x] != '4' && str[x] != '5' && str[x] != '6' &&
				 str[x] != '7' && str[x] != '8' && str[x] != '9' && str[x] != 'N' && str[x] != 'a' && x < std::strlen(str) - 1);
		 x++)
		;
	return x;
}

size_t LN::getStart(const std::string_view str)
{
	size_t x;
	for (x = 0; (str[x] != '1' && str[x] != '2' && str[x] != '3' && str[x] != '4' && str[x] != '5' && str[x] != '6' &&
				 str[x] != '7' && str[x] != '8' && str[x] != '9' && str[x] != 'N' && str[x] != 'a' && x < str.size() - 1);
		 x++)
		;
	return x;
}

bool LN::checkZero(const char *str)
{
	return std::strcmp(str, "0") == 0;
}

bool LN::checkNaN(const char *str)
{
	return std::strcmp(str, "NaN") == 0;
}

size_t LN::getLength(const long long int expr)
{
	long long int copy = std::abs(expr);
	size_t x = 0;
	while (true)
	{
		copy = (copy - (copy % 10)) / 10;
		x++;
		if (copy == 0)
		{
			break;
		}
	}
	return x;
}

void LN::copyElements(const char *from, char *to, const size_t size)
{
	for (size_t i = 0; i < size; i++)
	{
		to[i] = from[i];
	}
	to[size] = ZERO;
}

int LN::convertToInt(const char c)
{
	return c - '0';
}

char LN::convertToChar(const int c)
{
	return (char)(c + '0');
}

char *LN::getArray(const size_t size)
{
	char *array = (char *)malloc(sizeof(char) * size);
	if (checkNaN(array))
	{
		return NULL;
	}
	for (size_t i = 0; i < size; i++)
	{
		array[i] = LN::convertToChar(0);
	}
	return array;
}

void LN::reverse(char *str, const size_t size)
{
	size_t j = size - 1;
	for (size_t i = 0; i < size / 2; i++)
	{
		const char t = str[i];
		str[i] = str[j];
		str[j] = t;
		j--;
	}
}

void LN::toString(FILE *const file) const
{
	if (this->isNaN)
	{
		fprintf(file, "NaN\n");
		return;
	}

	if (this->isZero)
	{
		fprintf(file, "0\n");
		return;
	}

	if (this->isNegate)
	{
		fprintf(file, "-");
	}

	for (size_t i = (this->length - 1); i != 0; i--)
	{
		fprintf(file, "%c", this->data[i]);
	}
	fprintf(file, "%c\n", this->data[0]);
}

LN &LN::operator=(LN &&right)
{
	if (this == &right)
	{
		return (*this);
	}
	this->isNaN = right.isNaN;
	this->isZero = right.isZero;
	this->isNegate = right.isNegate;
	this->length = right.length;
	this->data = (char *)malloc(sizeof(char) * (this->length + 1));
	if (check(this->data))
	{
		throw EXCEPTION_MALLOC;
	}
	LN::copyElements(right.data, this->data, this->length);
	right.~LN();
	return (*this);
}

LN::LN(LN &&right)
{
	if (this == &right)
	{
		return;
	}
	this->isNaN = right.isNaN;
	this->isZero = right.isZero;
	this->isNegate = right.isNegate;
	this->length = right.length;
	this->data = (char *)malloc(sizeof(char) * (this->length + 1));
	if (check(this->data))
	{
		throw EXCEPTION_MALLOC;
	}
	LN::copyElements(right.data, this->data, this->length);
	right.~LN();
}

LN::operator long long() const
{
	if (this->isNaN)
	{
		throw EXCEPTION_LONG_LONG;
	}
	const char *numb = this->toCharArray();
	if (check(numb))
	{
		throw EXCEPTION_MALLOC;
	}
	const long long int number = std::atoll(numb);
	if (LN(number) != *this)
	{
		throw EXCEPTION_LONG_LONG;
	}
	return number;
}

const char *LN::toCharArray() const
{
	if (this->isNegate)
	{
		char *string = (char *)malloc(sizeof(char) * (this->length + 2));
		if (check(string))
		{
			return NULL;
		}
		string[this->length + 1] = ZERO;
		string[0] = '-';
		size_t j = 1;
		for (size_t i = this->length - 1; i != 0; i--)
		{
			string[j++] = this->data[i];
		}
		string[j] = this->data[0];
		return string;
	}
	else
	{
		char *string = (char *)malloc(sizeof(char) * (this->length + 1));
		if (check(string))
		{
			return NULL;
		}
		string[this->length] = ZERO;
		size_t j = 0;
		for (size_t i = this->length - 1; i != 0; i--)
		{
			string[j++] = this->data[i];
		}
		string[j] = this->data[0];
		return string;
	}
}

LN::operator bool() const
{
	return this->isZero && !this->isNaN;
}
