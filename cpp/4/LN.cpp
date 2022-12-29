#include "LN.h"

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

LN::LN(std::size_t const capacity) : capacity(capacity)
{
	digit = new (std::nothrow) char[capacity + 1]();
	if (!digit)
	{
		throw ERROR_MEMORY_LEAK;
	}
	digit[capacity] = '\0';
	for (std::size_t i = 0; i != capacity; i++)
	{
		digit[i] = '0';
	}
}

LN::LN(long long int const expression)
{
	zero = expression == 0;
	negate = expression < 0;
	capacity = size = LN::length(expression) + zero;
	digit = new (std::nothrow) char[capacity + 1]();
	if (!digit)
	{
		throw ERROR_MEMORY_LEAK;
	}
	std::sprintf(digit, "%llu", std::abs(expression));
	LN::inverse(digit, capacity);
	digit[capacity] = '\0';
}

LN::LN(char const *expression)
{
	negate = expression[0] == '-';
	std::size_t const pos = LN::position(expression);
	std::size_t const len = std::strlen(expression);
	capacity = size = len - pos;
	digit = new (std::nothrow) char[capacity + 1]();
	if (!digit)
	{
		throw ERROR_MEMORY_LEAK;
	}
	for (std::size_t i = len - 1, j = 0; i != pos; i--, j++)
	{
		digit[j] = expression[i];
	}
	digit[capacity - 1] = expression[pos];
	digit[capacity] = '\0';
	zero = std::strcmp("0", digit) == 0;
	nan = std::strcmp("NaN", digit) == 0;
}

LN::LN(std::string_view const expression)
{
	negate = expression[0] == '-';
	std::size_t const pos = LN::position(expression);
	std::size_t const len = expression.size();
	capacity = size = len - pos;
	digit = new (std::nothrow) char[capacity + 1]();
	if (!digit)
	{
		throw ERROR_MEMORY_LEAK;
	}
	for (std::size_t i = len - 1, j = 0; i != pos; i--, j++)
	{
		digit[j] = expression[i];
	}
	digit[capacity - 1] = expression[pos];
	digit[capacity] = '\0';
	zero = std::strcmp("0", digit) == 0;
	nan = std::strcmp("NaN", digit) == 0;
}

LN::LN(LN const &other)
{
	if (this == &other)
	{
		return;
	}
	size = other.size;
	capacity = other.capacity;
	nan = other.nan;
	zero = other.zero;
	negate = other.negate;
	digit = new char[capacity + 1]();
	digit[capacity] = '\0';
	std::memcpy(digit, other.digit, capacity);
}

LN::LN(LN &&other)
{
	if (this == &other)
	{
		return;
	}
	size = other.size;
	capacity = other.capacity;
	nan = other.nan;
	zero = other.zero;
	negate = other.negate;
	digit = new (std::nothrow) char[capacity + 1]();
	if (!digit)
	{
		throw ERROR_MEMORY_LEAK;
	}
	digit[capacity] = '\0';
	std::memcpy(digit, other.digit, capacity);
	other.size = other.capacity = other.nan = other.zero = other.negate = false;
	delete[] other.digit;
	other.digit = nullptr;
}

LN &LN::operator=(LN const &other)
{
	if (this == &other)
	{
		return *this;
	}
	size = other.size;
	capacity = other.capacity;
	nan = other.nan;
	zero = other.zero;
	negate = other.negate;
	digit = new (std::nothrow) char[capacity + 1]();
	if (!digit)
	{
		throw ERROR_MEMORY_LEAK;
	}
	digit[capacity] = '\0';
	std::memcpy(digit, other.digit, capacity);
	return *this;
}

LN &LN::operator=(LN &&other)
{
	if (this == &other)
	{
		return *this;
	}
	size = other.size;
	capacity = other.capacity;
	nan = other.nan;
	zero = other.zero;
	negate = other.negate;
	digit = new (std::nothrow) char[capacity + 1]();
	if (!digit)
	{
		throw ERROR_MEMORY_LEAK;
	}
	digit[capacity] = '\0';
	std::memcpy(digit, other.digit, capacity);
	other.size = other.capacity = other.nan = other.zero = other.negate = false;
	delete[] other.digit;
	other.digit = nullptr;
	return *this;
}

LN::~LN()
{
	delete[] digit;
	digit = nullptr;
	size = capacity = 0;
	zero = nan = negate = false;
}

LN LN::operator-() const
{
	if (nan)
	{
		return LN("NaN");
	}
	LN unary(*this);
	unary.negate = !unary.negate;
	return unary;
}

LN LN::operator~() const
{
	if (zero)
	{
		return *this;
	}
	if (negate || nan)
	{
		return LN("NaN");
	}
	std::size_t const max_capacity = this->capacity / 2 + 1;
	LN res(max_capacity + 1);
	bool is_leading = true;
	for (std::size_t i = 0; i != max_capacity + 1; i++)
	{
		char last = '0';
		for (char j = '1'; j <= '9'; j++)
		{
			res[i] = j;
			LN ov(res.get());
			if (ov * ov <= (*this))
			{
				last = j;
			}
			else
			{
				break;
			}
		}
		if (is_leading && last == '0')
		{
			res.size++;
		}
		else if (is_leading && last != '0')
		{
			is_leading = false;
		}
		res[i] = last;
	}
	LN::inverse(res.get(), res.capacity);
	res.size = res.capacity - res.size;
	res.resize();
	return res;
}

bool operator<(const LN &lhs, const LN &rhs)
{
	if (lhs.nan || rhs.nan)
	{
		return false;
	}
	if (lhs.zero && rhs.zero)
	{
		return false;
	}
	if (lhs.negate && !rhs.negate)
	{
		return true;
	}
	if (!lhs.negate && rhs.negate)
	{
		return false;
	}
	if (!lhs.negate && !rhs.negate && lhs.capacity < rhs.capacity)
	{
		return true;
	}
	if (!lhs.negate && !rhs.negate && lhs.capacity > rhs.capacity)
	{
		return false;
	}
	if (lhs.negate && rhs.negate && lhs.capacity < rhs.capacity)
	{
		return false;
	}
	if (lhs.negate && rhs.negate && lhs.capacity > rhs.capacity)
	{
		return true;
	}
	if (!lhs.negate && !rhs.negate)
	{
		return LN::compare_left_less_right(lhs.get(), rhs.get(), lhs.capacity);
	}
	else
	{
		return LN::compare_left_less_right_negate(lhs.get(), rhs.get(), lhs.capacity);
	}
}

bool operator<=(const LN &lhs, const LN &rhs)
{
	return (lhs < rhs || lhs == rhs);
}

bool operator==(const LN &lhs, const LN &rhs)
{
	if (lhs.nan || rhs.nan)
	{
		return false;
	}

	return (lhs.zero && rhs.zero) || (rhs.negate == lhs.negate && std::strcmp(lhs.get(), rhs.get()) == 0);
}

bool operator>=(const LN &lhs, const LN &rhs)
{
	return (lhs > rhs || lhs == rhs);
}

bool operator>(const LN &lhs, const LN &rhs)
{
	if (lhs.nan || rhs.nan)
	{
		return false;
	}
	if (lhs.zero && rhs.zero)
	{
		return false;
	}
	if (lhs.negate && !rhs.negate)
	{
		return false;
	}
	if (!lhs.negate && rhs.negate)
	{
		return true;
	}
	if (!lhs.negate && !rhs.negate && lhs.capacity < rhs.capacity)
	{
		return false;
	}
	if (!lhs.negate && !rhs.negate && lhs.capacity > rhs.capacity)
	{
		return true;
	}
	if (lhs.negate && rhs.negate && lhs.capacity < rhs.capacity)
	{
		return true;
	}
	if (lhs.negate && rhs.negate && lhs.capacity > rhs.capacity)
	{
		return false;
	}
	if (!lhs.negate && !rhs.negate)
	{
		return LN::compare_left_greater_right(lhs.get(), rhs.get(), lhs.capacity);
	}
	else
	{
		return LN::compare_left_greater_right_negate(lhs.get(), rhs.get(), lhs.capacity);
	}
}

bool operator!=(const LN &lhs, const LN &rhs)
{
	return !(lhs == rhs);
}

LN operator+(const LN &lhs, const LN &rhs)
{
	if (lhs.nan || rhs.nan)
	{
		return LN("NaN");
	}
	if (lhs.negate && rhs.negate)
	{
		return -(-lhs + (-rhs));
	}
	if (lhs.negate && !rhs.negate)
	{
		return rhs - (-lhs);
	}
	if (!lhs.negate && rhs.negate)
	{
		return lhs - (-rhs);
	}
	std::size_t const max_capacity = std::max(lhs.capacity, rhs.capacity);
	LN res(max_capacity + 1);
	int ov = 0;
	for (std::size_t i = 0; i != max_capacity; i++)
	{
		int const up = GET(lhs);
		int const dw = GET(rhs);
		int const aw = up + dw + ov;
		if (aw > 9)
		{
			res[i] = LN::to_char(aw - 10);
			ov = 1;
		}
		else
		{
			res[i] = LN::to_char(aw);
			ov = 0;
		}
		res.size++;
	}
	if (ov)
	{
		res[res.size++] = LN::to_char(ov);
	}
	res.resize();
	return res;
}

LN operator-(LN const &lhs, LN const &rhs)
{
	if (lhs.nan || rhs.nan)
	{
		return LN("NaN");
	}
	if (rhs.negate)
	{
		return lhs + (-rhs);
	}
	if (lhs.negate)
	{
		return -(-lhs + rhs);
	}
	if (lhs < rhs)
	{
		return -(rhs - lhs);
	}
	LN res(lhs);
	for (std::size_t i = 0; i != lhs.capacity; i++)
	{
		int const up = GET(res);
		int const dw = GET(rhs);
		if (up < dw)
		{
			std::size_t pos = 0;
			for (std::size_t j = i + 1; j != res.capacity; j++)
			{
				int const ov = LN::to_int(res[j]) - 1;
				if (ov >= 0)
				{
					pos = j;
					res[j] = LN::to_char(ov);
					break;
				}
			}
			for (std::size_t j = i + 1; j != pos; j++)
			{
				res[j] = LN::to_char(9);
			}
			int const aw = (10 + up) - dw;
			res[i] = LN::to_char(aw);
		}
		else
		{
			int const aw = up - dw;
			res[i] = LN::to_char(aw);
		}
	}
	res.size = res.capacity - LN::position_reverse(res.get());
	res.resize();
	return res;
}

LN operator*(LN const &lhs, LN const &rhs)
{
	if (lhs.nan || rhs.nan)
	{
		return LN("NaN");
	}
	if (lhs.zero || rhs.zero)
	{
		return LN("0");
	}
	std::size_t const max_capacity = std::max(lhs.capacity, rhs.capacity);
	LN res(2 * max_capacity + 1);
	int ov = 0;
	for (std::size_t i = 0; i != max_capacity; i++)
	{
		std::size_t index = i;
		int const up = GET(rhs);
		if (!up)
		{
			continue;
		}
		for (std::size_t j = 0; j != max_capacity; j++)
		{
			int const dw = GETJ(lhs);
			int const aw = (up * dw) + ov + LN::to_int(res[index]);
			if (aw > 9)
			{
				res[index++] = LN::to_char(aw % 10);
				ov = ((aw - (aw % 10)) / 10);
			}
			else
			{
				res[index++] = LN::to_char(aw);
				ov = 0;
			}
		}
		if (ov)
		{
			res[index] = LN::to_char(ov);
			ov = 0;
		}
	}
	res.negate = lhs.negate ^ rhs.negate;
	res.size = res.capacity - LN::position_reverse(res.get());
	res.resize();
	return res;
}

LN operator/(LN const &lhs, LN const &rhs)
{
	if (rhs.nan || lhs.nan || rhs.zero)
	{
		return LN("NaN");
	}
	if (lhs.zero)
	{
		return LN("0");
	}
	if (rhs == LN("1"))
	{
		return lhs;
	}
	bool const negate_lhs = lhs.negate;
	bool const negate_rhs = rhs.negate;
	lhs.negate = false;
	rhs.negate = false;
	std::size_t const max_capacity = std::max(lhs.capacity, rhs.capacity);
	LN res(max_capacity + 1);
	bool is_leading = true;
	for (std::size_t i = 0; i != max_capacity + 1; i++)
	{
		char last = '0';
		for (char j = '1'; j <= '9'; j++)
		{
			res[i] = j;
			LN ov(res.get());
			if (ov * rhs <= lhs)
			{
				last = j;
			}
			else
			{
				break;
			}
		}
		if (is_leading && last == '0')
		{
			res.size++;
		}
		else if (is_leading && last != '0')
		{
			is_leading = false;
		}
		res[i] = last;
	}
	LN::inverse(res.get(), res.capacity);
	lhs.negate = negate_lhs;
	rhs.negate = negate_rhs;
	res.negate = negate_lhs ^ negate_rhs;
	res.size = res.capacity - res.size + is_leading;
	res.resize();
	return res;
}

LN operator%(LN const &lhs, LN const &rhs)
{
	if (lhs.nan || rhs.nan || rhs.zero)
	{
		return LN("NaN");
	}
	LN res = lhs - ((lhs / rhs) * rhs);
	if ((lhs.negate && !rhs.negate) || (lhs.negate && rhs.negate))
	{
		res.negate = true;
	}
	return res;
}

void LN::print(std::FILE *const fou)
{
	if (negate && !zero)
	{
		std::fprintf(fou, "-");
	}
	for (std::size_t i = size - 1; i != 0; i--)
	{
		std::fprintf(fou, "%c", digit[i]);
	}
	std::fprintf(fou, "%c\n", digit[0]);
}

void LN::resize()
{
	if (capacity > size)
	{
		char *new_data = new char[size + 1]();
		new_data[size] = '\0';
		std::memcpy(new_data, digit, size);
		delete[] digit;
		digit = new_data;
		capacity = size;
	}
	zero = std::strcmp(digit, "0") == 0;
}

LN::operator long long() const
{
	char *number_char = new (std::nothrow) char[capacity + 1 + negate]();
	if (!number_char)
	{
		throw ERROR_MEMORY_LEAK;
	}
	number_char[0] = (negate ? '-' : '0');
	for (std::size_t i = negate, j = capacity - 1; i != capacity + negate; i++, j--)
	{
		number_char[i] = digit[j];
	}
	number_char[capacity + negate] = '\0';
	long long int const number_long_long = std::atoll(number_char);
	if (LN(number_long_long) != *this)
	{
		throw ERROR_LONG_LONG;
	}
	delete[] number_char;
	return number_long_long;
}

std::size_t LN::length(long long int expression)
{
	std::size_t x = 0;
	while (expression)
	{
		expression = (expression - (expression % 10)) / 10;
		x++;
	}
	return x;
}

void LN::inverse(char *word, std::size_t const size)
{
	for (std::size_t i = 0, j = size - 1; i < j; i++, j--)
	{
		std::swap(word[i], word[j]);
	}
}

std::size_t LN::position(char const *expression)
{
	std::size_t x;
	for (x = 0; (expression[x] != '1' && expression[x] != '2' && expression[x] != '3' && expression[x] != '4' &&
				 expression[x] != '5' && expression[x] != '6' && expression[x] != '7' && expression[x] != '8' &&
				 expression[x] != '9' && expression[x] != 'N' && expression[x] != 'a' && x < std::strlen(expression) - 1);
		 x++)
		;
	return x;
}

std::size_t LN::position_reverse(char const *expression)
{
	std::size_t x, y;
	for (x = std::strlen(expression) - 1, y = 0;
		 (expression[x] != '1' && expression[x] != '2' && expression[x] != '3' && expression[x] != '4' &&
		  expression[x] != '5' && expression[x] != '6' && expression[x] != '7' && expression[x] != '8' &&
		  expression[x] != '9' && expression[x] != 'N' && expression[x] != 'a' && x != 0);
		 x--, y++)
		;
	return y;
}

std::size_t LN::position(std::string_view const &expression)
{
	std::size_t x;
	for (x = 0; (expression[x] != '1' && expression[x] != '2' && expression[x] != '3' && expression[x] != '4' &&
				 expression[x] != '5' && expression[x] != '6' && expression[x] != '7' && expression[x] != '8' &&
				 expression[x] != '9' && expression[x] != 'N' && expression[x] != 'a' && x < expression.size() - 1);
		 x++)
		;
	return x;
}

int LN::to_int(char const val)
{
	return val - '0';
}

char LN::to_char(int const val)
{
	return static_cast< char >(val + '0');
}

bool LN::compare_left_less_right(char const *lhs, char const *rhs, std::size_t const size)
{
	for (std::size_t i = size - 1; i != 0; i--)
	{
		if (lhs[i] > rhs[i])
		{
			return false;
		}
		else if (lhs[i] < rhs[i])
		{
			return true;
		}
	}
	return lhs[0] < rhs[0];
}

bool LN::compare_left_less_right_negate(char const *lhs, char const *rhs, std::size_t const size)
{
	for (std::size_t i = size - 1; i != 0; i--)
	{
		if (lhs[i] < rhs[i])
		{
			return false;
		}
		else if (lhs[i] > rhs[i])
		{
			return true;
		}
	}
	return lhs[0] > rhs[0];
}

bool LN::compare_left_greater_right(char const *lhs, char const *rhs, std::size_t const size)
{
	for (std::size_t i = size - 1; i != 0; i--)
	{
		if (rhs[i] > lhs[i])
		{
			return false;
		}
		else if (rhs[i] < lhs[i])
		{
			return true;
		}
	}
	return lhs[0] > rhs[0];
}

bool LN::compare_left_greater_right_negate(char const *lhs, char const *rhs, std::size_t const size)
{
	for (std::size_t i = size - 1; i != 0; i--)
	{
		if (rhs[i] < lhs[i])
		{
			return false;
		}
		else if (rhs[i] > lhs[i])
		{
			return true;
		}
	}
	return lhs[0] < rhs[0];
}
