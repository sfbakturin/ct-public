#pragma once

#include "macros.h"
#include "return_errors.h"
#include <string_view>

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <new>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

class LN
{
  private:
	char *digit = nullptr;
	std::size_t size = 0;
	std::size_t capacity = 2;
	mutable bool negate = false;
	bool nan = false;
	bool zero = false;

  public:
	LN() = default;
	explicit LN(std::size_t const);
	explicit LN(long long int const expression = 0);
	explicit LN(char const *);
	explicit LN(std::string_view const);

	LN(LN const &);
	LN(LN &&);

	LN &operator=(LN const &);
	LN &operator=(LN &&);

	~LN();

	LN operator-() const;
	LN operator~() const;

	friend bool operator<(LN const &, LN const &);
	friend bool operator<=(LN const &, LN const &);
	friend bool operator==(LN const &, LN const &);
	friend bool operator>=(LN const &, LN const &);
	friend bool operator>(LN const &, LN const &);
	friend bool operator!=(LN const &, LN const &);
	friend LN operator+(LN const &, LN const &);
	friend LN operator-(LN const &, LN const &);
	friend LN operator*(LN const &, LN const &);
	friend LN operator/(LN const &, LN const &);
	friend LN operator%(LN const &, LN const &);
	void operator+=(LN const &other) { (*this) = (*this) / other; }
	void operator-=(LN const &other) { (*this) = (*this) - other; }
	void operator*=(LN const &other) { (*this) = (*this) * other; }
	void operator/=(LN const &other) { (*this) = (*this) / other; }

	char &operator[](std::size_t const index) { return digit[index]; }
	char const &operator[](std::size_t const index) const { return digit[index]; }

	char *get() const { return digit; }

	void print(std::FILE *const fou = stdout);
	void resize();

	explicit operator long long() const;
	explicit operator bool() const { return zero; }

	static std::size_t length(long long int);
	static void inverse(char *, std::size_t const);
	static std::size_t position(char const *);
	static std::size_t position_reverse(char const *);
	static std::size_t position(std::string_view const &);
	static int to_int(const char);
	static char to_char(const int);
	static bool compare_left_less_right(char const *, char const *, std::size_t const);
	static bool compare_left_greater_right(char const *, char const *, std::size_t const);
	static bool compare_left_less_right_negate(char const *, char const *, std::size_t const);
	static bool compare_left_greater_right_negate(char const *, char const *, std::size_t const);
};

inline LN operator"" _ln(char const *string)
{
	return LN(string);
}