#pragma once

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

#include "functions.h"

class LN
{
  private:
	char *data = NULL;
	size_t length = 0;
	bool isNaN = false;
	bool isZero = false;
	mutable bool isNegate = false;

  public:
	LN(const long long expr = 0);
	LN(const char *);
	LN(const std::string_view);

	LN(const LN &);
	void operator=(const LN &);

	~LN();

	LN operator+(const LN &right) const;
	LN operator-(const LN &right) const;
	LN operator*(const LN &right) const;
	LN operator/(const LN &right) const;
	LN operator%(const LN &right) const;
	LN operator~() const;
	LN operator-() const;
	bool operator<(const LN &right) const;
	bool operator<=(const LN &right) const;
	bool operator==(const LN &right) const;
	bool operator!=(const LN &right) const;
	bool operator>=(const LN &right) const;
	bool operator>(const LN &right) const;
	void operator+=(const LN &right);
	void operator-=(const LN &right);
	void operator*=(const LN &right);
	void operator/=(const LN &right);

	static size_t getStart(const char *);
	static size_t getStart(const std::string_view);
	static bool checkZero(const char *);
	static bool checkNaN(const char *);
	static size_t getLength(const long long);
	static void copyElements(const char *, char *, const size_t);
	static int convertToInt(const char);
	static char convertToChar(const int);
	static char *getArray(const size_t);
	static void reverse(char *, const size_t);

	void toString(FILE *const) const;
	const char *toCharArray() const;

	LN &operator=(LN &&);
	LN(LN &&);

	operator long long() const;
	operator bool() const;
};

inline LN operator"" _ln(const char *string)
{
	return LN(string);
}
