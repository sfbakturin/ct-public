#include <cstdio>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct phonebook
{
	char name1[21], name2[21], name3[21];
	unsigned long long numb;
	bool operator<(const phonebook &right) const;
	bool operator>(const phonebook &right) const;
};
