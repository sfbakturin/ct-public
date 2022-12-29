#include <cstddef>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct phonebook
{
	char name1[21], name2[21], name3[21];
	std::size_t numb;
	bool operator<(phonebook const &right) const;
	bool operator>(phonebook const &right) const;
};
