#include "phonebook.h"
#include <cstring>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

bool phonebook::operator<(phonebook const &right) const
{
	if (std::strcmp(this->name1, right.name1) != 0)
	{
		return std::strcmp(this->name1, right.name1) < 0;
	}
	if (std::strcmp(this->name2, right.name2) != 0)
	{
		return std::strcmp(this->name2, right.name2) < 0;
	}
	if (std::strcmp(this->name3, right.name3) != 0)
	{
		return std::strcmp(this->name3, right.name3) < 0;
	}
	return this->numb < right.numb;
}

bool phonebook::operator>(phonebook const &right) const
{
	if (std::strcmp(this->name1, right.name1) != 0)
	{
		return std::strcmp(this->name1, right.name1) > 0;
	}
	if (std::strcmp(this->name2, right.name2) != 0)
	{
		return std::strcmp(this->name2, right.name2) > 0;
	}
	if (std::strcmp(this->name3, right.name3) != 0)
	{
		return std::strcmp(this->name3, right.name3) > 0;
	}
	return this->numb > right.numb;
}
