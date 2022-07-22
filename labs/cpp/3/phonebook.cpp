#include "phonebook.h"
#include <cstring>

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

bool phonebook::operator<(const phonebook& right) const
{
	if (strcmp(this->name1, right.name1) != 0)
	{
		return strcmp(this->name1, right.name1) < 0;
	}
	if (strcmp(this->name2, right.name2) != 0)
	{
		return strcmp(this->name2, right.name2) < 0;
	}
	if (strcmp(this->name3, right.name3) != 0)
	{
		return strcmp(this->name3, right.name3) < 0;
	}
	return this->numb < right.numb;
}

bool phonebook::operator>(const phonebook& right) const
{
	if (strcmp(this->name1, right.name1) != 0)
	{
		return strcmp(this->name1, right.name1) > 0;
	}
	if (strcmp(this->name2, right.name2) != 0)
	{
		return strcmp(this->name2, right.name2) > 0;
	}
	if (strcmp(this->name3, right.name3) != 0)
	{
		return strcmp(this->name3, right.name3) > 0;
	}
	return this->numb > right.numb;
}
