#include "functions.h"

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

void message(const char error[])
{
	fprintf(stderr, "ERROR: %s\n", error);
}

void message_error_args()
{
	message("INVALID NUMBER OF COMMAND LINE ARGUMENTS PASSED TO INPUT.");
}

void message_error_file()
{
	message("THE FILE DID NOT OPEN.");
}

void message_error_memo()
{
	message("FAILED TO ALLOCATE MEMORY.");
}

void message_error_unkn()
{
	message("WHAT THE HELL YOU'VE DONE WITH MY PROGRAM? I DON'T KNOW HOW TO HANDLE THIS.");
}

void message_error_long()
{
	message("CAN'T CONVERT A STRING TO LONG LONG, BECAUSE IT'S OVERFLOWED.");
}

bool check(const void *ptr)
{
	return ptr == NULL;
}

bool isNumeric(const char *str)
{
	bool flag = false;
	for (size_t x = 0; x < strlen(str); x++)
	{
		if (str[x] == '0' || str[x] == '1' || str[x] == '2' || str[x] == '3' || str[x] == '4' || str[x] == '5' ||
			str[x] == '6' || str[x] == '7' || str[x] == '8' || str[x] == '9' || str[x] == 'N' || str[x] == 'a')
		{
			flag = true;
			break;
		}
	}
	return flag;
}
