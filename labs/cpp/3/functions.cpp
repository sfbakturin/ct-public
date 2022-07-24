#include <cstdio>
#include <cstring>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

const char *IMPL[] = { "int", "float", "phonebook" };
const char *MODE[] = { "ascending", "descending" };

int check(const char *p, const bool f)
{
	int flag = 1;
	for (int i = 0; i < (f ? 3 : 2); i++)
	{
		if (strcmp(p, (f ? IMPL[i] : MODE[i])) == 0)
		{
			flag = 0;
			break;
		}
	}
	return flag;
}

int check(const void *p)
{
	return (p == nullptr ? 1 : 0);
}

void message(const char error[], const char solution[])
{
	fprintf(stderr, "ERROR: %s\nSOLUTION: %s\n", error, solution);
}

void message_error_args()
{
	message("Wrong arguments.", "Please, give me correct.");
}

void message_error_file()
{
	message("No such file was found.", "Please, give me something that exists.");
}

void message_error_memory()
{
	message("Not enough memory resources are available to process this command.", "Try free for me some memory and try again.");
}

void message_error_impl()
{
	message("Sorry, but we can't implicate such TYPE of MODE.", "Give me something that I can.");
}
