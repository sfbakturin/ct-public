#include "error_message.h"
#include "macros.h"
#include "quicksort.h"
#include "return_codes.h"
#include <cstdio>
#include <cstring>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

constexpr char const *IMPL[] = { "int", "float", "phonebook" };
constexpr char const *MODE[] = { "ascending", "descending" };

int check_type(char const *p, bool const f, std::size_t *type = nullptr)
{
	int flag = 1;
	for (std::size_t i = 0; i < (f ? 3 : 2); i++)
	{
		if (std::strcmp(p, (f ? IMPL[i] : MODE[i])) == 0)
		{
			if (f)
			{
				*type = i;
			}
			flag = 0;
			break;
		}
	}
	return flag;
}

int main(int const argc, char const **argv)
{
	std::FILE *fin, *fou;
	char type[21], mode[21];
	std::size_t size = 0, current_type = 0;
	bool flag;
	void *data;

	if (argc != 3)
	{
		error_arguments_count();
		return ERROR_INVALID_PARAMETER;
	}

	fin = std::fopen(argv[1], "r");
	if (!fin)
	{
		error_file_not_found();
		return ERROR_FILE_NOT_FOUND;
	}

	std::fscanf(fin, "%9s", type);
	std::fscanf(fin, "%10s", mode);

	if (check_type(type, true, &current_type) || check_type(mode, false))
	{
		error_no_such_implementation();
		std::fclose(fin);
		return ERROR_NOT_IMPLEMENTED;
	}

	std::fscanf(fin, "%zu", &size);
	flag = std::strcmp(mode, "descending") == 0;

	MALL(current_type)
	if (!data)
	{
		error_not_enough_memory();
		std::fclose(fin);
		return ERROR_NOT_ENOUGH_MEMORY;
	}
	for (std::size_t i = 0; i != size; i++)
	{
		READ(current_type)
	}
	SORT(current_type)
	fou = std::fopen(argv[2], "w");
	if (!fou)
	{
		error_file_exists();
		std::fclose(fou);
		DELETE(current_type)
		return ERROR_NOT_ENOUGH_MEMORY;
	}
	for (std::size_t i = 0; i != size; i++)
	{
		WRITE(current_type)
	}
	std::fclose(fin);
	std::fclose(fou);
	DELETE(current_type)
	return ERROR_SUCCESS;
}
