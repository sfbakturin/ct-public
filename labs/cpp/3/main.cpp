#include "functions.h"
#include "quicksort.h"
#include "return_codes.h"
#include <cstdio>
#include <cstdlib>
#include <cstring>

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

int main(const int argc, const char **argv)
{
	if (argc != 3)
	{
		message_error_args();
		return ERROR_INVALID_PARAMETER;
	}

	FILE *const in = fopen(argv[1], "r");
	if (check(in))
	{
		message_error_file();
		return ERROR_FILE_NOT_FOUND;
	}

	char type[21], mode[21];

	fscanf(in, "%9s", &type);
	fscanf(in, "%10s", &mode);

	if (check(type, true) || check(mode, false))
	{
		message_error_impl();
		fclose(in);
		return ERROR_NOT_IMPLEMENTED;
	}

	size_t size;
	fscanf(in, "%zu", &size);
	const bool flag = strcmp(mode, "descending") == 0;

	if (strcmp(type, "int") == 0)
	{
		int *data;
		data = (int *)malloc(sizeof(int) * size);
		if (check(data))
		{
			message_error_memory();
			fclose(in);
			return ERROR_NOT_ENOUGH_MEMORY;
		}

		for (size_t i = 0; i < size; i++)
		{
			int temp;
			fscanf(in, "%i", &temp);
			data[i] = temp;
		}

		if (flag)
		{
			quicksort< int, true >(data, size);
		}
		else
		{
			quicksort< int, false >(data, size);
		}

		FILE *const out = fopen(argv[2], "w");
		if (check(out))
		{
			message_error_memory();
			fclose(in);
			free(data);
			return ERROR_NOT_ENOUGH_MEMORY;
		}

		for (size_t i = 0; i < size; i++)
		{
			fprintf(out, "%i\n", data[i]);
		}

		fclose(in);
		fclose(out);
		free(data);
	}
	else if (strcmp(type, "float") == 0)
	{
		float *data;
		data = (float *)malloc(sizeof(float) * size);
		if (check(data))
		{
			message_error_memory();
			fclose(in);
			return ERROR_NOT_ENOUGH_MEMORY;
		}

		for (size_t i = 0; i < size; i++)
		{
			float temp;
			fscanf(in, "%f", &temp);
			data[i] = temp;
		}

		if (flag)
		{
			quicksort< float, true >(data, size);
		}
		else
		{
			quicksort< float, false >(data, size);
		}

		FILE *const out = fopen(argv[2], "w");
		if (check(out))
		{
			message_error_memory();
			fclose(in);
			free(data);
			return ERROR_NOT_ENOUGH_MEMORY;
		}

		for (size_t i = 0; i < size; i++)
		{
			fprintf(out, "%g\n", data[i]);
		}

		fclose(in);
		fclose(out);
		free(data);
	}
	else if (strcmp(type, "phonebook") == 0)
	{
		phonebook *data;
		data = (phonebook *)malloc(sizeof(phonebook) * size);
		if (check(data))
		{
			message_error_memory();
			fclose(in);
			return ERROR_NOT_ENOUGH_MEMORY;
		}

		for (size_t i = 0; i < size; i++)
		{
			phonebook phonebook;
			fscanf(in, "%s %s %s %llu", &phonebook.name1, &phonebook.name2, &phonebook.name3, &phonebook.numb);
			data[i] = phonebook;
		}

		if (flag)
		{
			quicksort< phonebook, true >(data, size);
		}
		else
		{
			quicksort< phonebook, false >(data, size);
		}

		FILE *const out = fopen(argv[2], "w");
		if (check(out))
		{
			message_error_memory();
			fclose(in);
			free(data);
			return ERROR_NOT_ENOUGH_MEMORY;
		}

		for (size_t i = 0; i < size; i++)
		{
			fprintf(out, "%s %s %s %llu\n", data[i].name1, data[i].name2, data[i].name3, data[i].numb);
		}

		fclose(in);
		fclose(out);
		free(data);
	}

	return ERROR_SUCCESS;
}
