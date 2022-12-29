#include "det.h"
#include "return_codes.h"
#include "return_errors.h"
#include <stdio.h>
#include <stdlib.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define FREE3     \
	free(args);   \
	free(deltas); \
	fclose(fin)
#define FREE4     \
	free(args);   \
	free(deltas); \
	fclose(fin);  \
	fclose(fou)

float const eps = 0.00001f;

int main(int const argc, char const **argv)
{
	FILE *fin = NULL, *fou = NULL;
	float *args = NULL, *deltas = NULL;
	size_t count = 0;
	float delta = 0;

	if (argc != 3)
	{
		error_arguments_count();
		return ERROR_INVALID_PARAMETER;
	}

	fin = fopen(argv[1], "r");
	if (!fin)
	{
		error_file_not_found();
		return ERROR_FILE_NOT_FOUND;
	}

	if (fscanf(fin, "%zu", &count) != 1)
	{
		error_invalid_data();
		return ERROR_INVALID_DATA;
	}

	args = malloc(sizeof(float) * count);
	deltas = malloc(sizeof(float) * count);
	if (!args || !deltas)
	{
		FREE3;
		error_not_enough_memory();
		return ERROR_NOT_ENOUGH_MEMORY;
	}

	for (size_t y = 0; y < count; y++)
	{
		for (size_t x = 0; x < count + 1; x++)
		{
			float temp = 0;
			fscanf(fin, "%f", &temp);
			if (x == count)
			{
				args[y] = temp;
			}
		}
	}

	if (det(0, count, args, fin, &delta, 0) != ERROR_SUCCESS)
	{
		FREE3;
		error_not_enough_memory();
		return ERROR_NOT_ENOUGH_MEMORY;
	}

	fou = fopen(argv[2], "w");
	if (!fou)
	{
		FREE3;
		error_file_exists();
		return ERROR_ALREADY_EXISTS;
	}

	for (size_t y = 0; y != count; y++)
	{
		if (det(y, count, args, fin, &deltas[y], 1) != ERROR_SUCCESS)
		{
			FREE4;
			error_not_enough_memory();
			return ERROR_NOT_ENOUGH_MEMORY;
		}
		if ((fabsf(delta) < eps) && (fabsf(deltas[y]) > eps))
		{
			fprintf(fou, "no solution\n");
			FREE4;
			return ERROR_SUCCESS;
		}
	}

	if (fabsf(delta) < eps)
	{
		fprintf(fou, "many solutions\n");
		FREE4;
		return ERROR_SUCCESS;
	}

	for (size_t y = 0; y != count; y++)
	{
		fprintf(fou, "%g\n", deltas[y] / delta);
	}

	FREE4;
	return ERROR_SUCCESS;
}
