#include "return_codes.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

int k = -1;
int error_code = 0;
const float eps = 0.00001;

float det(int num, int size, const float args[size], FILE *in);

int main(const int argc, const char **argv)
{
	if (argc != 3)
	{
		printf("ERROR: The parameter is incorrect.\nSOLUTION: Give me three arguments.\n");
		return ERROR_INVALID_PARAMETER;
	}

	FILE *const in = fopen(argv[1], "r");
	if (in == NULL)
	{
		printf("ERROR: The system cannot find the file specified.\nSOLUTION: Create file with that name.\n");
		return ERROR_FILE_NOT_FOUND;
	}

	FILE *const out = fopen(argv[2], "w");
	if (out == NULL)
	{
		printf("ERROR: Not enough storage is available to complete this operation.\nSOLUTION: Try free for me some "
			   "memory and try again.\n");
		fclose(in);
		return ERROR_OUTOFMEMORY;
	}

	int count = fscanf(in, "%i", &k);
	if (count != 1 || k <= 0)
	{
		printf("ERROR: The parameter is incorrect.\nSOLUTION: Please give me the correct form of input.\n");
		fclose(in);
		fclose(out);
		return ERROR_INVALID_PARAMETER;
	}

	float *args;
	args = malloc(sizeof(float) * k);
	if (args == NULL)
	{
		printf("ERROR: Not enough memory resources are available to process this command.\nSOLUTION: Try free for me "
			   "some memory and try again.\n");
		fclose(in);
		fclose(out);
		return ERROR_NOT_ENOUGH_MEMORY;
	}

	count = 0;

	float temp = 0;
	int ptr_y = 0, ptr_x = 0;
	while (fscanf(in, "%f", &temp) != EOF)
	{
		if (count == (k * (k + 1)))
		{
			printf("ERROR: Too many parameters to start the math.\nSOLUTION: Give me less arguments, please.\n");
			fclose(in);
			fclose(out);
			free(args);
			return ERROR_INVALID_PARAMETER;
		}
		if (ptr_x != k)
		{
			ptr_x++;
		}
		else
		{
			args[ptr_y] = temp;
			ptr_x = 0;
			ptr_y++;
		}
		count++;
	}

	if (count != (k * (k + 1)))
	{
		printf("ERROR: Too few parameters to start the math.\nSOLUTION: Give me more arguments, please.\n");
		fclose(in);
		fclose(out);
		free(args);
		return ERROR_INVALID_PARAMETER;
	}

	const float delta = det(-1, k, NULL, in);

	float *deltas;
	deltas = malloc(sizeof(float) * k);
	if (deltas == NULL)
	{
		printf("ERROR: Not enough memory resources are available to process this command.\nSOLUTION: Try free for me "
			   "some memory and try again.\n");
		fclose(in);
		fclose(out);
		free(args);
		return ERROR_NOT_ENOUGH_MEMORY;
	}

	for (int t = 0; t < k; t++)
	{
		deltas[t] = det(t, k, args, in);

		if (error_code != 0)
		{
			printf("ERROR: Not enough memory resources are available to process this command.\nSOLUTION: Try free for "
				   "me some memory and try again.\n");
			fclose(in);
			fclose(out);
			free(deltas);
			free(args);
			return error_code;
		}

		if ((fabsf(delta) < eps) && fabsf(deltas[t]) > eps)
		{
			fprintf(out, "no solution\n");
			fclose(in);
			fclose(out);
			free(deltas);
			free(args);
			return ERROR_SUCCESS;
		}
	}

	if (fabsf(delta) < eps)
	{
		fprintf(out, "many solutions\n");
		fclose(in);
		fclose(out);
		free(deltas);
		free(args);
		return ERROR_SUCCESS;
	}

	for (int y = 0; y < k; y++)
	{
		fprintf(out, "%g\n", deltas[y] / delta);
	}

	fclose(in);
	fclose(out);
	free(deltas);
	free(args);
	return ERROR_SUCCESS;
}

float det(const int num, const int size, const float args[size], FILE *const p)
{
	int o = 0;
	fseek(p, 0, SEEK_SET);
	fscanf(p, "%i", &o);

	float **clone;
	clone = malloc(sizeof(float *) * (size));
	if (clone == NULL)
	{
		error_code = ERROR_NOT_ENOUGH_MEMORY;
		return 0;
	}

	for (int y = 0; y < size; y++)
	{
		clone[y] = malloc(sizeof(float) * size);
		if (clone[y] == NULL)
		{
			for (int x = 0; x < y; x++)
			{
				free(clone[x]);
			}
			free(clone);
			error_code = ERROR_NOT_ENOUGH_MEMORY;
			return 0;
		}
	}

	for (int y = 0; y < size; y++)
	{
		for (int x = 0; x < size + 1; x++)
		{
			float input = 0;
			fscanf(p, "%f", &input);
			if (x == num)
			{
				clone[y][x] = args[y];
			}
			else
			{
				if (x != size)
				{
					clone[y][x] = input;
				}
			}
		}
	}

	int curr = 0, flag = 1;
	float sum = 1;

	while (curr != size)
	{
		int index = -1;
		for (int j = curr; j < size; j++)
		{
			if (clone[j][curr] != 0)
			{
				index = j;
				break;
			}
		}
		if (index == -1)
		{
			return 0;
		}
		if (index != curr)
		{
			const int *t = clone[index];
			clone[index] = clone[curr];
			clone[curr] = t;

			flag *= -1;

			for (int i = curr + 1; i < size; i++)
			{
				if (clone[curr][curr] != 0)
				{
					const float delta = clone[i][curr] / clone[curr][curr];
					for (int j = curr; j < size; j++)
					{
						clone[i][j] = clone[i][j] - delta * clone[curr][j];
					}
				}
			}
		}
		else
		{
			for (int i = curr + 1; i < size; i++)
			{
				if (clone[curr][curr] != 0)
				{
					const float delta = clone[i][curr] / clone[curr][curr];
					for (int j = curr; j < size; j++)
					{
						clone[i][j] = clone[i][j] - delta * clone[curr][j];
					}
				}
			}
		}
		curr++;
	}

	for (int i = 0; i < size; i++)
	{
		sum *= clone[i][i];
	}

	for (int i = 0; i < size; i++)
	{
		free(clone[i]);
	}

	free(clone);

	return sum * flag;
}
