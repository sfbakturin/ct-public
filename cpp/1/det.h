#pragma once

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#include "return_codes.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

int det(size_t const number, size_t const size, float const *args, FILE *const fin, float *result, bool const flag)
{
	fseek(fin, 0, SEEK_SET);
	fscanf(fin, "%*zu");

	float **matrix = malloc(sizeof(float *) * size);
	if (!matrix)
	{
		return ERROR_NOT_ENOUGH_MEMORY;
	}

	for (size_t y = 0; y != size; y++)
	{
		matrix[y] = malloc(sizeof(float) * size);
		if (!matrix[y])
		{
			for (size_t x = 0; x != y; x++)
			{
				free(matrix[x]);
			}
			free(matrix);
			return ERROR_NOT_ENOUGH_MEMORY;
		}
	}

	for (size_t y = 0; y != size; y++)
	{
		for (size_t x = 0; x != size + 1; x++)
		{
			float temp = 0;
			fscanf(fin, "%f", &temp);
			if (x == number && flag)
			{
				matrix[y][x] = args[y];
			}
			else
			{
				if (x != size)
				{
					matrix[y][x] = temp;
				}
			}
		}
	}

	int negate = 1;
	size_t current = 0;
	float sum = 1;

	while (current != size)
	{
		bool changed = 0;
		size_t index;
		for (size_t y = current; y != size; y++)
		{
			if (fabsf(matrix[y][current]) >= 0)
			{
				index = y;
				changed = 1;
				break;
			}
		}
		if (!changed)
		{
			*result = 0;
			return ERROR_SUCCESS;
		}
		if (index != current)
		{
			float *t = matrix[index];
			matrix[index] = matrix[current];
			matrix[current] = t;
			negate *= -1;
		}
		for (size_t y = current + 1; y != size; y++)
		{
			if (fabsf(matrix[current][current]) >= 0)
			{
				double const delta = matrix[y][current] / matrix[current][current];
				for (size_t x = current; x != size; x++)
				{
					matrix[y][x] = matrix[y][x] - (float)(delta * matrix[current][x]);
				}
			}
		}
		current++;
	}

	for (size_t y = 0; y != size; y++)
	{
		sum *= matrix[y][y];
	}

	for (size_t y = 0; y != size; y++)
	{
		free(matrix[y]);
	}
	free(matrix);

	*result = sum * negate;
	return ERROR_SUCCESS;
}
