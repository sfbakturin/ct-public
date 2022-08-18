#pragma once

#include "macros.h"
#include "return_errors.h"
#include "uchar.h"
#include <stdio.h>
#include <stdlib.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

size_t convert_hex(size_t const number, uchar const shift, size_t const i)
{
	if (i == 0)
	{
		return number | (shift << 24);
	}
	else if (i == 1)
	{
		return number | (shift << 16);
	}
	else if (i == 2)
	{
		return number | (shift << 8);
	}
	else
	{
		return number | shift;
	}
}

void free_pixels(void** pixels, size_t const size)
{
	for (size_t i = 0; i != size; i++)
	{
		free(pixels[i]);
	}
	free(pixels);
	pixels = NULL;
}

int pgm_start(FILE* const file, size_t const width, size_t const height)
{
	if (fprintf(file, "\n") <= 0)
	{
		return ERROR_WRITE;
	}
	if (fprintf(file, "%zu %zu", width, height) <= 0)
	{
		return ERROR_WRITE;
	}
	if (fprintf(file, "\n") <= 0)
	{
		return ERROR_WRITE;
	}
	if (fprintf(file, "255") <= 0)
	{
		return ERROR_WRITE;
	}
	if (fprintf(file, "\n") <= 0)
	{
		return ERROR_WRITE;
	}
	return ERROR_OK;
}

int check_sig(FILE* const fin, uchar const * sig)
{
	for (size_t index = 0; index != 8; index++)
	{
		BYTE;
		if (sig[index] != temp)
		{
			return PNG_SIGNATURE;
		}
	}
	return ERROR_OK;
}