#pragma once

#include "functions.h"
#include "macros.h"
#include "return_chunk.h"
#include "return_errors.h"
#include "uchar.h"
#include <stdio.h>
#include <string.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int get_length(FILE *const fin, size_t *result)
{
	for (size_t i = 0; i != 4; i++)
	{
		BYTE;
		*result = convert_hex(*result, temp, i);
	}
	return ERROR_OK;
}

int get_name(FILE *const fin, char *name)
{
	for (size_t i = 0; i != 4; i++)
	{
		WORD;
		name[i] = temp;
	}
	return ERROR_OK;
}

/*int get_data(FILE *const fin, uchar *data, size_t *size, size_t *capacity, size_t const chunk_size)
{
	for (size_t i = 0; i != chunk_size; i++)
	{
		BYTE;
		if ((*size) >= (*capacity))
		{
			size_t const new_size = sizeof(uchar) * 2 * (*capacity);
			uchar *p = realloc(data, new_size);
			if (!p)
			{
				return ERROR_MEMO;
			}
			*capacity = new_size;
			data = p;
		}
		data[(*size)] = temp;
		(*size) = (*size) + 1;
	}
	return ERROR_OK;
}*/

int skip(FILE *const fin, size_t const size)
{
	for (size_t i = 0; i != size; i++)
	{
		BYTE
	}
	return ERROR_OK;
}

int skip_crc(FILE *const fin)
{
	return skip(fin, 4);
}

int skip_chunk(FILE *const fin, size_t const size)
{
	if (skip(fin, size))
	{
		return ERROR_READ;
	}
	if (skip_crc(fin))
	{
		return ERROR_READ;
	}
	return ERROR_OK;
}
