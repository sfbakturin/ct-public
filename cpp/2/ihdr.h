#pragma once

#include "functions.h"
#include "macros.h"
#include "return_errors.h"
#include <stdio.h>
#include <stdlib.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct IHDR
{
	size_t width;
	size_t height;
	size_t bit_depth;
	size_t color_type;
	size_t compression_method;
	size_t filter_method;
	size_t interlace_method;
};

int check_ihdr(FILE *const fin, struct IHDR *ihdr, uchar const *ihdr_template)
{
	size_t chunk_size = 0;
	for (size_t index = 0; index != 4; index++)
	{
		BYTE;
		chunk_size = convert_hex(chunk_size, temp, index);
	}
	if (chunk_size != 13)
	{
		return IHDR_CORRUPTED;
	}
	for (size_t index = 0; index != 4; index++)
	{
		BYTE;
		if (ihdr_template[index] != temp)
		{
			return IHDR_CORRUPTED;
		}
	}
	for (size_t index = 0; index != 4; index++)
	{
		BYTE;
		ihdr->width = convert_hex(ihdr->width, temp, index);
	}
	for (size_t index = 0; index != 4; index++)
	{
		BYTE;
		ihdr->height = convert_hex(ihdr->height, temp, index);
	}
	if (!fread(&ihdr->bit_depth, sizeof(uchar), 1, fin))
	{
		return ERROR_READ;
	}
	if (!fread(&ihdr->color_type, sizeof(uchar), 1, fin))
	{
		return ERROR_READ;
	}
	if (!fread(&ihdr->compression_method, sizeof(uchar), 1, fin))
	{
		return ERROR_READ;
	}
	if (!fread(&ihdr->filter_method, sizeof(uchar), 1, fin))
	{
		return ERROR_READ;
	}
	if (!fread(&ihdr->interlace_method, sizeof(uchar), 1, fin))
	{
		return ERROR_READ;
	}
	for (size_t index = 0; index != 4; index++)
	{
		BYTE;
	}
	if (ihdr->bit_depth != 8 || (ihdr->color_type != 0 && ihdr->color_type != 2) || ihdr->compression_method != 0 ||
		ihdr->filter_method != 0 || ihdr->interlace_method != 0)
	{
		return IHDR_CORRUPTED;
	}
	return ERROR_OK;
}