#include "functions.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int skip(unsigned char* buffer, const size_t chunk_size, FILE* const in)
{
	for (size_t i = 0; i < chunk_size; i++)
	{
		if (next(in, buffer))
		{
			message_error_file();
			fclose(in);
			return 1;
		}
	}
	for (size_t i = 0; i < 4; i++)
	{
		if (next(in, buffer))
		{
			message_error_file();
			fclose(in);
			return 1;
		}
	}
	return 0;
}

int pgm_start(FILE* const file, const size_t width, const size_t height)
{
	if (fprintf(file, "\n") <= 0)
	{
		message_error_writer();
		return 1;
	}
	if (fprintf(file, "%zu %zu", width, height) <= 0)
	{
		message_error_writer();
		return 1;
	}
	if (fprintf(file, "\n") <= 0)
	{
		message_error_writer();
		return 1;
	}
	if (fprintf(file, "255") <= 0)
	{
		message_error_writer();
		return 1;
	}
	if (fprintf(file, "\n") <= 0)
	{
		message_error_writer();
		return 1;
	}
	return 0;
}

int check(const void* p)
{
	return p == NULL;
}

void message_error_memory()
{
	message("Not enough memory resources are available to process this command.", "Try free for me some memory and try again.");
}

void message_error_file()
{
	message("Failed to read file.", "Give me much bigger file.");
}

void message_error_png()
{
	message("It's not a PNG format file.", "Give me PNG file, please.");
}

void message_error_spec()
{
	message("This file does not conform to the PNG data standard.", "Try submitting another file to the program.");
}

void free_2ptr(unsigned char* ptr_1, unsigned char* ptr_2)
{
	free(ptr_1);
	free(ptr_2);
}

void free_3ptr(unsigned char* ptr_1, unsigned char* ptr_2, unsigned char* ptr_3)
{
	free(ptr_1);
	free(ptr_2);
	free(ptr_3);
}

int next(FILE* const file, unsigned char* buf)
{
	return fread(buf, 1, 1, file) == 0;
}

void message(const char error[], const char solution[])
{
	fprintf(stderr, "ERROR: %s\nSOLUTION: %s\n", error, solution);
}

size_t shift(const size_t num, const unsigned char buf, const int i)
{
	switch (i)
	{
	case 0:
	{
		return num | (buf << 24);
	}
	case 1:
	{
		return num | (buf << 16);
	}
	case 2:
	{
		return num | (buf << 8);
	}
	case 3:
	{
		return num | buf;
	}
	}
	return num;
}

int compare(const unsigned char* arr, const unsigned char* buf)
{
	int flag = 1;
	for (int i = 0; i < 4; i++)
	{
		if (arr[i] != buf[i])
		{
			flag = 0;
			break;
		}
	}
	return flag;
}

void message_error_writer()
{
	message("Something happened with FileWriter.", "Please, restart program.");
}

void message_error_corrupted()
{
	message("Your file is corrupted.", "Try another.");
}

void free_pix(const size_t h, void** arr)
{
	for (size_t i = 0; i < h; i++)
	{
		free(arr[i]);
	}
	free(arr);
}

int average(const int AVERAGE, const int RAW, const int PRIOR)
{
	int a = AVERAGE;
	int r = RAW;
	int p = PRIOR;
	int res = a + (int)floor((r + p + 0.0) / 2);
	return res;
}

int paeth_predictor(const int a, const int b, const int c)
{
	const int p = a + b - c;
	const int pa = abs(p - a);
	const int pb = abs(p - b);
	const int pc = abs(p - c);
	if (pa <= pb && pa <= pc)
	{
		return a;
	}
	else
	{
		if (pb <= pc)
		{
			return b;
		}
		else
		{
			return c;
		}
	}
}
