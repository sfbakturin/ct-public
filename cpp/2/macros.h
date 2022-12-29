#pragma once

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define BYTE                                  \
	uchar temp;                               \
	if (!fread(&temp, sizeof(uchar), 1, fin)) \
	{                                         \
		return ERROR_READ;                    \
	}

#define WORD                       \
	char temp;                     \
	if (!fscanf(fin, "%c", &temp)) \
	{                              \
		return ERROR_READ;         \
	}

#define PIX					  (ihdr.color_type == 2 ? sizeof(RGB) : sizeof(GS))
#define PTR					  (ihdr.color_type == 2 ? sizeof(RGB *) : sizeof(GS *))
#define SET_RGB(X, Y, C, V)	  ((RGB *)pixels[X])[Y].C = V
#define SET_GSC(X, Y, V)	  ((GS *)pixels[X])[Y].GRAY = V
#define GET_RGB(X, Y, C)	  ((RGB *)pixels[X])[Y].C
#define GET_GSC(X, Y)		  ((GS *)pixels[X])[Y].GRAY
#define GET_RGB_LEFT(X, Y, C) (X >= 1 ? GET_RGB(X - 1, Y, C) : 0)
#define GET_RGB_UP(X, Y, C)	  (Y >= 1 ? GET_RGB(X, Y - 1, C) : 0)
#define GET_RGB_DIAG(X, Y, C) (X >= 1 && Y >= 1 ? GET_RGB(X - 1, Y - 1, C) : 0)
#define GET_GSC_LEFT(X, Y)	  (X >= 1 ? GET_GSC(X - 1, Y) : 0)
#define GET_GSC_UP(X, Y)	  (Y >= 1 ? GET_GSC(X, Y - 1) : 0)
#define GET_GSC_DIAG(X, Y)	  (X >= 1 && Y >= 1 ? GET_GSC(X - 1, Y - 1) : 0)

#define FREE_PIXELS free_pixels(pixels, ihdr.height)
#define FREE_UNCOMPRESSED_COMPRESSED_FILTERS \
	free(filters);                           \
	free(compressed);                        \
	free(uncompressed)

#define CLOSE_ERROR_READ \
	error_read();        \
	return ERROR_UNKNOWN

#define CLOSE_ERROR_WRITE \
	error_write();        \
	return ERROR_UNKNOWN

#define CLOSE_ERROR_MEMORY     \
	error_not_enough_memory(); \
	return ERROR_NOT_ENOUGH_MEMORY

#define CLOSE_ERROR_DATA  \
	error_invalid_data(); \
	return ERROR_INVALID_DATA