#include "functions.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define COUNT  17
#define LENGTH 4

#ifdef ZLIB
	#include <zlib.h>
#else
	#ifdef LIBDEFLATE
		#include <libdeflate.h>
	#else
		#ifdef ISAL
			#include <include/igzip_lib.h>
		#else
			#error "No library was found!"
		#endif
	#endif
#endif

typedef struct
{
	unsigned char RED, GREEN, BLUE;
} RGB;

typedef struct
{
	unsigned char GRAY;
} GS;

const unsigned char png_template[] = { 137, 80, 78, 71, 13, 10, 26, 10 };
const unsigned char ihdr_template[] = { 73, 72, 68, 82 };
const unsigned char chunk_types[COUNT][LENGTH] = {
	"PLTE", "IDAT", "IEND", "cHRM", "gAMA", "iCCP", "sBIT", "sRGB", "bKGD",
	"hIST", "tRNS", "pHYs", "sPLT", "tIME", "iTXt", "tEXt", "zTXt"
};
const unsigned char p5_template[] = { 80, 53 };
const unsigned char p6_template[] = { 80, 54 };

void free_pixels(RGB* const * pixel_rgb, GS* const * pixel, const size_t height, const size_t color_type);
int get_type(const unsigned char* str);

int main(const int argc, const char** argv)
{
	size_t status = 0;
	unsigned char *compressedIDAT, *filters, *uncompressedIDAT;
	unsigned char buffer, buffer4[4], buffer8[8];
	RGB** pixel_rgb;
	GS** pixel;
	size_t chunk_size, width, height, bit_depth, color_type, compression_method, filter_method, interlace_method,
		buf_len, buf_size, size, loop;
	size_t y, x, flag_idat, flag_iend, flag_plte, flag_idat_start;
	y = x = flag_idat = flag_iend = flag_plte = flag_idat_start = 0;
	chunk_size = width = height = bit_depth = color_type = compression_method = filter_method = interlace_method =
		buf_len = size = 0;
	buf_size = 8;
	loop = 1;

	if (argc != 3)
	{
		message("The parameter is incorrect.", "Give me three arguments.");
		return ERROR_INVALID_PARAMETER;
	}

	FILE* const in = fopen(argv[1], "rb");
	if (check(in))
	{
		message("The system cannot find the file specified.", "Create file with that name.");
		return ERROR_FILE_NOT_FOUND;
	}

	compressedIDAT = malloc(sizeof(unsigned char) * buf_size);
	if (check(compressedIDAT))
	{
		message_error_memory();
		fclose(in);
		return ERROR_NOT_ENOUGH_MEMORY;
	}

	if (fread(&buffer8, 1, 8, in) != 8)
	{
		message_error_file();
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}

	if (!compare(buffer8, png_template))
	{
		message_error_png();
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}

	for (int i = 0; i < LENGTH; i++)
	{
		if (next(in, &buffer))
		{
			message_error_file();
			fclose(in);
			free(compressedIDAT);
			return ERROR_INVALID_DATA;
		}
		chunk_size = shift(chunk_size, buffer, i);
	}
	if (chunk_size != 13)
	{
		message_error_png();
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}

	if (fread(&buffer4, 1, LENGTH, in) != LENGTH)
	{
		message_error_file();
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}

	if (!compare(buffer4, ihdr_template))
	{
		message_error_png();
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}

	for (size_t i = 0; i < LENGTH; i++)
	{
		if (next(in, &buffer))
		{
			message_error_file();
			fclose(in);
			free(compressedIDAT);
			return ERROR_INVALID_DATA;
		}
		width = shift(width, buffer, i);
	}

	for (size_t i = 0; i < LENGTH; i++)
	{
		if (next(in, &buffer))
		{
			message_error_file();
			fclose(in);
			free(compressedIDAT);
			return ERROR_INVALID_DATA;
		}
		height = shift(height, buffer, i);
	}

	if (next(in, &buffer))
	{
		message_error_file();
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}
	bit_depth = buffer;
	if (bit_depth != 8)
	{
		message("We can't decoded non-8-bit-depth png file.", "Give me 8-bit-depth file, please.");
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}

	if (next(in, &buffer))
	{
		message_error_file();
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}
	color_type = buffer;
	if (color_type != 0 && color_type != 2)
	{
		message("We can't decoded non-0-color-type or non-2-color-type png file.", "Give me 2-color-type or 0-color-type file, please.");
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}

	if (next(in, &buffer))
	{
		message_error_file();
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}
	compression_method = buffer;
	if (compression_method != 0)
	{
		message("We can't decoded non-0-compression-method png file.", "Give me 0-compression-method file, please.");
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}

	if (next(in, &buffer))
	{
		message_error_file();
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}
	filter_method = buffer;
	if (filter_method != 0)
	{
		message("We can't decoded non-0-filter-method png file.", "Give me 0-filter-method file, please.");
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}

	if (next(in, &buffer))
	{
		message_error_file();
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}
	interlace_method = buffer;
	if (interlace_method != 0)
	{
		message("We can't decoded non-0-interlace-method", "Give me 0-interlace-method, please.");
		fclose(in);
		free(compressedIDAT);
		return ERROR_INVALID_DATA;
	}

	for (size_t i = 0; i < LENGTH; i++)
	{
		if (next(in, &buffer))
		{
			message_error_file();
			fclose(in);
			free(compressedIDAT);
			return ERROR_INVALID_DATA;
		}
	}

	if (color_type == 2)
	{
		pixel_rgb = malloc(sizeof(RGB*) * height);
		if (check(pixel_rgb))
		{
			message_error_memory();
			fclose(in);
			free(compressedIDAT);
			return ERROR_NOT_ENOUGH_MEMORY;
		}
		for (size_t i = 0; i < height; i++)
		{
			pixel_rgb[i] = malloc(sizeof(RGB) * width);
			if (check(pixel_rgb[i]))
			{
				for (size_t j = 0; j < i; j++)
				{
					free(pixel_rgb[i]);
				}
				message_error_memory();
				free(pixel_rgb);
				fclose(in);
				free(compressedIDAT);
				return ERROR_NOT_ENOUGH_MEMORY;
			}
		}
	}
	else
	{
		pixel = malloc(sizeof(GS*) * height);
		if (check(pixel))
		{
			message_error_memory();
			fclose(in);
			free(compressedIDAT);
			return ERROR_NOT_ENOUGH_MEMORY;
		}
		for (size_t i = 0; i < height; i++)
		{
			pixel[i] = malloc(sizeof(GS) * width);
			if (check(pixel[i]))
			{
				for (size_t j = 0; j < i; j++)
				{
					free(pixel[i]);
				}
				message_error_memory();
				free(pixel);
				fclose(in);
				free(compressedIDAT);
				return ERROR_NOT_ENOUGH_MEMORY;
			}
		}
	}
	filters = malloc(sizeof(unsigned char) * height);
	if (check(filters))
	{
		message_error_memory();
		free_pixels(pixel_rgb, pixel, height, color_type);
		fclose(in);
		free(compressedIDAT);
		return ERROR_NOT_ENOUGH_MEMORY;
	}
	uncompressedIDAT = malloc(sizeof(unsigned char) * ((color_type + 1) * width * height + height));
	if (check(uncompressedIDAT))
	{
		message_error_memory();
		free_pixels(pixel_rgb, pixel, height, color_type);
		fclose(in);
		free_2ptr(compressedIDAT, filters);
		return ERROR_NOT_ENOUGH_MEMORY;
	}

	while (loop)
	{
		chunk_size = 0;
		for (size_t i = 0; i < LENGTH; i++)
		{
			if (next(in, &buffer))
			{
				message_error_file();
				free_pixels(pixel_rgb, pixel, height, color_type);
				fclose(in);
				free_3ptr(compressedIDAT, filters, uncompressedIDAT);
				return ERROR_INVALID_DATA;
			}
			chunk_size = shift(chunk_size, buffer, i);
		}

		for (size_t i = 0; i < LENGTH; i++)
		{
			if (next(in, &buffer))
			{
				message_error_file();
				free_pixels(pixel_rgb, pixel, height, color_type);
				fclose(in);
				free_3ptr(compressedIDAT, filters, uncompressedIDAT);
				return ERROR_INVALID_DATA;
			}
			buffer4[i] = buffer;
		}

		switch (get_type(buffer4))
		{
		case 0:
		{
			flag_idat = 0;
			if (color_type == 0)
			{
				message_error_spec();
				free_pixels(pixel_rgb, pixel, height, color_type);
				fclose(in);
				free_3ptr(compressedIDAT, filters, uncompressedIDAT);
				return ERROR_INVALID_DATA;
			}
			if (skip(&buffer, chunk_size, in))
			{
				message_error_file();
				free_pixels(pixel_rgb, pixel, height, color_type);
				free_3ptr(compressedIDAT, filters, uncompressedIDAT);
				return ERROR_INVALID_DATA;
			}
			break;
		}
		case 1:
		{
			if (flag_idat == 0 && flag_idat_start == 0)
			{
				flag_idat_start = 1;
				flag_idat = 1;
			}
			if (flag_idat == 0 && flag_idat_start == 1)
			{
				message_error_spec();
				free_pixels(pixel_rgb, pixel, height, color_type);
				fclose(in);
				free_3ptr(compressedIDAT, filters, uncompressedIDAT);
				return ERROR_INVALID_DATA;
			}
			for (size_t i = 0; i < chunk_size; i++)
			{
				if (next(in, &buffer))
				{
					message_error_file();
					free_pixels(pixel_rgb, pixel, height, color_type);
					fclose(in);
					free_3ptr(compressedIDAT, filters, uncompressedIDAT);
					return ERROR_INVALID_DATA;
				}
				if (buf_len >= buf_size)
				{
					const size_t new_size = sizeof(unsigned char) * buf_size * 2;
					unsigned char* p = realloc(compressedIDAT, new_size);
					if (check(p))
					{
						message_error_memory();
						free_pixels(pixel_rgb, pixel, height, color_type);
						fclose(in);
						free_3ptr(compressedIDAT, filters, uncompressedIDAT);
						return ERROR_INVALID_DATA;
					}
					buf_size = new_size;
					compressedIDAT = p;
				}
				compressedIDAT[buf_len++] = buffer;
			}
			for (size_t i = 0; i < LENGTH; i++)
			{
				if (next(in, &buffer))
				{
					message_error_file();
					free_pixels(pixel_rgb, pixel, height, color_type);
					fclose(in);
					free_3ptr(compressedIDAT, filters, uncompressedIDAT);
					return ERROR_INVALID_DATA;
				}
			}
			break;
		}
		case 2:
		{
			if (chunk_size != 0)
			{
				message_error_spec();
				free_pixels(pixel_rgb, pixel, height, color_type);
				fclose(in);
				free_3ptr(compressedIDAT, filters, uncompressedIDAT);
				return ERROR_INVALID_DATA;
			}
			flag_iend = 1;
			flag_idat = 0;
			loop = 0;
			break;
		}
		case -1:
		{
			message_error_spec();
			free_pixels(pixel_rgb, pixel, height, color_type);
			fclose(in);
			free_3ptr(compressedIDAT, filters, uncompressedIDAT);
			return ERROR_INVALID_DATA;
		}
		default:
		{
			flag_idat = 0;
			if (skip(&buffer, chunk_size, in))
			{
				message_error_file();
				free_pixels(pixel_rgb, pixel, height, color_type);
				free_3ptr(compressedIDAT, filters, uncompressedIDAT);
				return ERROR_INVALID_DATA;
			}
			break;
		}
		}
	}

	if (!(flag_iend && flag_idat_start))
	{
		message_error_file();
		free_pixels(pixel_rgb, pixel, height, color_type);
		fclose(in);
		free_3ptr(compressedIDAT, filters, uncompressedIDAT);
		return ERROR_INVALID_DATA;
	}

	for (size_t i = 0; i < LENGTH; i++)
	{
		if (next(in, &buffer))
		{
			message_error_file();
			free_pixels(pixel_rgb, pixel, height, color_type);
			fclose(in);
			free_3ptr(compressedIDAT, filters, uncompressedIDAT);
			return ERROR_INVALID_DATA;
		}
	}

	if (fread(&buffer, 1, 1, in) != 0)
	{
		message_error_file();
		free_pixels(pixel_rgb, pixel, height, color_type);
		fclose(in);
		free_3ptr(compressedIDAT, filters, uncompressedIDAT);
		return ERROR_INVALID_DATA;
	}

	fclose(in);

	size = ((color_type + 1) * width * height + height);

#ifdef ISAL
	struct inflate_state state;
	isal_inflate_init(&state);
	state.crc_flag = ISAL_ZLIB;
	state.next_in = compressedIDAT;
	state.next_out = uncompressedIDAT;
	state.avail_in = buf_len + 1;
	state.avail_out = size;
	status = isal_inflate_stateless(&state);
	if (status != ISAL_DECOMP_OK && status != ISAL_END_INPUT)
	{
		message_error_corrupted();
		free_pixels(pixel_rgb, pixel, height, color_type);
		free_3ptr(compressedIDAT, filters, uncompressedIDAT);
		return ERROR_NOT_ENOUGH_MEMORY;
	}
#endif

#ifdef LIBDEFLATE
	struct libdeflate_decompressor* decompressor = libdeflate_alloc_decompressor();
	if (libdeflate_zlib_decompress(decompressor, compressedIDAT, buf_len + 1, uncompressedIDAT, ((color_type + 1) * width * height + height), &size) !=
		LIBDEFLATE_SUCCESS)
	{
		message_error_corrupted();
		libdeflate_free_decompressor(decompressor);
		free_pixels(pixel_rgb, pixel, height, color_type);
		free_3ptr(compressedIDAT, filters, uncompressedIDAT);
		return ERROR_NOT_ENOUGH_MEMORY;
	}
	libdeflate_free_decompressor(decompressor);
#endif

#ifdef ZLIB
	if (uncompress(uncompressedIDAT, (uLongf*)&size, compressedIDAT, buf_len + 1) != Z_OK)
	{
		message_error_corrupted();
		free_pixels(pixel_rgb, pixel, height, color_type);
		free_3ptr(compressedIDAT, filters, uncompressedIDAT);
		return ERROR_NOT_ENOUGH_MEMORY;
	}
#endif

	size = ((color_type + 1) * width * height + height);
	short color = 0;
	y = -1;
	for (size_t i = 0; i < size; i++)
	{
		if (i % (width * (color_type + 1) + 1) == 0)
		{
			y++;
			filters[y] = uncompressedIDAT[i];
			x = 0;
		}
		else
		{
			if (color_type == 2)
			{
				switch (color)
				{
				case 0:
				{
					pixel_rgb[y][x].RED = uncompressedIDAT[i];
					color = 1;
					break;
				}
				case 1:
				{
					pixel_rgb[y][x].GREEN = uncompressedIDAT[i];
					color = 2;
					break;
				}
				case 2:
				{
					pixel_rgb[y][x].BLUE = uncompressedIDAT[i];
					x++;
					color = 0;
					break;
				}
				}
			}
			else
			{
				pixel[y][x++].GRAY = uncompressedIDAT[i];
			}
		}
	}

	free_2ptr(uncompressedIDAT, compressedIDAT);

	for (size_t i = 0; i < height; i++)
	{
		const unsigned char current = filters[i];
		switch (current)
		{
		case 0:
		{
			break;
		}
		case 1:
		{
			for (size_t j = 0; j < width; j++)
			{
				if (j > 0)
				{
					if (color_type == 2)
					{
						const unsigned char red_sub = pixel_rgb[i][j].RED;
						const unsigned char blue_sub = pixel_rgb[i][j].BLUE;
						const unsigned char green_sub = pixel_rgb[i][j].GREEN;
						const unsigned char red_raw = pixel_rgb[i][j - 1].RED;
						const unsigned char blue_raw = pixel_rgb[i][j - 1].BLUE;
						const unsigned char green_raw = pixel_rgb[i][j - 1].GREEN;
						const RGB rgb = { .RED = (unsigned char)((red_sub + red_raw) % 256),
										  .BLUE = (unsigned char)((blue_sub + blue_raw) % 256),
										  .GREEN = (unsigned char)((green_sub + green_raw) % 256) };
						pixel_rgb[i][j] = rgb;
					}
					else
					{
						const unsigned char sub = pixel[i][j].GRAY;
						const unsigned char raw = pixel[i][j - 1].GRAY;
						const GS gs = { .GRAY = (unsigned char)((sub + raw) % 256) };
						pixel[i][j] = gs;
					}
				}
				else
				{
					if (color_type == 2)
					{
						const unsigned char red_sub = pixel_rgb[i][j].RED;
						const unsigned char blue_sub = pixel_rgb[i][j].BLUE;
						const unsigned char green_sub = pixel_rgb[i][j].GREEN;
						const RGB rgb = { .RED = (unsigned char)((red_sub + 0) % 256),
										  .BLUE = (unsigned char)((0 + blue_sub) % 256),
										  .GREEN = (unsigned char)((0 + green_sub) % 256) };
						pixel_rgb[i][j] = rgb;
					}
					else
					{
						const unsigned char sub = pixel[i][j].GRAY;
						const GS gs = { .GRAY = (unsigned char)((sub + 0) % 256) };
						pixel[i][j] = gs;
					}
				}
			}
			break;
		}
		case 2:
		{
			for (size_t j = 0; j < width; j++)
			{
				if (i > 0)
				{
					if (color_type == 2)
					{
						const unsigned char red_up = pixel_rgb[i][j].RED;
						const unsigned char blue_up = pixel_rgb[i][j].BLUE;
						const unsigned char green_up = pixel_rgb[i][j].GREEN;
						const unsigned char red_prior = pixel_rgb[i - 1][j].RED;
						const unsigned char blue_prior = pixel_rgb[i - 1][j].BLUE;
						const unsigned char green_prior = pixel_rgb[i - 1][j].GREEN;
						const RGB rgb = { .RED = (unsigned char)((red_up + red_prior) % 256),
										  .GREEN = (unsigned char)((green_up + green_prior) % 256),
										  .BLUE = (unsigned char)((blue_up + blue_prior) % 256) };
						pixel_rgb[i][j] = rgb;
					}
					else
					{
						const unsigned char up = pixel[i][j].GRAY;
						const unsigned char prior = pixel[i - 1][j].GRAY;
						const GS gs = { .GRAY = (unsigned char)((up + prior) % 256) };
						pixel[i][j] = gs;
					}
				}
				else
				{
					if (color_type == 2)
					{
						const unsigned char red_up = pixel_rgb[i][j].RED;
						const unsigned char blue_up = pixel_rgb[i][j].BLUE;
						const unsigned char green_up = pixel_rgb[i][j].GREEN;
						const RGB rgb = { .RED = (unsigned char)((0 + red_up) % 256),
										  .GREEN = (unsigned char)((0 + green_up) % 256),
										  .BLUE = (unsigned char)((0 + blue_up) % 256) };
						pixel_rgb[i][j] = rgb;
					}
					else
					{
						const unsigned char up = pixel[i][j].GRAY;
						const GS gs = { .GRAY = (unsigned char)((up + 0) % 256) };
						pixel[i][j] = gs;
					}
				}
			}
			break;
		}
		case 3:
		{
			for (size_t j = 0; j < width; j++)
			{
				if (i > 0 && j > 0)
				{
					if (color_type == 2)
					{
						const unsigned char red_prior = pixel_rgb[i - 1][j].RED;
						const unsigned char blue_prior = pixel_rgb[i - 1][j].BLUE;
						const unsigned char green_prior = pixel_rgb[i - 1][j].GREEN;
						const unsigned char red_raw = pixel_rgb[i][j - 1].RED;
						const unsigned char blue_raw = pixel_rgb[i][j - 1].BLUE;
						const unsigned char green_raw = pixel_rgb[i][j - 1].GREEN;
						const unsigned char red_average = pixel_rgb[i][j].RED;
						const unsigned char blue_average = pixel_rgb[i][j].BLUE;
						const unsigned char green_average = pixel_rgb[i][j].GREEN;
						const RGB rgb = { .RED = average(red_average, red_raw, red_prior),
										  .GREEN = average(green_average, green_raw, green_prior),
										  .BLUE = average(blue_average, blue_raw, blue_prior) };
						pixel_rgb[i][j] = rgb;
						continue;
					}
					else
					{
						const unsigned char prior = pixel[i - 1][j].GRAY;
						const unsigned char raw = pixel[i][j - 1].GRAY;
						const unsigned char g_average = pixel[i][j].GRAY;
						const GS gs = { .GRAY = average(g_average, raw, prior) };
						pixel[i][j] = gs;
					}
				}
				if (i > 0 && j < 1)
				{
					if (color_type == 2)
					{
						const unsigned char red_prior = pixel_rgb[i - 1][j].RED;
						const unsigned char blue_prior = pixel_rgb[i - 1][j].BLUE;
						const unsigned char green_prior = pixel_rgb[i - 1][j].GREEN;
						const unsigned char red_average = pixel_rgb[i][j].RED;
						const unsigned char blue_average = pixel_rgb[i][j].BLUE;
						const unsigned char green_average = pixel_rgb[i][j].GREEN;
						const RGB rgb = { .RED = average(red_average, 0, red_prior),
										  .GREEN = average(green_average, 0, green_prior),
										  .BLUE = average(blue_average, 0, blue_prior) };
						pixel_rgb[i][j] = rgb;
						continue;
					}
					else
					{
						const unsigned char prior = pixel[i - 1][j].GRAY;
						const unsigned char g_average = pixel[i][j].GRAY;
						const GS gs = { .GRAY = average(g_average, 0, prior) };
						pixel[i][j] = gs;
					}
				}
				if (i < 1 && j > 0)
				{
					if (color_type == 2)
					{
						const unsigned char red_raw = pixel_rgb[i][j - 1].RED;
						const unsigned char blue_raw = pixel_rgb[i][j - 1].BLUE;
						const unsigned char green_raw = pixel_rgb[i][j - 1].GREEN;
						const unsigned char red_average = pixel_rgb[i][j].RED;
						const unsigned char blue_average = pixel_rgb[i][j].BLUE;
						const unsigned char green_average = pixel_rgb[i][j].GREEN;
						const RGB rgb = { .RED = average(red_average, red_raw, 0),
										  .GREEN = average(green_average, green_raw, 0),
										  .BLUE = average(blue_average, blue_raw, 0) };
						pixel_rgb[i][j] = rgb;
						continue;
					}
					else
					{
						const unsigned char raw = pixel[i][j - 1].GRAY;
						const unsigned char g_average = pixel[i][j].GRAY;
						const GS gs = { .GRAY = average(g_average, raw, 0) };
						pixel[i][j] = gs;
					}
				}
				if (i < 1 && j < 1)
				{
					if (color_type == 2)
					{
						const unsigned char red_average = pixel_rgb[i][j].RED;
						const unsigned char blue_average = pixel_rgb[i][j].BLUE;
						const unsigned char green_average = pixel_rgb[i][j].GREEN;
						const RGB rgb = { .RED = average(red_average, 0, 0),
										  .GREEN = average(green_average, 0, 0),
										  .BLUE = average(blue_average, 0, 0) };
						pixel_rgb[i][j] = rgb;
						continue;
					}
					else
					{
						const unsigned char g_average = pixel[i][j].GRAY;
						const GS gs = { .GRAY = average(g_average, 0, 0) };
						pixel[i][j] = gs;
					}
				}
			}
			break;
		}
		case 4:
		{
			for (size_t j = 0; j < width; j++)
			{
				if (i > 0 && j > 0)
				{
					if (color_type == 2)
					{
						const unsigned char red_paeth = pixel_rgb[i][j].RED;
						const unsigned char blue_paeth = pixel_rgb[i][j].BLUE;
						const unsigned char green_paeth = pixel_rgb[i][j].GREEN;
						const unsigned char red_raw = pixel_rgb[i][j - 1].RED;
						const unsigned char blue_raw = pixel_rgb[i][j - 1].BLUE;
						const unsigned char green_raw = pixel_rgb[i][j - 1].GREEN;
						const unsigned char red_prior = pixel_rgb[i - 1][j].RED;
						const unsigned char blue_prior = pixel_rgb[i - 1][j].BLUE;
						const unsigned char green_prior = pixel_rgb[i - 1][j].GREEN;
						const unsigned char red_raw_prior = pixel_rgb[i - 1][j - 1].RED;
						const unsigned char blue_raw_prior = pixel_rgb[i - 1][j - 1].BLUE;
						const unsigned char green_raw_prior = pixel_rgb[i - 1][j - 1].GREEN;
						const RGB rgb = {
							.RED = (unsigned char)((red_paeth + paeth_predictor(red_raw, red_prior, red_raw_prior)) % 256),
							.GREEN = (unsigned char)((green_paeth + paeth_predictor(green_raw, green_prior, green_raw_prior)) % 256),
							.BLUE = (unsigned char)((blue_paeth + paeth_predictor(blue_raw, blue_prior, blue_raw_prior)) % 256)
						};
						pixel_rgb[i][j] = rgb;
						continue;
					}
					else
					{
						const unsigned char paeth = pixel[i][j].GRAY;
						const unsigned char raw = pixel[i][j - 1].GRAY;
						const unsigned char prior = pixel[i - 1][j].GRAY;
						const unsigned char raw_prior = pixel[i - 1][j - 1].GRAY;
						const GS gs = { .GRAY = (unsigned char)((paeth + paeth_predictor(raw, prior, raw_prior)) % 256) };
						pixel[i][j] = gs;
					}
				}
				if (i < 1 && j > 0)
				{
					if (color_type == 2)
					{
						const unsigned char red_paeth = pixel_rgb[i][j].RED;
						const unsigned char blue_paeth = pixel_rgb[i][j].BLUE;
						const unsigned char green_paeth = pixel_rgb[i][j].GREEN;
						const unsigned char red_raw = pixel_rgb[i][j - 1].RED;
						const unsigned char blue_raw = pixel_rgb[i][j - 1].BLUE;
						const unsigned char green_raw = pixel_rgb[i][j - 1].GREEN;
						const RGB rgb = { .RED = (unsigned char)((red_paeth + paeth_predictor(red_raw, 0, 0)) % 256),
										  .GREEN = (unsigned char)((green_paeth + paeth_predictor(green_raw, 0, 0)) % 256),
										  .BLUE = (unsigned char)((blue_paeth + paeth_predictor(blue_raw, 0, 0)) % 256) };
						pixel_rgb[i][j] = rgb;
						continue;
					}
					else
					{
						const unsigned char paeth = pixel[i][j].GRAY;
						const unsigned char raw = pixel[i][j - 1].GRAY;
						const GS gs = { .GRAY = (unsigned char)((paeth + paeth_predictor(raw, 0, 0)) % 256) };
						pixel[i][j] = gs;
					}
				}
				if (i > 0 && j < 1)
				{
					if (color_type == 2)
					{
						const unsigned char red_paeth = pixel_rgb[i][j].RED;
						const unsigned char blue_paeth = pixel_rgb[i][j].BLUE;
						const unsigned char green_paeth = pixel_rgb[i][j].GREEN;
						const unsigned char red_prior = pixel_rgb[i - 1][j].RED;
						const unsigned char blue_prior = pixel_rgb[i - 1][j].BLUE;
						const unsigned char green_prior = pixel_rgb[i - 1][j].GREEN;
						const RGB rgb = {
							.RED = (unsigned char)((red_paeth + paeth_predictor(0, red_prior, 0)) % 256),
							.GREEN = (unsigned char)((green_paeth + paeth_predictor(0, green_prior, 0)) % 256),
							.BLUE = (unsigned char)((blue_paeth + paeth_predictor(0, blue_prior, 0)) % 256)
						};
						pixel_rgb[i][j] = rgb;
						continue;
					}
					else
					{
						const unsigned char paeth = pixel[i][j].GRAY;
						const unsigned char prior = pixel[i - 1][j].GRAY;
						const GS gs = { .GRAY = (unsigned char)((paeth + paeth_predictor(0, prior, 0)) % 256) };
						pixel[i][j] = gs;
					}
				}
				if (i < 1 && j < 1)
				{
					if (color_type == 2)
					{
						const unsigned char red_paeth = pixel_rgb[i][j].RED;
						const unsigned char blue_paeth = pixel_rgb[i][j].BLUE;
						const unsigned char green_paeth = pixel_rgb[i][j].GREEN;
						const RGB rgb = { .RED = (unsigned char)((red_paeth + paeth_predictor(0, 0, 0)) % 256),
										  .GREEN = (unsigned char)((green_paeth + paeth_predictor(0, 0, 0)) % 256),
										  .BLUE = (unsigned char)((blue_paeth + paeth_predictor(0, 0, 0)) % 256) };
						pixel_rgb[i][j] = rgb;
						continue;
					}
					else
					{
						const unsigned char paeth = pixel[i][j].GRAY;
						const GS gs = { .GRAY = (unsigned char)((paeth + paeth_predictor(0, 0, 0)) % 256) };
						pixel[i][j] = gs;
					}
				}
			}
			break;
		}
		default:
		{
			message_error_spec();
			free_pixels(pixel_rgb, pixel, height, color_type);
			free(filters);
			return ERROR_INVALID_DATA;
		}
		}
	}

	free(filters);

	FILE* const out = fopen(argv[2], "wb");
	if (check(out))
	{
		message_error_memory();
		free_pixels(pixel_rgb, pixel, height, color_type);
		return ERROR_OUTOFMEMORY;
	}

	if (color_type == 2)
	{
		if (fwrite(p6_template, 1, 2, out) != 2)
		{
			message_error_writer();
			fclose(out);
			free_pixels(pixel_rgb, pixel, height, color_type);
			return ERROR_UNKNOWN;
		}
	}
	else
	{
		if (fwrite(p5_template, 1, 2, out) != 2)
		{
			message_error_writer();
			fclose(out);
			free_pixels(pixel_rgb, pixel, height, color_type);
			return ERROR_UNKNOWN;
		}
	}

	if (pgm_start(out, width, height))
	{
		message_error_writer();
		fclose(out);
		free_pixels(pixel_rgb, pixel, height, color_type);
		return ERROR_UNKNOWN;
	}

	for (size_t i = 0; i < height; i++)
	{
		for (size_t j = 0; j < width; j++)
		{
			if (color_type == 2)
			{
				const unsigned char output[] = { pixel_rgb[i][j].RED, pixel_rgb[i][j].GREEN, pixel_rgb[i][j].BLUE };
				if (fwrite(output, 1, 3, out) != 3)
				{
					message_error_writer();
					fclose(out);
					free_pixels(pixel_rgb, pixel, height, color_type);
					return ERROR_UNKNOWN;
				}
			}
			else
			{
				const unsigned char output[] = { pixel[i][j].GRAY };
				if (fwrite(output, 1, 1, out) != 1)
				{
					message_error_writer();
					fclose(out);
					free_pixels(pixel_rgb, pixel, height, color_type);
					return ERROR_UNKNOWN;
				}
			}
		}
	}

	free_pixels(pixel_rgb, pixel, height, color_type);
	fclose(out);
	return ERROR_SUCCESS;
}

void free_pixels(RGB* const * pixel_rgb, GS* const * pixel, const size_t height, const size_t color_type)
{
	if (color_type == 2)
	{
		free_pix(height, (void**)pixel_rgb);
	}
	else
	{
		free_pix(height, (void**)pixel);
	}
}

int get_type(const unsigned char* str)
{
	for (size_t i = 0; i < COUNT; i++)
	{
		int temp = 1;
		for (size_t j = 0; j < LENGTH; j++)
		{
			temp &= (str[j] == chunk_types[i][j]);
		}
		if (temp)
		{
			return i;
		}
	}
	return -1;
}
