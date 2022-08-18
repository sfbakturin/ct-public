#include "chunk.h"
#include "error_message.h"
#include "filter.h"
#include "gs.h"
#include "ihdr.h"
#include "macros.h"
#include "return_chunk.h"
#include "return_codes.h"
#include "rgb.h"
#include "uchar.h"
#include <stdbool.h>
#include <stdio.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

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

uchar const png_template[] = { 137, 80, 78, 71, 13, 10, 26, 10 };
uchar const ihdr_template[] = { 73, 72, 68, 82 };
uchar const chunk_critical[4][4] = { "IHDR", "PLTE", "IDAT", "IEND" };
uchar const chunk_ancillary[14][4] = {
	"cHRM", "gAMA", "iCCP", "sBIT", "sRGB", "bKGD", "hIST", "tRNS", "pHYs", "sPLT", "tIME", "iTXt", "tEXt", "zTXt"
};
uchar const p5_template[] = { 80, 53 };
uchar const p6_template[] = { 80, 54 };

int get_type(char const *const name)
{
	int type = CHUNK_NONSTANDARD;
	for (int i = 0; i != 4; i++)
	{
		bool temp = true;
		for (size_t j = 0; j != 4; j++)
		{
			temp &= (name[j] == chunk_critical[i][j]);
		}
		if (temp)
		{
			type = i;
		}
	}
	for (int i = 0; i != 14; i++)
	{
		bool temp = true;
		for (size_t j = 0; j != 4; j++)
		{
			temp &= (name[j] == chunk_ancillary[i][j]);
		}
		if (temp)
		{
			type = CHUNK_ANCILLARY;
		}
	}
	return type;
}

int main(int const argc, char const **argv)
{
	FILE *fin = NULL, *fou = NULL;
	struct IHDR ihdr = { 0, 0, 0, 0, 0, 0, 0 };
	void **pixels = NULL;
	uchar *filters = NULL, *compressed = NULL, *uncompressed = NULL;
	bool loop = true, flag_idat = false, flag_idat_start = false, flag_iend = false;
	size_t size = 0, capacity = 2, uncompressed_size, pos_y = 0, pos_x = 0, color = 0;
	uchar buffer = 0;

	if (argc != 3)
	{
		error_arguments_count();
		return ERROR_INVALID_PARAMETER;
	}

	fin = fopen(argv[1], "rb");
	if (!fin)
	{
		error_file_not_found();
		return ERROR_FILE_NOT_FOUND;
	}

	switch (check_sig(fin, png_template))
	{
	case ERROR_READ:
	{
		fclose(fin);
		CLOSE_ERROR_READ;
	}
	case PNG_SIGNATURE:
	{
		fclose(fin);
		CLOSE_ERROR_DATA;
	}
	}

	switch (check_ihdr(fin, &ihdr, ihdr_template))
	{
	case ERROR_READ:
	{
		fclose(fin);
		CLOSE_ERROR_READ;
	}
	case IHDR_CORRUPTED:
	{
		fclose(fin);
		CLOSE_ERROR_DATA;
	}
	}

	pixels = malloc(sizeof(PTR) * ihdr.height);
	if (!pixels)
	{
		fclose(fin);
		CLOSE_ERROR_MEMORY;
	}
	for (size_t y = 0; y != ihdr.height; y++)
	{
		pixels[y] = malloc(sizeof(PIX) * ihdr.width);
		if (!pixels[y])
		{
			fclose(fin);
			free_pixels(pixels, y);
			CLOSE_ERROR_MEMORY;
		}
	}

	filters = malloc(sizeof(uchar) * ihdr.height);
	if (!filters)
	{
		fclose(fin);
		FREE_PIXELS;
		CLOSE_ERROR_MEMORY;
	}

	uncompressed_size = ((ihdr.color_type + 1) * ihdr.width * ihdr.height + ihdr.height);
	uncompressed = malloc(sizeof(uchar) * uncompressed_size);
	if (!uncompressed)
	{
		fclose(fin);
		FREE_PIXELS;
		free(filters);
		CLOSE_ERROR_MEMORY;
	}

	compressed = malloc(sizeof(uchar) * capacity);
	if (!compressed)
	{
		fclose(fin);
		FREE_PIXELS;
		free(filters);
		free(uncompressed);
		CLOSE_ERROR_MEMORY;
	}

	while (loop)
	{
		char name[4];
		size_t chunk_size = 0;
		if (get_length(fin, &chunk_size))
		{
			fclose(fin);
			FREE_PIXELS;
			FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
			CLOSE_ERROR_READ;
		}
		if (get_name(fin, name))
		{
			fclose(fin);
			FREE_PIXELS;
			FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
			CLOSE_ERROR_READ;
		}
		switch (get_type(name))
		{
		case CHUNK_IDHR:
		{
			fclose(fin);
			FREE_PIXELS;
			FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
			CLOSE_ERROR_DATA;
		}
		case CHUNK_PLTE:
		{
			flag_idat = false;
			if (!ihdr.color_type)
			{
				fclose(fin);
				FREE_PIXELS;
				FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
				CLOSE_ERROR_DATA;
			}
			if (skip_chunk(fin, chunk_size))
			{
				fclose(fin);
				FREE_PIXELS;
				FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
				CLOSE_ERROR_READ;
			}
			break;
		}
		case CHUNK_IDAT:
		{
			if (!flag_idat && !flag_idat_start)
			{
				flag_idat = true;
				flag_idat_start = true;
			}
			if (!flag_idat && flag_idat_start)
			{
				fclose(fin);
				FREE_PIXELS;
				FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
				CLOSE_ERROR_DATA;
			}
			for (size_t i = 0; i != chunk_size; i++)
			{
				uchar temp;
				if (!fread(&temp, sizeof(uchar), 1, fin))
				{
					fclose(fin);
					FREE_PIXELS;
					FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
					CLOSE_ERROR_READ;
				}
				if (size >= capacity)
				{
					size_t const new_size = sizeof(uchar) * 2 * capacity;
					uchar *p = realloc(compressed, new_size);
					if (!p)
					{
						fclose(fin);
						FREE_PIXELS;
						FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
						CLOSE_ERROR_MEMORY;
					}
					capacity = new_size;
					compressed = p;
				}
				compressed[size++] = temp;
			}
			/*switch (get_data(fin, compressed, &size, &capacity, chunk_size))
			{
			case ERROR_READ:
			{
				fclose(fin);
				FREE_PIXELS;
				FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
				CLOSE_ERROR_READ;
			}
			case ERROR_MEMO:
			{
				fclose(fin);
				FREE_PIXELS;
				FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
				CLOSE_ERROR_MEMORY;
			}
			}
			*/
			if (skip_crc(fin))
			{
				fclose(fin);
				FREE_PIXELS;
				FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
				CLOSE_ERROR_READ;
			}
			break;
		}
		case CHUNK_IEND:
		{
			if (chunk_size)
			{
				fclose(fin);
				FREE_PIXELS;
				FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
				CLOSE_ERROR_DATA;
			}
			flag_idat = false;
			flag_iend = true;
			loop = false;
			break;
		}
		case CHUNK_ANCILLARY:
		{
			flag_idat = false;
			if (skip_chunk(fin, chunk_size))
			{
				fclose(fin);
				FREE_PIXELS;
				FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
				CLOSE_ERROR_READ;
			}
			break;
		}
		case CHUNK_NONSTANDARD:
		{
			fclose(fin);
			FREE_PIXELS;
			FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
			CLOSE_ERROR_DATA;
		}
		}
	}

	if (!(flag_iend && flag_idat_start) || skip_crc(fin))
	{
		fclose(fin);
		FREE_PIXELS;
		FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
		CLOSE_ERROR_DATA;
	}

	if (fread(&buffer, 1, 1, fin))
	{
		fclose(fin);
		FREE_PIXELS;
		FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
		CLOSE_ERROR_DATA;
	}

	fclose(fin);

#ifdef ZLIB
	if (uncompress(uncompressed, (uLongf *)&uncompressed_size, compressed, size + 1) != Z_OK)
	{
		FREE_PIXELS;
		FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
		CLOSE_ERROR_MEMORY;
	}
#endif

#ifdef LIBDEFLATE
	struct libdeflate_decompressor *decompressor = libdeflate_alloc_decompressor();
	if (libdeflate_zlib_decompress(decompressor, compressed, size + 1, uncompressed, uncompressed_size, &uncompressed_size) != LIBDEFLATE_SUCCESS)
	{
		libdeflate_free_decompressor(decompressor);
		FREE_PIXELS;
		FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
		CLOSE_ERROR_MEMORY;
	}
#endif

#ifdef ISAL
	struct inflate_state state;
	isal_inflate_init(&state);
	state.crc_flag = ISAL_ZLIB;
	state.next_in = compressed;
	state.next_out = uncompressed;
	state.avail_in = size + 1;
	state.avail_out = uncompressed_size;
	int status = isal_inflate_stateless(&state);
	if (status != ISAL_DECOMP_OK && status != ISAL_END_INPUT)
	{
		FREE_PIXELS;
		FREE_UNCOMPRESSED_COMPRESSED_FILTERS;
		CLOSE_ERROR_MEMORY;
	}
#endif

	pos_y = -1;
	for (size_t i = 0; i != uncompressed_size; i++)
	{
		if (i % (ihdr.width * (ihdr.color_type + 1) + 1) == 0)
		{
			pos_y++;
			filters[pos_y] = uncompressed[i];
			pos_x = 0;
		}
		else
		{
			if (ihdr.color_type)
			{
				if (color % 3 == 0)
				{
					SET_RGB(pos_y, pos_x, RED, uncompressed[i]);
				}
				else if (color % 3 == 1)
				{
					SET_RGB(pos_y, pos_x, GREEN, uncompressed[i]);
				}
				else
				{
					SET_RGB(pos_y, pos_x++, BLUE, uncompressed[i]);
				}
				color++;
			}
			else
			{
				SET_GSC(pos_y, pos_x++, uncompressed[i]);
			}
		}
	}

	free(uncompressed);
	free(compressed);

	for (size_t i = 0; i != ihdr.height; i++)
	{
		switch (filters[i])
		{
		case 0:
			break;
		case 1:
		{
			for (size_t j = 0; j != ihdr.width; j++)
			{
				if (ihdr.color_type)
				{
					uchar const red_sub = GET_RGB(i, j, RED);
					uchar const blu_sub = GET_RGB(i, j, BLUE);
					uchar const gre_sub = GET_RGB(i, j, GREEN);
					uchar const red_raw = GET_RGB_UP(i, j, RED);
					uchar const blu_raw = GET_RGB_UP(i, j, BLUE);
					uchar const gre_raw = GET_RGB_UP(i, j, GREEN);
					SET_RGB(i, j, RED, sub_up(red_sub, red_raw));
					SET_RGB(i, j, BLUE, sub_up(blu_sub, blu_raw));
					SET_RGB(i, j, GREEN, sub_up(gre_sub, gre_raw));
				}
				else
				{
					uchar const gra_sub = GET_GSC(i, j);
					uchar const gra_raw = GET_GSC_UP(i, j);
					SET_GSC(i, j, sub_up(gra_sub, gra_raw));
				}
			}
			break;
		}
		case 2:
		{
			for (size_t j = 0; j != ihdr.width; j++)
			{
				if (ihdr.color_type)
				{
					uchar const red_up = GET_RGB(i, j, RED);
					uchar const blu_up = GET_RGB(i, j, BLUE);
					uchar const gre_up = GET_RGB(i, j, GREEN);
					uchar const red_prior = GET_RGB_LEFT(i, j, RED);
					uchar const blu_prior = GET_RGB_LEFT(i, j, BLUE);
					uchar const gre_prior = GET_RGB_LEFT(i, j, GREEN);
					SET_RGB(i, j, RED, sub_up(red_up, red_prior));
					SET_RGB(i, j, BLUE, sub_up(blu_up, blu_prior));
					SET_RGB(i, j, GREEN, sub_up(gre_up, gre_prior));
				}
				else
				{
					uchar const gra_up = GET_GSC(i, j);
					uchar const gra_prior = GET_GSC_LEFT(i, j);
					SET_GSC(i, j, sub_up(gra_up, gra_prior));
				}
			}
			break;
		}
		case 3:
		{
			for (size_t j = 0; j != ihdr.width; j++)
			{
				if (ihdr.color_type)
				{
					uchar const red_prior = GET_RGB_LEFT(i, j, RED);
					uchar const blu_prior = GET_RGB_LEFT(i, j, BLUE);
					uchar const gre_prior = GET_RGB_LEFT(i, j, GREEN);
					uchar const red_raw = GET_RGB_UP(i, j, RED);
					uchar const blu_raw = GET_RGB_UP(i, j, BLUE);
					uchar const gre_raw = GET_RGB_UP(i, j, GREEN);
					uchar const red_average = GET_RGB(i, j, RED);
					uchar const blu_average = GET_RGB(i, j, BLUE);
					uchar const gre_average = GET_RGB(i, j, GREEN);
					SET_RGB(i, j, RED, average(red_average, red_raw, red_prior));
					SET_RGB(i, j, BLUE, average(blu_average, blu_raw, blu_prior));
					SET_RGB(i, j, GREEN, average(gre_average, gre_raw, gre_prior));
				}
				else
				{
					uchar const gra_prior = GET_GSC_LEFT(i, j);
					uchar const gra_raw = GET_GSC_UP(i, j);
					uchar const gra_average = GET_GSC(i, j);
					SET_GSC(i, j, average(gra_average, gra_raw, gra_prior));
				}
			}
			break;
		}
		case 4:
		{
			for (size_t j = 0; j != ihdr.width; j++)
			{
				if (ihdr.color_type)
				{
					uchar const red_paeth = GET_RGB(i, j, RED);
					uchar const blu_paeth = GET_RGB(i, j, BLUE);
					uchar const gre_paeth = GET_RGB(i, j, GREEN);
					uchar const red_raw = GET_RGB_UP(i, j, RED);
					uchar const blu_raw = GET_RGB_UP(i, j, BLUE);
					uchar const gre_raw = GET_RGB_UP(i, j, GREEN);
					uchar const red_prior = GET_RGB_LEFT(i, j, RED);
					uchar const blu_prior = GET_RGB_LEFT(i, j, BLUE);
					uchar const gre_prior = GET_RGB_LEFT(i, j, GREEN);
					uchar const red_raw_prior = GET_RGB_DIAG(i, j, RED);
					uchar const blu_raw_prior = GET_RGB_DIAG(i, j, BLUE);
					uchar const gre_raw_prior = GET_RGB_DIAG(i, j, GREEN);
					SET_RGB(i, j, RED, sub_up(red_paeth, paeth(red_raw, red_prior, red_raw_prior)));
					SET_RGB(i, j, BLUE, sub_up(blu_paeth, paeth(blu_raw, blu_prior, blu_raw_prior)));
					SET_RGB(i, j, GREEN, sub_up(gre_paeth, paeth(gre_raw, gre_prior, gre_raw_prior)));
				}
				else
				{
					uchar const gra_paeth = GET_GSC(i, j);
					uchar const gra_raw = GET_GSC_UP(i, j);
					uchar const gra_prior = GET_GSC_LEFT(i, j);
					uchar const gra_raw_prior = GET_GSC_DIAG(i, j);
					SET_GSC(i, j, sub_up(gra_paeth, paeth(gra_raw, gra_prior, gra_raw_prior)));
				}
			}
			break;
		}
		default:
		{
			free(filters);
			FREE_PIXELS;
			CLOSE_ERROR_DATA;
		}
		}
	}

	free(filters);

	fou = fopen(argv[2], "wb");
	if (!fou)
	{
		error_file_exists();
		FREE_PIXELS;
		return ERROR_FILE_EXISTS;
	}

	if (ihdr.color_type)
	{
		if (fwrite(p6_template, sizeof(uchar), 2, fou) != 2)
		{
			fclose(fou);
			FREE_PIXELS;
			CLOSE_ERROR_WRITE;
		}
	}
	else
	{
		if (fwrite(p5_template, sizeof(uchar), 2, fou) != 2)
		{
			fclose(fou);
			FREE_PIXELS;
			CLOSE_ERROR_WRITE;
		}
	}

	if (pgm_start(fou, ihdr.width, ihdr.height))
	{
		fclose(fou);
		FREE_PIXELS;
		CLOSE_ERROR_WRITE;
	}

	for (size_t i = 0; i != ihdr.height; i++)
	{
		for (size_t j = 0; j != ihdr.width; j++)
		{
			if (ihdr.color_type)
			{
				uchar const output[] = { GET_RGB(i, j, RED), GET_RGB(i, j, GREEN), GET_RGB(i, j, BLUE) };
				if (fwrite(output, 1, 3, fou) != 3)
				{
					fclose(fou);
					FREE_PIXELS;
					CLOSE_ERROR_WRITE;
				}
			}
			else
			{
				uchar const output[] = { GET_GSC(i, j) };
				if (fwrite(output, 1, 1, fou) != 1)
				{
					fclose(fou);
					FREE_PIXELS;
					CLOSE_ERROR_WRITE;
				}
			}
		}
	}

	fclose(fou);
	FREE_PIXELS;

	return ERROR_SUCCESS;
}
