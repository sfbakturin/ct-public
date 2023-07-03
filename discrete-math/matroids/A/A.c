#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

char __READ_CHAR(FILE *__IN)
{
	char __RESULT = 0;
	fscanf(__IN, "%c", &__RESULT);
	return __RESULT;
}

int __READ_INT(FILE *__IN)
{
	int __RESULT = 0;
	fscanf(__IN, "%i", &__RESULT);
	return __RESULT;
}

long long __READ_LONG_LONG(FILE *__IN)
{
	long long __RESULT = 0;
	fscanf(__IN, "%lld", &__RESULT);
	return __RESULT;
}

float __READ_FLOAT(FILE *__IN)
{
	float __RESULT = 0;
	fscanf(__IN, "%f", &__RESULT);
	return __RESULT;
}

double __READ_DOUBLE(FILE *__IN)
{
	double __RESULT = 0;
	fscanf(__IN, "%lf", &__RESULT);
	return __RESULT;
}

char *__READ_STRING(FILE *__IN, size_t __MAX_LENGTH)
{
	size_t __PTR = 0;
	char __DELIM = ' ';
	char __ZERO = '\0';
	char *__RESULT = malloc(sizeof(char) * (__MAX_LENGTH + 1));
	for (;;)
	{
		char __CHAR = __READ_CHAR(__IN);
		if (__CHAR == EOF || __CHAR == __DELIM || __PTR == __MAX_LENGTH)
		{
			break;
		}
		__RESULT[__PTR++] = __CHAR;
	}
	__RESULT[__PTR] = __ZERO;
	return __RESULT;
}

void __WRITE_CHAR(FILE *__OUT, char __CHAR)
{
	fprintf(__OUT, "%c", __CHAR);
}

void __WRITE_INT(FILE *__OUT, int __INT)
{
	fprintf(__OUT, "%i", __INT);
}

void __WRITE_LONG_LONG(FILE *__OUT, long long __LONG_LONG)
{
	fprintf(__OUT, "%lld", __LONG_LONG);
}

void __WRITE_FLOAT(FILE *__OUT, float __FLOAT)
{
	fprintf(__OUT, "%f", __FLOAT);
}

void __WRITE_DOUBLE(FILE *__OUT, double __DOUBLE)
{
	fprintf(__OUT, "%lf", __DOUBLE);
}

void __WRITE_STRING(FILE *__OUT, char *__STRING)
{
	fprintf(__OUT, "%s", __STRING);
}

#define make_array1d(TYPE, SIZE) malloc(sizeof(TYPE) * SIZE)
#define make_array2d(TYPE, X, Y) malloc(sizeof(TYPE) * (X * Y))

#define get_array2d(ARRAY, N, X, Y) ARRAY[X * N + Y]

#define io_set_user   \
	FILE *in = stdin; \
	FILE *out = stdout
#define io_set_file(I, O)     \
	FILE *in = fopen(I, "r"); \
	FILE *out = fopen(O, "w")
#define io_close       \
	if (in != stdin)   \
		fclose(in);    \
	if (out != stdout) \
		fclose(out);

#define read_char __READ_CHAR(in)
#define read_int __READ_INT(in)
#define read_long __READ_LONG_LONG(in)
#define read_float __READ_FLOAT(in)
#define read_double __READ_DOUBLE(in)
#define read_string(MAX) __READ_STRING(in, MAX)

#define print_char(I) __WRITE_CHAR(out, I)
#define print_int(I) __WRITE_INT(out, I)
#define print_long(I) __WRITE_LONG_LONG(out, I)
#define print_float(I) __WRITE_FLOAT(out, I)
#define print_double(I) __WRITE_DOUBLE(out, I)
#define print_string(I) __WRITE_STRING(out, I)

#define print_whitespace print_string(" ")
#define print_newline print_string("\n")

typedef struct
{
	int d, w;
} pair;

int cmp(void const *a, void const *b)
{
	int wa = ((pair *)a)->w;
	int wb = ((pair *)b)->w;
	return (wb - wa);
}

int main()
{
	io_set_file("schedule.in", "schedule.out");
	int n = read_int;
	long long sum = 0;
	pair *array = make_array1d(pair, n);
	bool *enabled = make_array1d(bool, n);
	for (int i = 0; i < n; i++)
	{
		enabled[i] = true;
		array[i].d = read_int;
		array[i].w = read_int;
	}
	qsort(array, n, sizeof(pair), (int (*)(void const *, void const *))cmp);
	for (int i = 0; i < n; i++)
	{
		if (array[i].d < n)
		{
			bool flag = false;
			for (int j = array[i].d - 1; j >= 0; j--)
			{
				if (enabled[j])
				{
					enabled[j] = false;
					flag = true;
					break;
				}
			}
			if (!flag)
			{
				sum += array[i].w;
			}
		}
	}
	print_long(sum);
	print_newline;
	free(array);
	free(enabled);
	io_close;
	return 0;
}
