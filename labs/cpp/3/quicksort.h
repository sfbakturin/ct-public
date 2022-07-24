#include "functions.h"
#include "phonebook.h"
#include <algorithm>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

template< typename T, bool descending >
size_t choose(T *array, const size_t left, const size_t right)
{
	const T q = array[((left + right) / 2)];
	size_t l = left;
	size_t r = right;
	while (l <= r)
	{
		if (descending)
		{
			while (array[l] > q && q > array[r])
			{
				l++;
				r--;
			}
			while (array[l] > q)
			{
				l++;
			}
			while (q > array[r])
			{
				r--;
			}
			if (r > l)
			{
				std::swap(array[l++], array[r--]);
			}
			else
			{
				break;
			}
		}
		else
		{
			while (array[l] < q && q < array[r])
			{
				l++;
				r--;
			}
			while (array[l] < q)
			{
				l++;
			}
			while (q < array[r])
			{
				r--;
			}
			if (r > l)
			{
				std::swap(array[l++], array[r--]);
			}
			else
			{
				break;
			}
		}
	}
	return r;
}

template< typename T, bool descending >
void sort(T *array, const size_t left, const size_t right)
{
	size_t A = left, B = right;
	while (true)
	{
		if (A >= B)
		{
			break;
		}
		const size_t k = choose< T, descending >(array, A, B);
		if ((A + B) / 2 > k)
		{
			sort< T, descending >(array, A, k);
			A = k + 1;
		}
		else
		{
			sort< T, descending >(array, k + 1, B);
			B = k;
		}
	}
}

template< typename T, bool descending >
void quicksort(T *array, const size_t size)
{
	if (size != 0)
	{
		sort< T, descending >(array, 0, size - 1);
	}
}
