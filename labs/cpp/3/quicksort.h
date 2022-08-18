#include "error_message.h"
#include "phonebook.h"
#include <algorithm>

#define COMP(A, B) (descending ? A > B : A < B)

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

template< typename T, bool descending >
std::size_t choose(T *array, std::size_t const left, std::size_t const right)
{
	T const q = array[((left + right) / 2)];
	std::size_t l = left;
	std::size_t r = right;
	while (l <= r)
	{
		while (COMP(array[l], q) && COMP(q, array[r]))
		{
			l++;
			r--;
		}
		while (COMP(array[l], q))
		{
			l++;
		}
		while (COMP(q, array[r]))
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
	return r;
}

template< typename T, bool descending >
void sort(T *array, std::size_t const left, std::size_t const right)
{
	std::size_t A = left, B = right;
	while (true)
	{
		if (A >= B)
		{
			break;
		}
		std::size_t const k = choose< T, descending >(array, A, B);
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
void quicksort(T *array, std::size_t const size)
{
	if (size != 0)
	{
		sort< T, descending >(array, 0, size - 1);
	}
}
