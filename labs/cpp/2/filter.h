#pragma once

#include "uchar.h"
#include <math.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

uchar sub_up(int const a, int const b)
{
	return (uchar)((a + b) % 256);
}

int average(int const average, int const raw, int const prior)
{
	int const a = average;
	int const r = raw;
	int const p = prior;
	return (a + (int)floor((r + p + 0.0) / 2));
}

int paeth(int const a, int const b, int const c)
{
	int const p = a + b - c;
	int const pa = abs(p - a);
	int const pb = abs(p - b);
	int const pc = abs(p - c);
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