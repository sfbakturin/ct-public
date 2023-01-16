#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <tgmath.h>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

long double gen(int n, int s) {
	long double res = 0;
	for (int i = 1; i <= n; i++) {
		long double add = (long double)1/(long double)(powl((long double)i, (long double)s));
		res += add;
	}
	return res;
}

int main(int argc, char **argv) {
	long double res = gen(atoi(argv[1]), atoi(argv[2]));
	return 0;
}
