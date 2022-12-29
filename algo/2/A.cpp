#include <cstdio>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main() {
	FILE *in = fopen("input.txt", "r");
	FILE *out = fopen("output.txt", "w");
	int n;
	fscanf(in, "%i", &n);
	long long *a = new long long[n + 1]();
	a[1] = 1;
	for (int i = 2; i < n + 1; i++) {
		a[i] += a[i - 1] + a[i - 2];
	}
	fprintf(out, "%lli", a[n]);
	fclose(in);
	fclose(out);
}
