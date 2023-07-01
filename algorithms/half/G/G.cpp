#include <cstdio>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main() {
	int n, m;
	scanf("%i %i", &n, &m);
	long long *a = new long long[n + 1]();
	for (int i = 1; i < n + 1; i++) {
		scanf("%lld", &a[i]);
		a[i] += a[i - 1];
	}
	for (int i = 0; i < m; i++) {
		int l, r;
		scanf("%i %i", &l, &r);
		printf("%lld\n", a[r] - a[l - 1]);
	}
}
