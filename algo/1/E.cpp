#include <cstdio>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

bool flag = true;

void check(int *arr, int i, int n);

int main() {
	int n;
	scanf("%i", &n);
	int *arr = new int[n]();
	for (int i = 0; i < n; i++) {
		scanf("%i", &arr[i]);
	}
	check(arr, 0, n);
	if (flag) {
		printf("YES");
	} else {
		printf("NO");
	}
}

void check(int *arr, int i, int n) {
	if (!flag) {
		return;
	}
	if ((2 * i) + 1 < n) {
		if (arr[i] <= arr[(2 * i) + 1]) {
			check(arr, (2 * i) + 1, n);
		} else {
			flag = false;
			return;
		}
	}
	if ((2 * i) + 2 < n) {
		if (arr[i] <= arr[(2 * i) + 2]) {
			check(arr, (2 * i) + 2, n);
		} else {
			flag = false;
			return;
		}
	}
}
