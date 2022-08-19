#include <cstdio>
#include <cmath>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main() {
	int n;
	std::scanf("%i", &n);
	int *p = new int[n + 1]();
	for (int i = 1; i < n + 1; i++) {
		int a;
		std::scanf("%i", &a);
		p[i] = a;
	}
	int const slog = static_cast<int>(std::log(n + 1) / std::log(2)) + 1;
	int **up = new int*[slog]();
	for (int i = 0; i < slog; i++) {
		up[i] = new int[n + 1]();
	}
	for (int i = 1; i < n + 1; i++) {
		up[0][i] = p[i];
	}
	for (int d = 1; d < slog; d++) {
		for (int i = 1; i < n + 1; i++) {
			up[d][i] = up[d - 1][up[d - 1][i]];
		}
	}
	for (int i = 1; i < n + 1; i++) {
		std::printf("%i: ", i);
		for (int j = 0; j < slog; j++) {
			if (up[j][i] != 0) {
				std::printf("%i ", up[j][i]);
			}
		}
		std::printf("\n");
	}
}
