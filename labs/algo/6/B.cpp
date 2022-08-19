#include <cstdio>
#include <cmath>
#include <vector>
#include <iostream>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int *tIN, *tOUT, **up, *p, *height;
int T = 0;

int lca(int, int, int const);
bool is_ancestor(int const, int const);
void dfs(std::vector<std::vector<int>> const&, int const, int const);

int main() {
	int n, m;
	std::scanf("%i", &n);
	p = new int[n + 1]();
	p[0] = 1;
	p[1] = 1;
	tIN = new int[n + 1]();
	tOUT = new int[n + 1]();
	height = new int[n + 1]();
	std::vector<std::vector<int>> children(n + 1, std::vector<int>());
	for (int i = 2; i < n + 1; i++) {
		int x;
		std::scanf("%i", &x);
		p[i] = x;
		children[x].push_back(i);
	}
	dfs(children, 1, 0);
	int const slog = static_cast<int>(std::log(n + 1) / std::log(2)) + 1;
	up = new int*[slog];
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
	std::scanf("%i", &m);
	for (int i = 0; i < m; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		std::printf("%i\n", lca(u, v, slog));
	}
}

int lca(int u, int v, int const slog) {
	if (is_ancestor(u, v)) {
		return u;
	}
	if (is_ancestor(v, u)) {
		return v;
	}
	if (height[u] > height[v]) {
		std::swap(u, v);
	}
	for (int i = slog - 1; i >= 0; i--) {
		if (height[up[i][v]] - height[u] >= 0) {
			v = up[i][v];
		}
	}
	if (u == v) {
		return v;
	}
	for (int i = slog - 1; i >= 0; i--) {
		if (up[i][u] != up[i][v]) {
			u = up[i][u];
			v = up[i][v];
		}
	}
	return p[u];
}

bool is_ancestor(int const u, int const v) {
	return (tIN[u] <= tIN[v]) && (tOUT[u] >= tOUT[v]);
}

void dfs(std::vector<std::vector<int>> const &children,  int const x, int const h) {
	height[x] = h;
	tIN[x] = T++;
	for (auto const &y : children[x]) {
		dfs(children, y, h + 1);
	}
	tOUT[x] = T++;
}
