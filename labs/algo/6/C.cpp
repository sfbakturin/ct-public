#include <cstdio>
#include <cmath>
#include <vector>
#include <iostream>
#include <map>
#include <utility>
#include <limits>
#include <algorithm>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define __INT__MAX__ std::numeric_limits<int>::max()

int *tIN, *tOUT, *depth;
int T = 0;

int lca(int, int, int const, std::vector<std::vector<std::pair<int, int>>> const&);
bool is_ancestor(int const, int const);
void dfs(std::vector<std::vector<int>> const&, int const, int const);

int main() {
	int n, m;
	std::scanf("%i", &n);
	int const slog = static_cast<int>(std::log(n + 1) / std::log(2)) + 10;
	std::vector<std::vector<int>> children(n + 1, std::vector<int>());
	std::vector<std::vector<std::pair<int, int>>> up(slog, std::vector<std::pair<int, int>>());
	for (int i = 0; i < slog; i++) {
		up[i] = std::vector<std::pair<int, int>>(n + 1, {1, __INT__MAX__});
	}
	up[0][0].first = 1;
	up[0][1].first = 1;
	tIN = new int[n + 1]();
	tOUT = new int[n + 1]();
	depth = new int[n + 1]();
	for (int i = 2; i < n + 1; i++) {
		int x, y;
		std::scanf("%i %i", &x, &y);
		children[x].push_back(i);
		up[0][i].first = x;
		up[0][i].second = y;
	}
	dfs(children, 1, 0);
	for (int d = 1; d < slog; d++) {
		for (int i = 1; i < n + 1; i++) {
			up[d][i].first = up[d - 1][up[d - 1][i].first].first;
			up[d][i].second = std::min(up[d - 1][up[d - 1][i].first].second, up[d - 1][i].second);
		}
	}
	std::scanf("%i", &m);
	for (int i = 0; i < m; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		std::printf("%i\n", lca(u, v, slog, up));
	}
}

int lca(int u, int v, int const slog, std::vector<std::vector<std::pair<int, int>>> const &up) {
	int min = __INT__MAX__;
	if (depth[u] > depth[v]) {
		std::swap(u, v);
	}
	for (int i = slog - 1; i >= 0; i--) {
		if (depth[up[i][v].first] - depth[u] >= 0) {
			min = std::min(min, up[i][v].second);
			v = up[i][v].first;
		}
	}
	if (u == v) {
		return min;
	}
	for (int i = slog - 1; i >= 0; i--) {
		if (up[i][u] != up[i][v]) {
			min = std::min(up[i][u].second, std::min(up[i][v].second, min));
			u = up[i][u].first;
			v = up[i][v].first;
		}
	}
	return min;
}

bool is_ancestor(int const u, int const v) {
	return (tIN[u] <= tIN[v]) && (tOUT[u] >= tOUT[v]);
}

void dfs(std::vector<std::vector<int>> const &children, int const x, int const h) {
	depth[x] = h;
	tIN[x] = T++;
	for (auto const &y : children[x]) {
		dfs(children, y, h + 1);
	}
	tOUT[x] = T++;
}
