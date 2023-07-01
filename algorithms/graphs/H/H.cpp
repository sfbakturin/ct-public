
/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define BEGIN int main() {
#define END }
#define OPTIMIZE std::ios_base::sync_with_stdio(false); std::cin.tie(0); std::cout.tie(0)

#include <cstdio>
#include <vector>
#include <limits>
#include <algorithm>

void dfs_ts(std::vector<std::vector<int>> const& edge, int const& v, int const& fuel, int const& n, std::vector<int> &ts, std::vector<bool> &visited, std::vector<std::vector<int>> &reversed) {
	visited[v] = true;
	for (int u = 0; u < n; u++) {
		if (u != v && edge[v][u] <= fuel) {
			reversed[u].push_back(v);
			if (!visited[u]) dfs_ts(edge, u, fuel, n, ts, visited, reversed);
		}
	}
	ts.push_back(v);
}

void dfs_scc(std::vector<std::vector<int>> const& edge_reverse, int const& v, int const& c, std::vector<int> &comp) {
	comp[v] = c;
	for (int const& u : edge_reverse[v]) if (!comp[u]) dfs_scc(edge_reverse, u, c, comp);
}

bool is_connectivity(int const& fuel, std::vector<std::vector<int>> const& matrix, int const& n) {
	std::vector<bool> visited(n);
	std::vector<int> ts, comp(n);
	std::vector<std::vector<int>> reversed(n, std::vector<int>());
	int c = 1;
	for (int i = 0; i < n; i++) if (!visited[i]) dfs_ts(matrix, i, fuel, n, ts, visited, reversed);
	std::reverse(ts.begin(), ts.end());
	for (int const& i : ts) {
		if (!comp[i]) dfs_scc(reversed, i, c++, comp);
	}
	for (int i = 0; i < n; i++) if (comp[i] != 1) return false;
	return true;
}

int binary_search(int l, int r, std::vector<std::vector<int>> const& matrix, int const& n) {
	while (l < r - 1) {
		int m = static_cast<int>((l + r + 0ULL) / 2);
		if (is_connectivity(m, matrix, n)) r = m;
		else l = m;
	}
	return r;
}

BEGIN
	int n;
	std::scanf("%i", &n);
	std::vector<std::vector<int>> matrix(n, std::vector<int>(n));
	int fuel_l = std::numeric_limits<int>::max();
	int fuel_r = std::numeric_limits<int>::min();
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++) {
			std::scanf("%i", &matrix[i][j]);
			if (matrix[i][j]) {
				fuel_l = std::min(fuel_l, matrix[i][j]);
				fuel_r = std::max(fuel_r, matrix[i][j]);
			}
		}
	}
	std::printf("%i", binary_search(-1, fuel_r, matrix, n));
END
