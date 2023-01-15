#include <cstdio>
#include <vector>
#include <set>
#include <algorithm>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

void dfs_ts(std::vector<std::vector<int>> const& edge, int const& v, std::vector<int> &ts, bool *visited) {
	visited[v] = true;
	for (int const& u : edge[v]) if (!visited[u]) dfs_ts(edge, u, ts, visited);
	ts.push_back(v);
}

void dfs_scc(std::vector<std::vector<int>> const& edge_reverse, int const& v, int const& c, std::vector<int> &comp) {
	comp[v] = c;
	for (int const& u : edge_reverse[v]) if (!comp[u]) dfs_scc(edge_reverse, u, c, comp);
}

int main() {
	std::size_t n, m;
	std::scanf("%zu %zu", &n, &m);
	bool *visited = new bool[n + 1]();
	std::vector<std::vector<int>> edge(n + 1, std::vector<int>());
	std::vector<std::vector<int>> edge_reverse(n + 1, std::vector<int>());
	std::vector<int> comp(n + 1);
	std::vector<int> ts;
	std::set<std::pair<int, int>> answer;
	int c = 1;
	for (std::size_t i = 0; i < m; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		edge[u].push_back(v);
		edge_reverse[v].push_back(u);
	}
	for (int v = 1; v < n + 1; v++) if (!visited[v]) dfs_ts(edge, v, ts, visited);
	std::reverse(ts.begin(), ts.end());
	for (int const& v : ts) if (!comp[v]) dfs_scc(edge_reverse, v, c++, comp);
	for (int u = 1; u < n + 1; u++) {
		for (int v : edge[u]) if (comp[u] != comp[v]) answer.emplace(comp[u], comp[v]);
	}
	std::printf("%zu\n", answer.size());
}
