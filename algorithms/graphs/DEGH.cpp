
/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define CURRENT 'D'
#define BEGIN int main() {
#define END }
#define OPTIMIZE std::ios_base::sync_with_stdio(false); std::cin.tie(0); std::cout.tie(0)

#if CURRENT == 'D'

#include <cstdio>
#include <vector>
#include <utility>
#include <map>
#include <deque>

struct graph {
private:
	int T = 0;
	bool *mark = nullptr;
	bool *bridges = nullptr;
	std::vector<int> tin, up;
	std::vector<std::vector<int>> edges;
	std::map<std::pair<int, int>, std::pair<int, bool>> &numbered_edges;

	void bridge_impl(int const& curr, int const& prev) {
		tin[curr] = T++;
		up[curr] = tin[curr];
		mark[curr] = true;
		for (int const& item : edges[curr]) {
			if (item == prev) continue;
			if (!mark[item]) {
				bridge_impl(item, curr);
				up[curr] = std::min(up[curr], up[item]);
				if (up[item] > tin[curr]) {
					std::pair<int, bool> edge = numbered_edges[{std::min(curr, item), std::max(curr, item)}];
					bridges[edge.first] = !(edge.second);
				}
			} else up[curr] = std::min(up[curr], tin[item]);
		}
	}

public:
	graph(int const& size, int const& size_edge, std::map<std::pair<int, int>, std::pair<int, bool>> &numbered_edges) :
		edges(size + 1, std::vector<int>()),
		tin(size + 1),
		up(size + 1),
		mark(new bool[size + 1]()),
		bridges(new bool[size_edge + 1]()),
		numbered_edges(numbered_edges) {}

	void add(int const& u, int const& v) {
		edges[u].push_back(v);
		edges[v].push_back(u);
	}

	std::vector<int> const& get(int const& index) const {
		return edges[index];
	}

	bool check(int const& u, int const& v) const {
		return bridges[numbered_edges[{u, v}].first];
	}

	void bridge(int const& i) {
		bridge_impl(i, -1);
	}

	bool const* get_exists() const {
		return bridges;
	}
};

void select_components(int const& v, graph const& g, std::vector<int> &comp) {
	comp[v] = 1;
	for (int const& item : g.get(v)) if (comp[item] == -1) select_components(item, g, comp);
}

void dfs(int const& color, graph const& g, int const& v, std::vector<int> &comp) {
	comp[v] = color;
	for (int const& item : g.get(v)) {
		if (comp[item] == -1 && !g.check(std::min(v, item), std::max(v, item))) {
			dfs(color, g, item, comp);
		}
	}
}

BEGIN
	int n, m;
	std::scanf("%i %i", &n, &m);
	std::map<std::pair<int, int>, std::pair<int, bool>> numbered_edges;
	std::vector<int> comp(n + 1);
	std::vector<int> start_comp;
	graph g(n, m, numbered_edges);
	int c = 1;
	std::fill(comp.begin(), comp.end(), -1);
	for (int i = 1; i < m + 1; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		std::pair<int, int> pair = {std::min(u, v), std::max(u, v)};
		auto found = numbered_edges.find(pair);
		if (found == numbered_edges.end()) {
			numbered_edges[pair] = {i, false};
			g.add(u, v);
		}
		else found->second.second = true;
	}
	for (int i = 1; i < n + 1; i++) {
		if (comp[i] == -1) {
			select_components(i, g, comp);
			start_comp.push_back(i);
		}
	}
	std::fill(comp.begin(), comp.end(), -1);
	for (int const& item : start_comp) g.bridge(item);
	for (int i = 1; i < n + 1; i++) {
		if (comp[i] == -1) {
			dfs(c++, g, i, comp);
		}
	}
	std::printf("%i\n", c - 1);
	for (int i = 1; i < n + 1; i++) std::printf("%i ", (comp[i] < 0 ? c++ : comp[i]));
END

#endif

#if CURRENT == 'E'

#include <cstdio>
#include <vector>
#include <map>
#include <utility>
#include <deque>
#include <set>

struct graph {
private:
	int timer = 0;
	int max_color = 0;
	std::vector<int> up;
	std::vector<int> tin;
	std::vector<bool> used;
	std::vector<bool> visited;
	std::set<std::pair<int, int>> unique_ribs;
	std::vector<std::pair<int, int>> numbered_ribs;
	std::map<std::pair<int, int>, int> colour_ribs;
	std::vector<std::vector<int>> edges;

public:
	graph(int const& size, int const& size_edge) :
		up(size + 1),
		tin(size + 1),
		used(size + 1),
		visited(size + 1),
		numbered_ribs(size_edge),
		edges(size + 1, std::vector<int>()) 
	{}

	void add(int const& u, int const& v, int const& index) {
		std::pair<int, int> edge = {std::min(u, v), std::max(u, v)};
		numbered_ribs[index] = edge;
		if (unique_ribs.find(edge) == unique_ribs.end()) {
			unique_ribs.insert(edge);
			edges[u].push_back(v);
			edges[v].push_back(u);
		}
	}

	void find_points(int const& v, int const& p = -1) {
		tin[v] = up[v] = timer++;
		used[v] = true;
		for (int const& item : edges[v]) {
			if (v == p) continue;
			if (!used[item]) {
				find_points(item, v);
				up[v] = std::min(up[v], up[item]);
			} else {
				up[v] = std::min(up[v], tin[item]);
			}
		}
	}

	void paint(int v, int color, int parent = -1) {
		visited[v] = true;
		for (int const& u : edges[v]) {
			if (u == parent) continue;
			if (visited[u]) {
				if (tin[u] < tin[v]) {
					auto edge = colour_ribs.find({std::min(u, v), std::max(u, v)});
					if (edge != colour_ribs.end()) {
						colour_ribs[{std::min(u, v), std::max(u, v)}] = std::min(color, (*edge).second);
					} else {
						colour_ribs[{std::min(u, v), std::max(u, v)}] = color;
					}
				}
			} else {
				if (up[u] >= tin[v]) {
					int const new_color = ++max_color;
					colour_ribs[{std::min(u, v), std::max(u, v)}] = new_color;
					paint(u, new_color, v);
				} else {
					colour_ribs[{std::min(u, v), std::max(u, v)}] = color;
					paint(u, color, v);
				}
			}
		}
	}

	void debug() {
		for (int i = 1; i < edges.size(); i++) {
			std::printf("up[%i] = %i, tin[%i] = %i\n", i, up[i], i, tin[i]);
		}
	}

	void view() {
		for (auto const& edge : numbered_ribs) {
			std::printf("%i ", colour_ribs.at(edge));
		}
	}

	std::vector<int> const& get_children(int const& v) const {
		return edges[v];
	}

	int const& color() const {
		return max_color;
	}
};

void select_components(int const& v, graph const& g, std::vector<int> &components) {
	components[v]++;
	for (int const& item : g.get_children(v)) {
		if (!(components[item])) select_components(item, g, components);
	}
}

BEGIN
	int n, m;
	std::scanf("%i %i", &n, &m);
	std::vector<int> components(n + 1);
	graph g(n, m);
	for (int i = 0; i < m; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		g.add(u, v, i);
	}
	for (int i = 1; i < n + 1; i++) {
		if (!components[i]) {
			select_components(i, g, components);
			g.find_points(i);
			g.paint(i, g.color());
		}
	}
	std::printf("%i\n", g.color());
	g.view();
END

#endif

#if CURRENT == 'G'

#include <algorithm>
#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <list>
#include <set>

void dfs_ts(std::vector<std::vector<int>> const& edge, int const& v, std::vector<int> &ts, std::vector<bool> &visited) {
	visited[v] = true;
	for (int const& u : edge[v]) if (!visited[u]) dfs_ts(edge, u, ts, visited);
	ts.push_back(v);
}
 
void dfs_scc(std::vector<std::vector<int>> const& edge_reverse, int const& v, int const& c, std::vector<int> &comp) {
	comp[v] = c;
	for (int const& u : edge_reverse[v]) if (!comp[u]) dfs_scc(edge_reverse, u, c, comp);
}

BEGIN
	OPTIMIZE;
	int n, m;
	std::cin >> n >> m;
	std::vector<std::string> id(n);
	std::map<std::string, int> names;
	std::vector<std::vector<int>> g(2 * n, std::vector<int>());
	std::vector<std::vector<int>> g_reverse(2 * n, std::vector<int>());
	std::vector<int> comp(2 * n);
	std::vector<bool> vis(2 * n);
	std::vector<int> ts;
	std::set<int> answer;
	int c = 1;
	for (int i = 0; i < n; i++) {
		std::string name;
		std::cin >> name;
		names[name] = i;
		id[i] = name;
	}
	for (int i = 0; i < m; i++) {
		char mark1, mark2;
		std::string name1, name2, temp;
		std::cin >> mark1 >> name1 >> temp >> mark2 >> name2;
		int index1 = names[name1];
		int index2 = names[name2];
		bool flag1 = mark1 == '+';
		bool flag2 = mark2 == '+';
		if (flag1 && flag2) {
			g[index1].push_back(index2);
			g_reverse[index2].push_back(index1);

			g[index2 + n].push_back(index1 + n);
			g_reverse[index1 + n].push_back(index2 + n);
		}
		if (flag1 && !flag2) {
			g[index1].push_back(index2 + n);
			g_reverse[index2 + n].push_back(index1);

			g[index2].push_back(index1 + n);
			g_reverse[index1 + n].push_back(index2);
		}
		if (!flag1 && flag2) {
			g[index1 + n].push_back(index2);
			g_reverse[index2].push_back(index1 + n);

			g[index2 + n].push_back(index1);
			g_reverse[index1].push_back(index2 + n);
		}
		if (!flag1 && !flag2) {
			g[index1 + n].push_back(index2 + n);
			g_reverse[index2 + n].push_back(index1 + n);

			g[index2].push_back(index1);
			g_reverse[index1].push_back(index2);
		}
	}
	for (int i = 0; i < 2 * n; i++) {
		if (!vis[i]) dfs_ts(g, i, ts, vis);
	}
	std::reverse(ts.begin(), ts.end());
	for (int const& v : ts) if (!comp[v]) dfs_scc(g_reverse, v, c++, comp);
	for (int i = 0; i < n; i++) {
		if (comp[i] == comp[i + n]) {
			std::cout << "-1" << std::endl;
			return 0;
		} else if (comp[i] > comp[i + n]) {
			answer.insert(i);
		}
	}
	std::cout << answer.size() << std::endl;
	for (int const& item : answer) std::cout << id[item] << std::endl;
END

#endif

#if CURRENT == 'H'

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

#endif
