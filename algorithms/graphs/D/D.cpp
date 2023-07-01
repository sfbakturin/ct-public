
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
