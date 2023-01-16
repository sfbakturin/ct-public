#include <cstdio>
#include <vector>
#include <algorithm>
#include <map>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct bridges {
private:
	int T = 0;
	std::vector<int> tin, up;
	std::vector<bool> mark;
	std::vector<std::vector<int>> &edges;
	std::vector<std::pair<int, int>> found_bridges;

	void dfs(int const& curr, int const& prev) {
		tin[curr] = T++;
		up[curr] = tin[curr];
		mark[curr] = true;
		for (int const& item : edges[curr]) {
			if (item == prev) continue;
			if (!mark[item]) {
				dfs(item, curr);
				up[curr] = std::min(up[curr], up[item]);
				if (up[item] > tin[curr]) found_bridges.emplace_back(std::min(curr, item), std::max(curr, item));
			} else {
				up[curr] = std::min(up[curr], tin[item]);
			}
		}
	}

public:
	bridges(std::vector<std::vector<int>> &edges) :
		edges(edges),
		tin(edges.size()),
		up(edges.size()),
		mark(edges.size())
	{}

	void init(int const& start) {
		dfs(start, -1);
	}

	std::vector<std::pair<int, int>> const& get() const {
		return found_bridges;
	}
};

void dfs(std::size_t const& v, std::vector<std::vector<int>> const& children, std::vector<int> &comp) {
	comp[v] = 1;
	for (int const& item : children[v]) if (comp[item] == -1) dfs(item, children, comp);
}

int main() {
	std::size_t n, m;
	std::scanf("%zu %zu", &n, &m);
	std::vector<std::vector<int>> edges(n + 1, std::vector<int>());
	std::map<std::pair<int, int>, std::size_t> numbered_edges;
	std::vector<int> comp(n + 1);
	std::vector<int> start;
	std::vector<int> answer;
	std::fill(comp.begin(), comp.end(), -1);
	bridges bridges(edges);
	for (std::size_t i = 1; i < m + 1; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		edges[u].push_back(v);
		edges[v].push_back(u);
		numbered_edges[{std::min(u, v), std::max(u, v)}] = i;
	}
	for (std::size_t i = 1; i < n + 1; i++) {
		if (comp[i] == -1) {
			dfs(i, edges, comp);
			start.push_back(static_cast<int>(i));
		}
	}
	for (int const& item : start) bridges.init(item);
	for (auto const& item : bridges.get()) answer.emplace_back(numbered_edges[item]);
	std::printf("%zu\n", answer.size());
	std::sort(answer.begin(), answer.end(), std::less<>());
	for (int const& item : answer) std::printf("%i ", item);
}
