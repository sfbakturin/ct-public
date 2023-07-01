#include <cstdio>
#include <vector>
#include <algorithm>
#include <map>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct cut_points {
private:
	int T = 0;
	std::vector<int> tin, up;
	std::vector<bool> mark;
	std::vector<std::vector<int>> &edges;
	std::vector<int> found_points;

	void dfs(int const& curr, int const& prev) {
		tin[curr] = T++;
		up[curr] = tin[curr];
		mark[curr] = true;
		int counter = 0;
		bool ok = false;
		for (int const& item : edges[curr]) {
			if (item == prev) continue;
			if (!mark[item]) {
				dfs(item, curr);
				up[curr] = std::min(up[curr], up[item]);
				counter++;
				if (prev > 0 && tin[curr] <= up[item]) ok = true;
			} else up[curr] = std::min(up[curr], tin[item]);
		}
		if (ok || (prev < 0 && counter > 1)) found_points.emplace_back(curr);
	}

public:
	cut_points(std::vector<std::vector<int>> &edges) :
		edges(edges),
		tin(edges.size()),
		up(edges.size()),
		mark(edges.size())
	{}

	void init(int const& start) {
		dfs(start, -1);
	}

	std::vector<int> &get() {
		return found_points;
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
	std::vector<int> comp(n + 1);
	std::vector<int> start;
	std::fill(comp.begin(), comp.end(), -1);
	cut_points cut_points(edges);
	for (std::size_t i = 1; i < m + 1; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		edges[u].push_back(v);
		edges[v].push_back(u);
	}
	for (std::size_t i = 1; i < n + 1; i++) {
		if (comp[i] == -1) {
			dfs(i, edges, comp);
			start.push_back(static_cast<int>(i));
		}
	}
	for (int const& item : start) cut_points.init(item);
	std::printf("%zu\n", cut_points.get().size());
	std::sort(cut_points.get().begin(), cut_points.get().end(), std::less<>());
	for (int const& item : cut_points.get()) std::printf("%i ", item);
}
