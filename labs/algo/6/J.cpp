#include <cstdio>
#include <vector>
#include <utility>
#include <algorithm>
#include <limits>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

class Centroid {
private:
	std::size_t const m_n = 0;
	std::vector<std::vector<int>> m_dist;
	std::vector<std::vector<std::pair<int, int>>> m_components;
	int *m_size = nullptr;
	int *m_parent = nullptr;
	int *m_height = nullptr;
	bool *m_paint = nullptr;
	bool *m_centroid = nullptr;
	int **m_prefix = nullptr;

	int init_dfs(std::vector<std::vector<int>> const &tree, int const x = 1, bool const flag = false, int const h = 0) {
		if (m_paint[x] || m_centroid[x]) return 0;
		m_paint[x] = true;
		m_size[x] = 1;
		m_height[x] = h;
		if (flag) m_dist[x].push_back(h);
		for (int const &item : tree[x]) m_size[x] += init_dfs(tree, item, flag, h + 1);
		m_paint[x] = false;
		return m_size[x];
	}

	int init_centroid(std::vector<std::vector<int>> const &tree, int const tree_size, int const x = 1) {
		if (m_paint[x] || m_centroid[x]) return x;
		m_paint[x] = true;
		int max_subtree = 0;
		for (int const &item : tree[x]) {
			if (m_size[item] > (tree_size / 2) && m_size[item] > m_size[max_subtree] && !m_centroid[item] && !m_paint[item]) max_subtree = item;
		}
		int result;
		if (max_subtree) result = init_centroid(tree, tree_size, max_subtree);
		else result = x;
		m_paint[x] = false;
		return result;
	}

	void init_centroids(std::vector<std::vector<int>> const &tree, int const x, int const depth = 0) {
		if (m_centroid[x]) return;
		m_centroid[x] = true;
		m_components[x].push_back({x, 0});
		for (int const &item : tree[x]) {
			if (m_centroid[item]) continue;
			m_size[item] = init_dfs(tree, item, true, 1);
			int const found = init_centroid(tree, m_size[item], item);
			m_parent[found] = x;
			m_components[x].push_back({found, m_height[found]});
			init_centroids(tree, found, depth + 1);
			for (std::pair<int, int> const &y : m_components[found]) {
				if (y.second) m_components[x].push_back({y.first, m_dist[y.first][depth]});
			}
		}
	}

	void init_sort() {
		for (std::size_t i = 1; i < m_components.size(); i++) {
			std::sort(m_components[i].begin(), m_components[i].end(), [](std::pair<int, int> a, std::pair<int, int> b) {
				if (a.second == b.second) return a.first < b.first;
				else return a.second < b.second;
			});
			m_prefix[i] = new int[m_components[i].size()]();
			int min = m_components[i][0].first;
			for (std::size_t j = 0; j < m_components[i].size(); j++) {
				min = std::min(min, m_components[i][j].first);
				m_prefix[i][j] = min;
			}
		}
	}

	static int binary_search(std::vector<std::pair<int, int>> const &array, int const d) {
		int l = 0, r = static_cast<int>(array.size()) - 1;
		while (l <= r) {
			int const m = (l + r) / 2;
			if (array[m].second < d) l = m + 1;
			else if (array[m].second > d) r = m - 1;
			else return m;
		}
		return r;
	}

public:
	Centroid() = default;

	explicit Centroid(std::size_t const &n, std::vector<std::vector<int>> const &tree) :
		m_n(n),
		m_dist(m_n + 1, std::vector<int>()),
		m_components(m_n + 1, std::vector<std::pair<int, int>>()),
		m_size(new int[m_n + 1]()),
		m_parent(new int[m_n + 1]()),
		m_height(new int[m_n + 1]()),
		m_paint(new bool[m_n + 1]()),
		m_centroid(new bool[m_n + 1]()),
		m_prefix(new int*[m_n + 1]())
	{
		m_size[1] = init_dfs(tree);
		int const first = init_centroid(tree, m_size[1]);
		init_centroids(tree, first);
		init_sort();
	}

	int min(int const &v, int const &d) {
		int index = m_prefix[v][binary_search(m_components[v], d)];
		for (int j = 0; j < m_dist[v].size(); j++) {
			int const delta = m_dist[v][j];
			if (d - delta < 0) continue;
			int parentof = v, counter = static_cast<int>(m_dist[v].size()) - j;
			while (counter) {
				parentof = m_parent[parentof];
				counter--;
			}
			index = std::min(index, m_prefix[parentof][binary_search(m_components[parentof], d - delta)]);
		}
		return index;
	}
};

int main() {
	std::size_t n, m;
	std::scanf("%zu %zu", &n, &m);
	std::vector<std::vector<int>> tree(n + 1, std::vector<int>());
	for (std::size_t i = 1; i != n; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		tree[u].push_back(v);
		tree[v].push_back(u);
	}
	Centroid centroid(n, tree);
	for (std::size_t i = 0; i < m; i++) {
		int v, d;
		std::scanf("%i %i", &v, &d);
		std::printf("%i\n", centroid.min(v, d));
	}
}
