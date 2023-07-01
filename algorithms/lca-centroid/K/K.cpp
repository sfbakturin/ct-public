#include <cstdio>
#include <vector>
#include <algorithm>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct Node {
	int count_afroamerican = 0;
	int count_white = 0;
	long long sum_afroamerican = 0;
	long long sum_white;
	std::vector<int> count_afroamericans;
	std::vector<int> count_whites;
	std::vector<long long> sum_afroamericans;
	std::vector<long long> sum_whites;
	bool afroamerican = true;

	Node() = default;
	~Node() = default;
};

class Centroid {
private:
	std::vector<std::vector<int>> m_dist;
	std::vector<std::vector<int>> m_components;
	std::vector<Node> m_nodes;
	int *m_size = nullptr;
	int *m_parent = nullptr;
	int *m_height = nullptr;
	int *m_child = nullptr;
	bool *m_paint = nullptr;
	bool *m_centroid = nullptr;

	int init_dfs(std::vector<std::vector<int>> const &tree, int const x = 1, int const h = 0, bool const flag = false, int const delta = 0) {
		if (m_paint[x] || m_centroid[x]) return 0;
		m_paint[x] = true;
		m_size[x] = 1;
		m_height[x] = h;
		if (flag) m_dist[x].push_back(h);
		for (int const &item: tree[x]) m_size[x] += init_dfs(tree, item, h + 1, flag, delta + 1);
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
		m_components[x].push_back(x);
		for (int const &item : tree[x]) {
			if (m_centroid[item]) continue;
			m_size[item] = init_dfs(tree, item, 1, true);
			int const found = init_centroid(tree, m_size[item], item);
			m_nodes[x].count_afroamericans.push_back(0);
			m_nodes[x].sum_afroamericans.push_back(0);
			m_nodes[x].count_whites.push_back(0);
			m_nodes[x].sum_whites.push_back(0);
			m_child[found] = m_nodes[x].count_afroamericans.size() - 1;
			m_parent[found] = x;
			init_centroids(tree, found, depth + 1);
			for (auto const &y : m_components[found]) {
				m_components[x].push_back(y);
				m_nodes[x].sum_afroamerican += m_dist[y][depth];
				m_nodes[x].sum_afroamericans[m_child[found]] += m_dist[y][depth];
			}
			m_nodes[x].count_afroamericans[m_child[found]] += m_components[found].size();
			m_nodes[x].count_afroamerican += m_components[found].size();
		}
	}

public:
	Centroid() = default;

	Centroid(std::size_t const &n, std::vector<std::vector<int>> &tree) :
		m_dist(n + 1, std::vector<int>()),
		m_components(n + 1, std::vector<int>()),
		m_nodes(n + 1, Node{}),
		m_size(new int[n + 1]()),
		m_parent(new int[n + 1]()),
		m_height(new int[n + 1]()),
		m_child(new int[n + 1]()),
		m_paint(new bool[n + 1]()),
		m_centroid(new bool[n + 1]())
	{
		m_size[1] = init_dfs(tree);
		int const first = init_centroid(tree, m_size[1]);
		init_centroids(tree, first);
	}

	~Centroid() = default;

	unsigned long long get(int const &v) {
		unsigned long long sum = m_nodes[v].afroamerican ? m_nodes[v].sum_afroamerican : m_nodes[v].sum_white;
		int prev = v, depth = 0;
		for (int j = m_dist[v].size() - 1; j >= 0; j--) {
			depth++;
			int const delta = m_dist[v][j];
			int parentof = v, counter = depth;
			while (counter) {
				parentof = m_parent[parentof];
				counter--;
			}
			if (m_nodes[v].afroamerican && m_nodes[parentof].afroamerican) sum += (m_nodes[parentof].sum_afroamerican - m_nodes[parentof].sum_afroamericans[m_child[prev]]) + delta * (m_nodes[parentof].count_afroamerican - m_nodes[parentof].count_afroamericans[m_child[prev]] + 1ULL);
			if (!m_nodes[v].afroamerican && !m_nodes[parentof].afroamerican) sum += (m_nodes[parentof].sum_white - m_nodes[parentof].sum_whites[m_child[prev]]) + delta * (m_nodes[parentof].count_white - m_nodes[parentof].count_whites[m_child[prev]] + + 1ULL);
			if (m_nodes[v].afroamerican && !m_nodes[parentof].afroamerican) sum += (m_nodes[parentof].sum_afroamerican - m_nodes[parentof].sum_afroamericans[m_child[prev]]) + delta * (m_nodes[parentof].count_afroamerican - m_nodes[parentof].count_afroamericans[m_child[prev]] + 0ULL);
			if (!m_nodes[v].afroamerican && m_nodes[parentof].afroamerican) sum += (m_nodes[parentof].sum_white - m_nodes[parentof].sum_whites[m_child[prev]]) + delta * (m_nodes[parentof].count_white - m_nodes[parentof].count_whites[m_child[prev]] + 0ULL);
			prev = parentof;
		}
		return sum;
	}

	void set(int const &v) {
		bool const was = m_nodes[v].afroamerican;
		m_nodes[v].afroamerican ^= true;
		int prev = v, depth = 0;
		for (int j = m_dist[v].size() - 1; j >= 0; j--) {
			depth++;
			int const delta = m_dist[v][j];
			int parentof = v, counter = depth;
			while (counter) {
				parentof = m_parent[parentof];
				counter--;
			}
			if (was) {
				m_nodes[parentof].count_afroamerican--;
				m_nodes[parentof].count_white++;
				m_nodes[parentof].sum_afroamerican -= delta;
				m_nodes[parentof].sum_white += delta;
				m_nodes[parentof].sum_afroamericans[m_child[prev]] -= delta;
				m_nodes[parentof].sum_whites[m_child[prev]] += delta;
				m_nodes[parentof].count_afroamericans[m_child[prev]]--;
				m_nodes[parentof].count_whites[m_child[prev]]++;
			} else {
				m_nodes[parentof].count_afroamerican++;
				m_nodes[parentof].count_white--;
				m_nodes[parentof].sum_afroamerican += delta;
				m_nodes[parentof].sum_white -= delta;
				m_nodes[parentof].sum_afroamericans[m_child[prev]] += delta;
				m_nodes[parentof].sum_whites[m_child[prev]] -= delta;
				m_nodes[parentof].count_afroamericans[m_child[prev]]++;
				m_nodes[parentof].count_whites[m_child[prev]]--;
			}
			prev = parentof;
		}
	}
};

int main() {
	std::size_t n, m;
	std::scanf("%zu %zu", &n, &m);
	std::vector<std::vector<int>> tree(n + 1, std::vector<int>());
	for (std::size_t i = 0; i < n - 1; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		tree[u].push_back(v);
		tree[v].push_back(u);
	}
	Centroid centroid(n, tree);
	for (std::size_t i = 0; i < m; i++) {
		int o, v;
		std::scanf("%i %i", &o, &v);
		if (o == 1) centroid.set(v);
		else std::printf("%llu\n", centroid.get(v));
	}
}
