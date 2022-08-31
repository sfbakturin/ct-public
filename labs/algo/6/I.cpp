#include <cstdio>
#include <vector>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

class Centroid {
private:
	std::size_t const m_n = 0;
	int *m_size = nullptr;
	int *m_parent = nullptr;
	bool *m_paint = nullptr;
	bool *m_centroid = nullptr;

	int init_dfs(std::vector<std::vector<int>> const &tree, int const &x = 1) {
		if (m_paint[x] || m_centroid[x]) return 0;
		m_paint[x] = true;
		m_size[x] = 1;
		for (int const &item : tree[x]) m_size[x] += init_dfs(tree, item);
		m_paint[x] = false;
		return m_size[x];
	}

	int init_centroid(std::vector<std::vector<int>> const &tree, int const &tree_size, int const &x = 1) {
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

	void init_centroids(std::vector<std::vector<int>> const &tree, int const &x) {
		if (m_centroid[x]) return;
		m_centroid[x] = true;
		for (int const &item : tree[x]) {
			if (m_centroid[item]) continue;
			m_size[item] = init_dfs(tree, item);
			int const found = init_centroid(tree, m_size[item], item);
			m_parent[found] = x;
			init_centroids(tree, found);
		}
	}

public:
	Centroid() = default;

	explicit Centroid(std::size_t const &n, std::vector<std::vector<int>> const &tree) :
		m_n(n),
		m_size(new int[m_n + 1]()),
		m_parent(new int[m_n + 1]()),
		m_paint(new bool[m_n + 1]()),
		m_centroid(new bool[m_n + 1]())
	{
		m_size[1] = init_dfs(tree);
		int const first = init_centroid(tree, m_size[1]);
		init_centroids(tree, first);
	}

	void show() {
		for (std::size_t i = 1; i <= m_n; i++) std::printf("%i ", m_parent[i]);
	}
};

int main() {
	std::size_t n;
	std::scanf("%zu", &n);
	std::vector<std::vector<int>> tree(n + 1, std::vector<int>());
	for (std::size_t i = 1; i < n; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		tree[u].push_back(v);
		tree[v].push_back(u);
	}
	Centroid centroid(n, tree);
	centroid.show();
}
