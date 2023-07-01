#include <cstdio>
#include <cmath>
#include <vector>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

class LCA {
private:
	std::size_t m_n = 0;
	std::size_t m_nlog = 0;
	int m_global_timer = 0;
	int *m_tin = nullptr;
	int *m_tout = nullptr;
	int *m_parent = nullptr;
	int *m_height = nullptr;
	int **m_up = nullptr;

	bool is_ancestor(int const &u, int const &v) { return (m_tin[u] <= m_tin[v]) && (m_tout[u] >= m_tout[v]); }

public:
	LCA() = default;

	explicit LCA(std::size_t const &n) :
		m_n(n),
		m_nlog(static_cast<std::size_t>(std::log2(m_n + 1)) + 1),
		m_tin(new int[m_n + 1]()),
		m_tout(new int[m_n + 1]()),
		m_parent(new int[m_n + 1]()),
		m_height(new int[m_n + 1]()),
		m_up(new int*[m_nlog]())
	{
		m_parent[0] = 1;
		m_parent[1] = 1;
		for (std::size_t i = 0; i < m_nlog; i++) m_up[i] = new int[m_n + 1]();
	}

	int *operator[] (std::size_t const i) { return &m_parent[i]; }

	void calc() {
		for (std::size_t i = 1; i < m_n + 1; i++) m_up[0][i] = m_parent[i];
		for (std::size_t d = 1; d < m_nlog; d++) {
			for (std::size_t i = 1; i < m_n + 1; i++) m_up[d][i] = m_up[d - 1][m_up[d - 1][i]];
		}
	}

	void dfs(std::vector<std::vector<int>> const &children, int const x = 1, int const h = 0) {
		m_height[x] = h;
		m_tin[x] = m_global_timer++;
		for (int const &y : children[x]) dfs(children, y, h + 1);
		m_tout[x] = m_global_timer++;
	}

	int lca(int &u, int &v) {
		if (is_ancestor(u, v)) return u;
		if (is_ancestor(v, u)) return v;
		if (m_height[u] > m_height[v]) std::swap(u, v);
		for (std::size_t i = m_nlog - 1; i != 0; i--) {
			if (m_height[m_up[i][v]] - m_height[u] >= 0) v = m_up[i][v];
		}
		if (m_height[m_up[0][v]] - m_height[u] >= 0) v = m_up[0][v];
		if (u == v) return v;
		for (std::size_t i = m_nlog - 1; i != 0; i--) {
			if (m_up[i][u] != m_up[i][v]) {
				u = m_up[i][u];
				v = m_up[i][v];
			}
		}
		if (m_up[0][u] != m_up[0][v]) {
			u = m_up[0][u];
			v = m_up[0][v];
		}
		return m_parent[u];
	}
};

int main() {
	std::size_t n, m;
	std::scanf("%zu", &n);
	LCA lca(n);
	std::vector<std::vector<int>> children(n + 1, std::vector<int>());
	for (std::size_t i = 2; i < n + 1; i++) {
		std::scanf("%i", lca[i]);
		children[*lca[i]].push_back(static_cast<int>(i));
	}
	lca.calc();
	lca.dfs(children);
	std::scanf("%zu", &m);
	for (std::size_t i = 0; i < m; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		std::printf("%i\n", lca.lca(u, v));
	}
}
