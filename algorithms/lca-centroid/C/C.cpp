#include <cstdio>
#include <cmath>
#include <vector>
#include <utility>
#include <limits>
#include <algorithm>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define MAX std::numeric_limits<int>::max()

class LCA {
private:
	std::size_t m_n = 0;
	std::size_t m_nlog = 0;
	int *m_height = nullptr;
	std::vector<std::vector<std::pair<int, int>>> m_up;

public:
	LCA() = default;

	explicit LCA(std::size_t const &n) :
		m_n(n),
		m_nlog(static_cast<std::size_t>(std::log2(m_n + 1)) + 1),
		m_height(new int[m_n + 1]()),
		m_up(m_nlog, std::vector<std::pair<int, int>>())
	{
		for (std::size_t i = 0; i < m_nlog; i++) m_up[i] = std::vector<std::pair<int, int>>(n + 1, {1, MAX});
		m_up[0][0].first = 1;
		m_up[0][1].first = 1;
	}

	void set(std::size_t const &i, int const &x, int const &y) {
		m_up[0][i].first = x;
		m_up[0][i].second = y;
	}

	void calc() {
		for (std::size_t d = 1; d < m_nlog; d++) {
			for (std::size_t i = 1; i < m_n + 1; i++) {
				m_up[d][i].first = m_up[d - 1][m_up[d - 1][i].first].first;
				m_up[d][i].second = std::min(m_up[d - 1][m_up[d - 1][i].first].second, m_up[d - 1][i].second);
			}
		}
	}

	void dfs(std::vector<std::vector<int>> const &children, int const x = 1, int const h = 0) {
		m_height[x] = h;
		for (int const &y : children[x]) dfs(children, y, h + 1);
	}

	int lca(int &u, int &v) {
		int min = MAX;
		if (m_height[u] > m_height[v]) std::swap(u, v);
		for (std::size_t i = m_nlog - 1; i != 0; i--) {
			if (m_height[m_up[i][v].first] - m_height[u] >= 0) {
				min = std::min(min, m_up[i][v].second);
				v = m_up[i][v].first;
			}
		}
		if (m_height[m_up[0][v].first] - m_height[u] >= 0) {
			min = std::min(min, m_up[0][v].second);
			v = m_up[0][v].first;
		}
		if (u == v) return min;
		for (std::size_t i = m_nlog - 1; i != 0; i--) {
			if (m_up[i][u] != m_up[i][v]) {
				min = std::min(m_up[i][u].second, std::min(m_up[i][v].second, min));
				u = m_up[i][u].first;
				v = m_up[i][v].first;
			}
		}
		if (m_up[0][u] != m_up[0][v]) {
			min = std::min(m_up[0][u].second, std::min(m_up[0][v].second, min));
			u = m_up[0][u].first;
			v = m_up[0][v].first;
		}
		return min;
	}
};

int main() {
	std::size_t n, m;
	std::scanf("%zu", &n);
	LCA lca(n);
	std::vector<std::vector<int>> children(n + 1, std::vector<int>());
	for (std::size_t i = 2; i < n + 1; i++) {
		int x, y;
		std::scanf("%i %i", &x, &y);
		children[x].push_back(static_cast<int>(i));
		lca.set(i, x, y);
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