#include <cstdio>
#include <cmath>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

class LCA {
private:
	std::size_t const m_n = 0;
	std::size_t const m_nlog = 0;
	int *m_parent = nullptr;
	int **m_up = nullptr;

public:
	LCA() = default;

	explicit LCA(std::size_t const &n) :
		m_n(n),
		m_nlog(static_cast<std::size_t>(std::log2(m_n + 1)) + 1),
		m_parent(new int[m_n + 1]()),
		m_up(new int*[m_nlog])
	{
		for (std::size_t i = 0; i < m_nlog; i++) m_up[i] = new int[m_n + 1]();
	}

	int *operator[] (std::size_t const i) { return &m_parent[i]; }

	void calc() {
		for (std::size_t i = 1; i < m_n + 1; i++) m_up[0][i] = m_parent[i];
		for (std::size_t d = 1; d < m_nlog; d++) {
			for (std::size_t i = 1; i < m_n + 1; i++) m_up[d][i] = m_up[d - 1][m_up[d - 1][i]];
		}
	}

	void print() {
		for (std::size_t i = 1; i < m_n + 1; i++) {
			std::printf("%zu: ", i);
			for (std::size_t j = 0; j < m_nlog; j++) {
				if (m_up[j][i] != 0) std::printf("%i ", m_up[j][i]);
			}
			std::printf("\n");
		}
	}
};

int main() {
	std::size_t n;
	std::scanf("%zu", &n);
	LCA lca(n);
	for (std::size_t i = 1; i < n + 1; i++) std::scanf("%i", lca[i]);
	lca.calc();
	lca.print();
}
