#include <cstdio>
#include <vector>
#include <tuple>
#include <algorithm>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

class dsu {
private:
	std::vector<int> root;
	std::vector<int> rank;

	void make(int const& v) {
		root[v] = v;
		rank[v] = 0;
	}

public:
	dsu(std::size_t const &size) :
			root(std::vector<int>(size + 1)),
			rank(std::vector<int>(size + 1)) {
		for (std::size_t i = 1; i < size + 1; i++) make(static_cast<int>(i));
	}

	int find(int const& v) {
		if (v == root[v]) return v;
		else return root[v] = find(root[v]);
	}

	void unification(int const& l, int const& r) {
		int a = find(l);
		int b = find(r);
		if (a != b) {
			if (rank[a] < rank[b]) std::swap(a, b);
			root[b] = a;
			if (rank[a] == rank[b]) rank[a]++;
		}
	}
};

int main() {
	using node_t = std::tuple<int, int, int>;
	std::size_t n, m;
	std::scanf("%zu %zu", &n, &m);
	dsu dsu(n);
	std::vector<node_t> edges;
	long long sum = 0;
	for (std::size_t i = 0; i < m; i++) {
		int u, v, w;
		std::scanf("%i %i %i", &u, &v, &w);
		edges.emplace_back(u, v, w);
	}
	std::sort(edges.begin(), edges.end(), [](node_t a, node_t b){
		return std::get<2>(a) < std::get<2>(b);
	});
	for (auto const& item : edges) {
		if (dsu.find(std::get<0>(item)) != dsu.find(std::get<1>(item))) {
			sum += std::get<2>(item);
			dsu.unification(std::get<0>(item), std::get<1>(item));
		}
	}
	std::printf("%lld", sum);
}
