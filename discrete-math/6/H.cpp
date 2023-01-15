#include <cstdio>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cstring>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct graph {
private:
	mutable std::size_t size = 0;
	mutable bool *matrix = nullptr;

public:
	graph() = default;
	graph(std::size_t const& n) : size(n), matrix(new bool[size * size]()) {
		for (std::size_t i = 0; i < size; i++) {
			matrix[i * size + i] = true;
		}
	}
	graph(graph const& other) : size(other.size), matrix(new bool[size * size]()) {
		std::memcpy(matrix, other.matrix, size * size);
	}
	~graph() {
		delete[] matrix;
		matrix = nullptr;
	}

	void add_edge(int const& u, int const& v) {
		matrix[u * size + v] = matrix[v * size + u] = true;
	}

	void remove_edge(std::pair<std::size_t, std::size_t> &&uv) {
		matrix[uv.first * size + uv.second] = matrix[uv.second * size + uv.first] = false;
	}

	void link_children(std::size_t const& from, std::size_t const& to) {
		matrix[from * size  + from] = false;
		for (std::size_t i = 0; i < size; i++) {
			if (matrix[i * size + from]) {
				matrix[i * size + to] = matrix[to * size + i] = true;
			}
			matrix[i * size + from] = matrix[from * size + i] = false;
		}
	}

	std::size_t is_dots() const {
		std::size_t count = 0;
		for (std::size_t i = 0; i < size; i++) {
			for (std::size_t j = 0; j < size; j++) {
				if (i == j && matrix[i * size + j]) {
					count++;
				} else {
					if (i != j && matrix[i * size + j]) {
						return 0;
					}
				}
			}
		}
		return count;
	}

	std::pair<std::size_t, std::size_t> get_edge() const {
		for (std::size_t i = 0; i < size; i++) {
			for (std::size_t j = 0; j < size; j++) {
				if (matrix[i * size + j] && i != j) return {i, j};
			}
		}
		return {0, 0};
	}
};

graph make_without_edge(graph const& g) {
	graph res(g);
	auto uv = g.get_edge();
	res.remove_edge(std::move(uv));
	return res;
}

graph make_tightening_edge(graph const& g) {
	graph res(g);
	auto uv = g.get_edge();
	res.link_children(uv.second, uv.first);
	return res;
}

void get_chromatic(std::vector<long long> &chromatic, graph &&g) {
	auto res = g.is_dots();
	if (res) {
		chromatic[res]++;
	} else {
		get_chromatic(chromatic, std::move(make_without_edge(g)));
		get_chromatic(chromatic, std::move(make_tightening_edge(g)));
	}
}

int main() {
	std::size_t n, m;
	std::scanf("%zu %zu", &n, &m);
	graph g(n);
	std::vector<long long> chromatic(n + 1);
	for (std::size_t i = 0; i < m; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		g.add_edge(u - 1, v - 1);
	}
	get_chromatic(chromatic, std::move(g));
	auto func = [& chromatic]() {
		for (auto it = std::prev(chromatic.end()); it != chromatic.begin(); it--) {
			if (*it) {
				return it;
			}
		}
		return chromatic.end();
	};
	auto found = func();
	bool flag = true;
	std::printf("%zu\n", n);
	while (found != chromatic.begin()) {
		std::printf("%lld ", (flag ? *found : (-1) * *found));
		flag = !flag;
		found = std::prev(found);
	}
	if (*chromatic.begin() != 0) {
		std::printf("%lld", (flag ? *chromatic.begin() : (-1) * *chromatic.begin()));
	}
	std::printf("0\n");
}
