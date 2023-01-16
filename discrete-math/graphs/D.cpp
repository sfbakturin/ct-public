#include <cstdio>
#include <list>
#include <vector>
#include <deque>
#include <iostream>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct graph {
private:
	mutable std::size_t size = 0;
	mutable bool **matrix = nullptr;

public:
	graph() = default;
	graph(std::size_t const& n) : size(n), matrix(new bool*[size + 1]) {
		for (std::size_t i = 1; i < size + 1; i++) matrix[i] = new bool[size + 1]();
	}

	void add_edge(std::size_t const& u, std::size_t const& v) {
		matrix[u][v] = true;
	}

	std::list<std::size_t> get_path() {
		std::list<std::size_t> res;
		for (std::size_t i = 1; i < size + 1; i++) {
			std::list<std::size_t>::iterator found = res.begin();
			for (; found != res.end(); found++) {
				if (matrix[i][*found]) {
					break;
				}
			}
			res.insert(found, i);
		}
		return res;
	}

	auto get_loop(std::list<std::size_t> &list) {
		auto third = std::next(list.begin());
		auto it = std::prev(list.end());
		while (it != third) {
			if (!matrix[*it][*(list.begin())]) {
				it--;
			} else {
				return std::next(it);
			}
		}
		return third;
	}

	std::list<std::size_t> edit_loop(std::list<std::size_t> &path, std::list<std::size_t>::iterator &loop) {
		std::list<std::size_t> res;
		for (auto it = path.begin(); it != loop; it++) res.push_back(*it);
		for (auto it = loop; it != path.end(); it++) {
			static auto is_exists = [& res](std::size_t const& from, bool **mat) {
				for (auto to = res.begin(); to != res.end(); to++) {
					if (mat[from][*to]) {
						return to;
					}
				}
				return res.end();
			};
			auto found = is_exists(*it, matrix);
			if (found != res.end()) {
				res.insert(found, *it);
			} else {
				auto from = it;
				while (found == res.end()) {
					it++;
					found = is_exists(*it, matrix);
				}
				res.insert(found, from, std::next(it));
			}
		}
		return res;
	}
};

int main() {
	std::size_t n;
	std::scanf("%zu", &n);
	graph g(n);
	for (std::size_t i = 1; i < n + 1; i++) {
		char c;
		for (std::size_t j = 1; j < i; j++) {
			std::scanf(" %c", &c);
			if (c == '1') {
				g.add_edge(i, j);
			} else {
				g.add_edge(j, i);
			}
		}
	}
	std::list<std::size_t> path = g.get_path();
	auto loop = g.get_loop(path);
	if (loop != path.end()) {
		path = g.edit_loop(path, loop);
	}
	for (std::size_t const& item : path) {
		std::printf("%zu ", item);
	}
}
