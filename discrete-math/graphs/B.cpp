#include <cstdio>
#include <vector>
#include <deque>
#include <optional>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct graph {
private:
	mutable bool **matrix = nullptr;
	mutable std::deque<int> vertices;
	mutable std::size_t size;

	std::optional<int> get_by_dirak() {
		int i = 2;
		for (; i < vertices.size(); i++) if (matrix[vertices[0]][vertices[i]] && matrix[vertices[1]][vertices[i + 1]]) return std::make_optional<int>(i);
		return {};
	}

	int get_by_chvatal() {
		int i = 1;
		for (; i < vertices.size(); i++) if (matrix[vertices[0]][vertices[i]]) return i;
		return -1;
	}

public:
	graph(std::size_t const& size) : matrix(new bool*[size + 1]), size(size) {}

	void mem(std::size_t const& i) {
		vertices.push_back(static_cast<int>(i));
		matrix[i] = new bool[size + 1]();
	}

	void add(std::size_t const& i, std::size_t const& j, bool const& flag) {
		matrix[i][j] = matrix[j][i] = flag;
	}

	void loop() {
		for (std::size_t counter = 0; counter < size * (size - 1); counter++) {
			if (matrix[vertices[0]][vertices[1]]) {
				vertices.push_back(vertices.front());
				vertices.pop_front();
			} else {
				auto found = get_by_dirak();
				int i = 0;
				if (found.has_value()) i = *found;
				else i = get_by_chvatal();
				for (int j = 0; ; j++) {
					if (1 + j < i - j) std::swap(vertices[1 + j], vertices[i - j]);
					else break;
				}
				vertices.push_back(vertices.front());
				vertices.pop_front();
			}
		}
	}

	std::deque<int> const& get() const {
		return vertices;
	}
};

int main() {
	std::size_t n;
	std::scanf("%zu", &n);
	graph g(n);
	for (std::size_t i = 1; i < n + 1; i++) {
		char c;
		g.mem(i);
		for (std::size_t j = 1; j < i; j++) {
			std::scanf(" %c", &c);
			g.add(i, j, c != '0');
		}
	}
	g.loop();
	for (int const& item : g.get()) std::printf("%i ", item);
}
