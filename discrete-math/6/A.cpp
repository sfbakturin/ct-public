#include <cstdio>
#include <deque>
#include <utility>
#include <algorithm>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main() {
	std::size_t n;
	std::scanf("%zu", &n);
	std::deque<int> vertices;
	bool **matrix = new bool*[n + 1];
	for (std::size_t i = 1; i < n + 1; i++) {
		char c;
		vertices.push_back(static_cast<int>(i));
		matrix[i] = new bool[n + 1]();
		for (std::size_t j = 1; j < i; j++) {
			std::scanf(" %c", &c);
			matrix[i][j] = c != '0';
			matrix[j][i] = c != '0';
		}
	}
	for (std::size_t counter = 0; counter < n * (n - 1); counter++) {
		if (matrix[vertices[0]][vertices[1]]) {
			vertices.push_back(vertices.front());
			vertices.pop_front();
		} else {
			int i = 2;
			for (; i < vertices.size(); i++) if (matrix[vertices[0]][vertices[i]] && matrix[vertices[1]][vertices[i + 1]]) break;
			for (int j = 0; ; j++) {
				if (1 + j < i - j) std::swap(vertices[1 + j], vertices[i - j]);
				else break;
			}
			vertices.push_back(vertices.front());
			vertices.pop_front();
		}
	}
	for (int const& item : vertices) std::printf("%i ", item);
}
