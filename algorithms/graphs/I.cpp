#include <cstdio>
#include <set>
#include <cmath>
#include <vector>
#include <limits>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

double squared_distance(int const& x0, int const& y0, int const& x1, int const& y1) {
	return std::pow(x1 - x0, 2) + std::pow(y1 - y0, 2);
}

int main() {
	std::size_t n;
	std::scanf("%zu", &n);
	std::vector<int> x(n + 1), y(n + 1);
	std::vector<double> dist(n + 1);
	std::vector<bool> visited(n + 1);
	double sum = 0;
	auto get_min = [](std::vector<bool> &visited, std::vector<double> &dist, std::size_t const& n){
		double min = std::numeric_limits<double>::infinity();
		std::size_t index = 0;
		for (std::size_t i = 1; i < n + 1; i++) {
			if (!visited[i] && (dist[i] < min)) {
				min = dist[i];
				index = i;
			}
		}
		return index;
	};
	for (std::size_t i = 1; i < n + 1; i++) {
		std::scanf("%i %i", &x[i], &y[i]);
		dist[i] = std::numeric_limits<double>::infinity();
	}
	dist[1] = 0;
	for (std::size_t i = 1; i < n + 1; i++) {
		std::size_t v = get_min(visited, dist, n);
		sum += std::sqrt(dist[v]);
		for (std::size_t u = 1; u < n + 1; u++) {
			if (!visited[u] && v != u) dist[u] = std::min(dist[u], squared_distance(x[v], y[v], x[u], y[u]));
		}
		visited[v] = true;
	}
	std::printf("%.10lf", sum);
}
