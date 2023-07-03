#include <cstdio>
#include <vector>
#include <deque>
#include <cstring>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

void paint(std::vector<std::vector<int>> const& edges, int const& k, std::vector<int> &color, bool *painted, bool *already) {
	auto min = [& color, edges, painted](int const& v){
		std::memset(painted, false, edges.size());
		for (int const& item : edges[v])
			painted[color[item]] = true;
		for (std::size_t i = 1; i < edges.size(); i++) {
			if (!painted[i]) return i;
		}
		/*return 0ULL;*/
	};
	std::deque<int> queue{1};
	already[1] = true;
	while (!queue.empty()) {
		int const v = queue.front();
		queue.pop_front();
		color[v] = static_cast<int>(min(v));
		for (int const& u : edges[v]) {
			if (!already[u]) {
				queue.push_back(u);
				already[u] = true;
			}
		}
	}
}

int main() {
	std::size_t n, m;
	std::scanf("%zu %zu", &n, &m);
	std::vector<std::vector<int>> edges(n + 1, std::vector<int>());
	std::vector<int> color(n + 1);
	bool *painted = new bool[n + 1]();
	bool *already = new bool[n + 1]();
	auto get_k = [& edges]() {
		std::size_t max = 0;
		for (auto const& item : edges)
			max = (item.size() > max ? item.size() : max);
		return (max % 2 ? max : max + 1);
	};
	for (std::size_t i = 0; i < m; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		edges[u].push_back(v);
		edges[v].push_back(u);
	}
	std::size_t const k = get_k();
	std::printf("%zu\n", k);
	paint(edges, static_cast<int>(k), color, painted, already);
	for (int const& item : color) if (item) std::printf("%i\n", item);
}
