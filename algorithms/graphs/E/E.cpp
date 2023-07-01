
/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define BEGIN int main() {
#define END }
#define OPTIMIZE std::ios_base::sync_with_stdio(false); std::cin.tie(0); std::cout.tie(0)

#include <cstdio>
#include <vector>
#include <map>
#include <utility>
#include <deque>
#include <set>

struct graph {
private:
	int timer = 0;
	int max_color = 0;
	std::vector<int> up;
	std::vector<int> tin;
	std::vector<bool> used;
	std::vector<bool> visited;
	std::set<std::pair<int, int>> unique_ribs;
	std::vector<std::pair<int, int>> numbered_ribs;
	std::map<std::pair<int, int>, int> colour_ribs;
	std::vector<std::vector<int>> edges;

public:
	graph(int const& size, int const& size_edge) :
		up(size + 1),
		tin(size + 1),
		used(size + 1),
		visited(size + 1),
		numbered_ribs(size_edge),
		edges(size + 1, std::vector<int>()) 
	{}

	void add(int const& u, int const& v, int const& index) {
		std::pair<int, int> edge = {std::min(u, v), std::max(u, v)};
		numbered_ribs[index] = edge;
		if (unique_ribs.find(edge) == unique_ribs.end()) {
			unique_ribs.insert(edge);
			edges[u].push_back(v);
			edges[v].push_back(u);
		}
	}

	void find_points(int const& v, int const& p = -1) {
		tin[v] = up[v] = timer++;
		used[v] = true;
		for (int const& item : edges[v]) {
			if (v == p) continue;
			if (!used[item]) {
				find_points(item, v);
				up[v] = std::min(up[v], up[item]);
			} else {
				up[v] = std::min(up[v], tin[item]);
			}
		}
	}

	void paint(int v, int color, int parent = -1) {
		visited[v] = true;
		for (int const& u : edges[v]) {
			if (u == parent) continue;
			if (visited[u]) {
				if (tin[u] < tin[v]) {
					auto edge = colour_ribs.find({std::min(u, v), std::max(u, v)});
					if (edge != colour_ribs.end()) {
						colour_ribs[{std::min(u, v), std::max(u, v)}] = std::min(color, (*edge).second);
					} else {
						colour_ribs[{std::min(u, v), std::max(u, v)}] = color;
					}
				}
			} else {
				if (up[u] >= tin[v]) {
					int const new_color = ++max_color;
					colour_ribs[{std::min(u, v), std::max(u, v)}] = new_color;
					paint(u, new_color, v);
				} else {
					colour_ribs[{std::min(u, v), std::max(u, v)}] = color;
					paint(u, color, v);
				}
			}
		}
	}

	void debug() {
		for (int i = 1; i < edges.size(); i++) {
			std::printf("up[%i] = %i, tin[%i] = %i\n", i, up[i], i, tin[i]);
		}
	}

	void view() {
		for (auto const& edge : numbered_ribs) {
			std::printf("%i ", colour_ribs.at(edge));
		}
	}

	std::vector<int> const& get_children(int const& v) const {
		return edges[v];
	}

	int const& color() const {
		return max_color;
	}
};

void select_components(int const& v, graph const& g, std::vector<int> &components) {
	components[v]++;
	for (int const& item : g.get_children(v)) {
		if (!(components[item])) select_components(item, g, components);
	}
}

BEGIN
	int n, m;
	std::scanf("%i %i", &n, &m);
	std::vector<int> components(n + 1);
	graph g(n, m);
	for (int i = 0; i < m; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		g.add(u, v, i);
	}
	for (int i = 1; i < n + 1; i++) {
		if (!components[i]) {
			select_components(i, g, components);
			g.find_points(i);
			g.paint(i, g.color());
		}
	}
	std::printf("%i\n", g.color());
	g.view();
END
