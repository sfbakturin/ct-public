#include <cstdio>
#include <vector>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main() {
	int n, m;
	std::scanf("%i %i", &n, &m);
	std::vector<std::vector<int>> edge_out(n + 1, std::vector<int>());
	std::vector<int> deg_in(n + 1);
	std::vector<int> top_sort;
	std::vector<int> queue;
	for (int i = 0; i < m; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		edge_out[u].push_back(v);
		deg_in[v]++;
	}
	for (int i = 1; i < n + 1; i++) if (!deg_in[i]) queue.push_back(i);
	while (!queue.empty()) {
		int v = queue.back();
		queue.pop_back();
		top_sort.push_back(v);
		for (int const& u : edge_out[v]) {
			deg_in[u]--;
			if (!deg_in[u]) queue.push_back(u);
		}
		n--;
	}
	if (n) std::printf("-1");
	else for (int const& item : top_sort) std::printf("%i ", item);
}
