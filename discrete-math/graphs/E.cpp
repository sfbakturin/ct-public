#include <cstdio>
#include <vector>
#include <queue>
#include <utility>
#include <algorithm>
#include <set>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main() {
	std::size_t n;
	std::scanf("%zu", &n);
	std::vector<std::vector<int>> edge(n + 1, std::vector<int>());
	std::vector<int> deg(n + 1);
	std::priority_queue<int, std::vector<int>, std::greater<>> queue;
	for (std::size_t i = 1; i < n; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		edge[u].push_back(v);
		edge[v].push_back(u);
		deg[u]++;
		deg[v]++;
	}
	for (std::size_t i = 1; i < n + 1; i++) if (deg[i] == 1) queue.push(static_cast<int>(i));
	while (n != 2) {
		n--;
		int leaf = queue.top();
		queue.pop();
		int incident = *edge[leaf].begin();
		std::printf("%i", incident);
		edge[incident].erase(std::find(edge[incident].begin(), edge[incident].end(), leaf));
		deg[incident]--;
		if (deg[incident] == 1) queue.push(incident);
		std::printf(" ");
	}
}
