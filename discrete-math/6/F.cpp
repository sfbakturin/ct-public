#include <cstdio>
#include <vector>
#include <queue>
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
	std::vector<int> code;
	std::vector<int> deg(n + 1);
	std::priority_queue<int, std::vector<int>, std::greater<>> queue;
	std::fill(deg.begin(), deg.end(), 1);
	for (std::size_t i = 0; i < n - 2; i++) {
		int v;
		std::scanf("%i", &v);
		deg[v]++;
		code.push_back(v);
	}
	for (std::size_t i = 1; i < n + 1; i++) if (deg[i] == 1) queue.push(static_cast<int>(i));
	auto it = code.begin();
	while (n != 2) {
		n--;
		int leaf = queue.top();
		queue.pop();
		deg[leaf]--;
		std::printf("%i %i\n", leaf, *it);
		deg[*it]--;
		if (deg[*it] == 1 && n != 2) queue.push(*it);
		it++;
	}
	std::printf("%i %i\n", *std::prev(it), queue.top());
}
