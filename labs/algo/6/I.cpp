#include <cstdio>
#include <vector>
#include <cstring>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int dfs_size_child(std::vector<std::vector<int>> const &tree, int const x, bool const *centroid, int *size, bool *paint) {
	if (paint[x] || centroid[x]) { return 0; }
	paint[x] = true;
	size[x] = 1;
	for (int const &item : tree[x]) {
		size[x] += dfs_size_child(tree, item, centroid, size, paint);
	}
	paint[x] = false;
	return size[x];
}

int dfs_centroid(std::vector<std::vector<int>> const &tree, int const x, int const *size, int const tree_size, bool const *centroid, bool *paint) {
	if (paint[x] || centroid[x]) { return x; }
	paint[x] = true;
	int max_subtree = 0;
	for (int const &item : tree[x]) {
		if (size[item] > (tree_size / 2) && size[item] > size[max_subtree] && !centroid[item] && !paint[item]) { max_subtree = item; }
	}
	int result;
	if (max_subtree) {
		result = dfs_centroid(tree, max_subtree, size, tree_size, centroid, paint);
	} else {
		result = x;
	}
	paint[x] = false;
	return result;
}

void dfs(std::vector<std::vector<int>> const &tree, std::size_t const n, int const x, bool *centroid, bool *paint, int *parent, int *size) {
	if (centroid[x]) { return; }
	centroid[x] = true;
	for (int const &item : tree[x]) {
		if (centroid[item]) { continue; }
		size[item] = dfs_size_child(tree, item, centroid, size, paint);
		int const found = dfs_centroid(tree, item, size, size[item], centroid, paint);
		parent[found] = x;
		dfs(tree, n, found, centroid, paint, parent, size);
	}
}

int main() {
	std::size_t n;
	std::scanf("%zu", &n);
	std::vector<std::vector<int>> tree(n + 1, std::vector<int>());
	std::vector<std::vector<int>> children(n + 1, std::vector<int>());
	int *size = new int[n + 1]();
	int *parent = new int[n + 1]();
	bool *centroid = new bool[n + 1]();
	bool *paint = new bool[n + 1]();
	for (std::size_t i = 1; i < n; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		tree[u].push_back(v);
		tree[v].push_back(u);
	}
	size[1] = dfs_size_child(tree, 1, centroid, size, paint);
	int const first = dfs_centroid(tree, 1, size, size[1], centroid, paint);
	dfs(tree, n, first, centroid, paint, parent, size);
	for (std::size_t i = 1; i <= n; i++) {
		std::printf("%i ", parent[i]);
	}
}
