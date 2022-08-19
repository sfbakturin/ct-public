#include <cstdio>
#include <vector>
#include <limits>
#include <map>
#include <cmath>
#include <utility>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define MIN std::numeric_limits<int>::min()

class Node {
private:
	std::vector<int> children;

public:
	Node() = default;
	~Node() = default;
	void add(int const x) { children.push_back(x); }
	const std::vector<int> &get() const { return children; }
};

class SegmentTree {
private:
	bool *array = nullptr;
	int size = 0;

	void add(int const l, int const r, int const v, int const lx, int const rx) {
		if (r <= lx || rx <= l) {
			return;
		}
		if (l <= lx && rx <= r) {
			if (rx - lx != 1) {
				push(v);
			}
			array[v] = true;
			return;
		}
		push(v);
		int const m = (lx + rx + 1) / 2;
		add(l, r, 2 * v + 1, lx, m);
		add(l, r, 2 * v + 2, m, rx);
		array[v] = false;
	}

	bool get(int const i, int const v, int const lx, int const rx) {
		if (rx - lx == 1) {
			return array[v];
		}
		push(v);
		int const m = (lx + rx + 1) / 2;
		if (i < m) {
			return get(i, 2 * v + 1, lx, m);
		} else {
			return get(i, 2 * v + 2, m, rx);
		}
	}

	void push(int const v) {
		if (array[v]) {
			array[2 * v + 1] = true;
			array[2 * v + 2] = true;
			array[v] = false;
		}
	}

public:
	SegmentTree() = default;
	SegmentTree(int const n) {
		size = static_cast<int>(std::pow(2, std::ceil(std::log2(n))));
		array = new bool[2 * size - 1]();
	}

	void add(int const l, int const r) { add(l, r, 0, 0, size); }
	bool get(int const i) { return get(i, 0, 0, size); }
};

int dfs_children(std::vector<std::vector<int>> &children, Node const *nodes, bool *checked, int const current_parent, int const current_depth, int *height, int *depth, int *maxs, int *parent, int const x) {
	if (checked[x]) {
		return 0;
	}
	int mx = MIN;
	int mx_id = x;
	height[x] = 1;
	depth[x] = current_depth;
	parent[x] = current_parent;
	checked[x] = true;
	std::vector<int> const &childs = nodes[x].get();
	for (auto const y : childs) {
		if (!checked[y]) {
			children[x].push_back(y);
		}
		int const inc = dfs_children(children, nodes, checked, x, current_depth + 1, height, depth, maxs, parent, y);
		height[x] += inc;
		if (inc > mx) {
			mx = inc;
			mx_id = y;
		}
	}
	maxs[x] = mx_id;
	return height[x];
}

void dfs_pathbuilding(std::map<int, int> &paths, int *top, int *pos, int const x, int const current_pos, int const current_top, int const *maxs, std::vector<std::vector<int>> const &children, std::map<int, std::pair<int, int>> &ribs, int const prev, int &id, int *stateIndex, int &stateIndexPos) {
	if (x != prev) {
		ribs[id++] = {prev, x};
	}
	stateIndex[x] = stateIndexPos++;
	top[x] = current_top;
	pos[x] = current_pos;
	if (children[x].empty()) {
		paths[top[x]] = current_top + 1;
	} else {
		for (auto const &y : children[x]) {
			if (maxs[x] == y) {
				dfs_pathbuilding(paths, top, pos, y, current_pos + 1, current_top, maxs, children, ribs, x, id, stateIndex, stateIndexPos);
			}
		}
		for (auto const &y : children[x]) {
			if (maxs[x] != y) {
				dfs_pathbuilding(paths, top, pos, y, 0, y, maxs, children, ribs, x, id, stateIndex, stateIndexPos);
			}
		}
	}
}

void act(int u, int v, int const *top, int const *pos, int const *parent, int const *depth, std::map<int, std::pair<int, int>> &ribs, SegmentTree &pathsTree, int const *stateIndex) {
	int top_last = 0;
	while (true) {
		if (top[u] == top[v]) {
			if (depth[u] > depth[v]) {
				pathsTree.add(stateIndex[v], stateIndex[u]);
			} else {
				pathsTree.add(stateIndex[u], stateIndex[v]);
			}
			break;
		}
		if (depth[top[u]] > depth[top[v]]) {
			if (top[u] == u) {
				pathsTree.add(stateIndex[top[u]] - 1, stateIndex[u]);
			} else {
				if (stateIndex[top[u]] < stateIndex[u]) {
					pathsTree.add(stateIndex[top[u]], stateIndex[u]);
				} else {
					pathsTree.add(stateIndex[u], stateIndex[top[u]]);
				}
			}
			pathsTree.add(stateIndex[top[u]] - 1, stateIndex[top[u]]);
			u = parent[top[u]];
		} else {
			if (top[v] == v) {
				pathsTree.add(stateIndex[top[v]] - 1, stateIndex[v]);
			} else {
				if (stateIndex[top[v]] < stateIndex[v]) {
					pathsTree.add(stateIndex[top[v]], stateIndex[v]);
				} else {
					pathsTree.add(stateIndex[v], stateIndex[top[v]]);
				}
			}
			pathsTree.add(stateIndex[top[v]] - 1, stateIndex[top[v]]);
			v = parent[top[v]];
		}
	}
}

int main() {
	int n, m;
	std::scanf("%i", &n);
	Node *nodes = new Node[n + 1]();
	std::vector<std::vector<int>> children(n + 1, std::vector<int>());
	std::map<int, std::pair<int, int>> ribs;
	bool *checked = new bool[n + 1]();
	int *height = new int[n + 1]();
	int *depth = new int[n + 1]();
	int *maxs = new int[n + 1]();
	int *parent = new int[n + 1]();
	parent[0] = 1;
	parent[1] = 1;
	for (int i = 2; i < n + 1; i++) {
		int b, e;
		std::scanf("%i %i", &b, &e);
		nodes[b].add(e);
		nodes[e].add(b);
	}
	height[1] = dfs_children(children, nodes, checked, 1, 0, height, depth, maxs, parent, 1);
	delete[] nodes;
	delete[] checked;
	delete[] height;
	std::map<int, int> paths;
	int *top = new int[n + 1]();
	int *pos = new int[n + 1]();
	int *stateIndex = new int[n + 1]();
	int id = 0, stateIndexPos = 0;
	dfs_pathbuilding(paths, top, pos, 1, 0, 1, maxs, children, ribs, 1, id, stateIndex, stateIndexPos);
	delete[] maxs;
	children.clear();
	paths.clear();
	SegmentTree pathsTree(n);
	bool *lca = new bool[n + 1]();
	std::scanf("%i", &m);
	for (int i = 0; i < m; i++) {
		int x, y;
		std::scanf("%i %i", &x, &y);
		act(x, y, top, pos, parent, depth, ribs, pathsTree, stateIndex);
	}
	int count = 0;
	for (const auto &item : ribs) {
		if (!pathsTree.get(item.first)) {
			count++;
		}
	}
	std::printf("%i", count);
}
