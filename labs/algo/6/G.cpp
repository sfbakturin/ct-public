#include <cmath>
#include <cstdio>
#include <limits>
#include <map>
#include <vector>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define __INT__MIN__ std::numeric_limits< int >::min()

class Node {
private:
	std::vector< int > out;

public:
	Node() = default;

	void add(int const x) { out.push_back(x); }
	int get_size() const { return out.size(); }
	int get(int const i) const { return out[i]; }
};

class SegmentTree {
private:
	long long *array = nullptr;
	int size = 0;

	void add(int const l, int const r, long long const x, int const v, int const lx, int const rx) {
		if (r <= lx || rx <= l) {
			return;
		}
		if (l <= lx && rx <= r) {
			if (rx - lx != 1) {
				push(v);
			}
			array[v] += x;
			return;
		}
		push(v);
		int const m = (lx + rx + 1) / 2;
		add(l, r, x, 2 * v + 1, lx, m);
		add(l, r, x, 2 * v + 2, m, rx);
		array[v] = 0;
	}

	long long get(int const i, int const v, int const lx, int const rx) {
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
		if (array[v] != 0) {
			array[2 * v + 1] += array[v];
			array[2 * v + 2] += array[v];
			array[v] = 0;
		}
	}

public:
	SegmentTree() = default;
	SegmentTree(int const v) {
		size = static_cast<int>(std::pow(2, std::ceil(std::log(v) / std::log(2))));
		array = new long long[2 * size - 1]();
	}

	void add(int const l, int const r, long long const x) { add(l, r, x, 0, 0, size); }
	long long get(int const i) { return get(i, 0, 0, size); }
};

int dfs_children(int const, int const, int *, int *, int *, int const, std::vector<Node> const&, int *, bool *, std::vector<std::vector<int>> &);
void dfs_pathbul(std::map<int, int> &, int *, int *, int const, int const, int const, int const*, std::vector<std::vector<int>> const&);
void act(int, int, std::map<int, SegmentTree> &, int const*, int const*, int const*, int const*, long long const);

int main() {
	int n, m;
	std::scanf("%i", &n);
	std::vector< std::vector<int>> children(n + 1, std::vector<int>());
	std::vector< Node > nodes(n + 1, Node());
	bool *paint = new bool[n + 1]();
	int *height = new int[n + 1]();
	int *top = new int[n + 1]();
	int *pos = new int[n + 1]();
	int *maxs = new int[n + 1]();
	int *parent = new int[n + 1]();
	int *depth = new int[n + 1]();
	for (int i = 2; i < n + 1; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		nodes[u].add(v);
		nodes[v].add(u);
	}
	height[1] = dfs_children(1, 0, height, depth, maxs, 1, nodes, parent, paint, children);
	nodes.clear();
	delete[] paint;
	std::map<int, int> paths;
	std::map<int, SegmentTree> sts;
	dfs_pathbul(paths, top, pos, 1, 0, 1, maxs, children);
	delete[] maxs;
	delete[] height;
	children.clear();
	for (auto const &y : paths) {
		SegmentTree st(y.second);
		sts[y.first] = st;
	}
	paths.clear();
	std::scanf("%i", &m);
	for (int i = 0; i < m; i++) {
		char op;
		std::scanf(" %c", &op);
		if (op == '+') {
			int u, v;
			long long d;
			std::scanf("%i %i %lli", &u, &v, &d);
			if (top[u] == top[v]) {
				if (depth[u] > depth[v]) {
					sts[top[u]].add(pos[v], pos[u] + 1, d);
				} else {
					sts[top[u]].add(pos[u], pos[v] + 1, d);
				}
			} else {
				act(u, v, sts, top, pos, parent, depth, d);
			}
		} else {
			int v;
			std::scanf("%i", &v);
			std::printf("%lli\n", sts[top[v]].get(pos[v]));
		}
	}
}

int dfs_children(int const curr_parent, int const curr_depth, int *height, int *depth, int *maxs, int const x, std::vector<Node> const &nodes, int *parent, bool *checked, std::vector<std::vector<int>> &children) {
	if (checked[x]) {
		return 0;
	}
	int mx = __INT__MIN__;
	int mx_id = x;
	height[x] = 1;
	depth[x] = curr_depth;
	parent[x] = curr_parent;
	checked[x] = true;
	for (int i = 0; i < nodes[x].get_size(); i++) {
		if (!checked[nodes[x].get(i)]) {
			children[x].push_back(nodes[x].get(i));
		}
		int const inc = dfs_children(x, curr_depth + 1, height, depth, maxs, nodes[x].get(i), nodes, parent, checked, children);
		height[x] += inc;
		if (inc > mx) {
			mx = inc;
			mx_id = nodes[x].get(i);
		}
	}
	maxs[x] = mx_id;
	return height[x];
}

void dfs_pathbul(std::map<int, int> &paths, int *head, int *posi, int const x, int const curr_posi, int const curr_head, int const *maxs, std::vector< std::vector<int>> const &ch) {
	head[x] = curr_head;
	posi[x] = curr_posi;
	if (ch[x].empty()) {
		paths[head[x]] = curr_posi + 1;
	} else {
		for (auto const &y : ch[x]) {
			if (maxs[x] == y) {
				dfs_pathbul(paths, head, posi, y, curr_posi + 1, curr_head, maxs, ch);
			} else {
				dfs_pathbul(paths, head, posi, y, 0, y, maxs, ch);
			}
		}
	}
}

void act(int u, int v, std::map<int, SegmentTree> &sts, int const *top, int const *pos, int const *parent, int const *depth, long long const d) {
	while (true) {
		if (top[u] == top[v]) {
			if (depth[u] > depth[v]) {
				sts[top[u]].add(pos[v], pos[u] + 1, d);
			} else {
				sts[top[u]].add(pos[u], pos[v] + 1, d);
			}
			break;
		}
		if (depth[top[u]] > depth[top[v]]) {
			sts[top[u]].add(pos[top[u]], pos[u] + 1, d);
			u = parent[top[u]];
		} else {
			sts[top[v]].add(pos[top[v]], pos[v] + 1, d);
			v = parent[top[v]];
		}
	}
}
