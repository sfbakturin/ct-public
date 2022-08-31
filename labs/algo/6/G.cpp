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

class Node {
private:
	std::vector<int> m_out;

public:
	Node() = default;

	void add(int const &x) { m_out.push_back(x); }
	std::vector<int> const &get() const { return m_out; }
};

class SegmentTree {
private:
	std::size_t size = 0;
	long long *array = nullptr;

	void add(std::size_t const &l, std::size_t const &r, long long const &x, std::size_t const &v, std::size_t const &lx, std::size_t const &rx) {
		if (r <= lx || rx <= l) return;
		if (l <= lx && rx <= r) {
			if (rx - lx != 1) push(v);
			array[v] += x;
			return;
		}
		push(v);
		std::size_t const m = (lx + rx + 1) / 2;
		add(l, r, x, 2 * v + 1, lx, m);
		add(l, r, x, 2 * v + 2, m, rx);
		array[v] = 0;
	}

	long long get(std::size_t const &i, std::size_t const &v, std::size_t const &lx, std::size_t const &rx) {
		if (rx - lx == 1) return array[v];
		push(v);
		std::size_t const m = (lx + rx + 1) / 2;
		if (i < m) return get(i, 2 * v + 1, lx, m);
		else return get(i, 2 * v + 2, m, rx);
	}

	void push(std::size_t const &v) {
		if (array[v] != 0) {
			array[2 * v + 1] += array[v];
			array[2 * v + 2] += array[v];
			array[v] = 0;
		}
	}

public:
	SegmentTree() = default;

	explicit SegmentTree(std::size_t const &v) :
		size(static_cast<std::size_t>(std::pow(2, std::ceil(std::log2(v))))),
		array(new long long[2 * size - 1]())
	{}

	void add(std::size_t const &l, std::size_t const &r, long long const &x) { add(l, r, x, 0, 0, size); }
	long long get(std::size_t const &i) { return get(i, 0, 0, size); }
};

class HLD {
private:
	std::map<int, SegmentTree> m_paths_tree;
	std::map<int, int> m_paths;
	std::vector<std::vector<int>> m_children;
	Node *m_nodes = nullptr;
	int *m_height = nullptr;
	int *m_depth = nullptr;
	int *m_maximums = nullptr;
	int *m_parent = nullptr;
	int *m_top = nullptr;
	int *m_pos = nullptr;
	bool *m_checked = nullptr;

	int dfs_children(int const &parent = 1, int const &depth = 0, int const &x = 1) {
		if (m_checked[x]) return 0;
		int mx = std::numeric_limits<int>::min();
		int mx_id = x;
		m_height[x] = 1;
		m_depth[x] = depth;
		m_parent[x] = parent;
		m_checked[x] = true;
		for (int const &item : m_nodes[x].get()) {
			if (!m_checked[item]) m_children[x].push_back(item);
			int const inc = dfs_children(x, depth + 1, item);
			m_height[x] += inc;
			if (inc > mx) {
				mx = inc;
				mx_id = item;
			}
		}
		m_maximums[x] = mx_id;
		return m_height[x];
	}

	void dfs_build(int const &x = 1,  int const &pos = 0, int const &top = 1) {
		m_top[x] = top;
		m_pos[x] = pos;
		if (m_children[x].empty()) m_paths[m_top[x]] = pos + 1;
		else {
			for (int const &y : m_children[x]) {
				if (m_maximums[x] == y) dfs_build(y, pos + 1, top);
				else dfs_build(y, 0, y);
			}
		}
	}
public:
	HLD() = default;

	explicit HLD(std::size_t const &n) :
		m_children(n + 1, std::vector<int>()),
		m_nodes(new Node[n + 1]()),
		m_height(new int[n + 1]()),
		m_depth(new int[n + 1]()),
		m_maximums(new int[n + 1]()),
		m_parent(new int[n + 1]()),
		m_top(new int[n + 1]()),
		m_pos(new int[n + 1]()),
		m_checked(new bool[n + 1]())
	{}

	Node *operator[] (std::size_t const &i) { return &m_nodes[i]; }

	void calc() {
		m_height[1] = dfs_children();
		dfs_build();
		for (auto const &y : m_paths) {
			SegmentTree st(y.second);
			m_paths_tree[y.first] = st;
		}
	}

	void act(int &u, int &v, long long const &d) {
		while (true) {
			if (m_top[u] == m_top[v]) {
				if (m_depth[u] > m_depth[v]) m_paths_tree[m_top[u]].add(m_pos[v], m_pos[u] + 1, d);
				else m_paths_tree[m_top[u]].add(m_pos[u], m_pos[v] + 1, d);
				break;
			}
			if (m_depth[m_top[u]] > m_depth[m_top[v]]) {
				m_paths_tree[m_top[u]].add(m_pos[m_top[u]], m_pos[u] + 1, d);
				u = m_parent[m_top[u]];
			} else {
				m_paths_tree[m_top[v]].add(m_pos[m_top[v]], m_pos[v] + 1, d);
				v = m_parent[m_top[v]];
			}
		}
	}

	long long get(int const &v) { return m_paths_tree[m_top[v]].get(m_pos[v]); }
};

int main() {
	std::size_t n, m;
	std::scanf("%zu", &n);
	std::vector<std::vector<int>> children(n + 1, std::vector<int>());
	HLD hld(n);
	for (std::size_t i = 2; i < n + 1; i++) {
		int u, v;
		std::scanf("%i %i", &u, &v);
		hld[u]->add(v);
		hld[v]->add(u);
	}
	hld.calc();
	std::scanf("%zu", &m);
	for (std::size_t i = 0; i < m; i++) {
		char op;
		std::scanf(" %c", &op);
		if (op == '+') {
			int u, v;
			long long d;
			std::scanf("%i %i %lli", &u, &v, &d);
			hld.act(u, v, d);
		} else {
			int v;
			std::scanf("%i", &v);
			std::printf("%lli\n", hld.get(v));
		}
	}
}
