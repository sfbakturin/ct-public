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

class Node {
private:
	std::vector<int> children;

public:
	Node() = default;

	void add(int const &x) { children.push_back(x); }
	std::vector<int> const &get() const { return children; }
};

class SegmentTree {
private:
	std::size_t m_size = 0;
	bool *m_array = nullptr;

	void add(std::size_t const &l, std::size_t const &r, std::size_t const &v, std::size_t const &lx, std::size_t const &rx) {
		if (r <= lx || rx <= l) return;
		if (l <= lx && rx <= r) {
			if (rx - lx != 1) push(v);
			m_array[v] = true;
			return;
		}
		push(v);
		std::size_t const m = (lx + rx + 1) / 2;
		add(l, r, 2 * v + 1, lx, m);
		add(l, r, 2 * v + 2, m, rx);
		m_array[v] = false;
	}

	bool get(std::size_t const &i, std::size_t const &v, std::size_t const &lx, std::size_t const &rx) {
		if (rx - lx == 1) return m_array[v];
		push(v);
		std::size_t const m = (lx + rx + 1) / 2;
		if (i < m) return get(i, 2 * v + 1, lx, m);
		else return get(i, 2 * v + 2, m, rx);
	}

	void push(std::size_t const &v) {
		if (m_array[v]) {
			m_array[2 * v + 1] = true;
			m_array[2 * v + 2] = true;
			m_array[v] = false;
		}
	}

public:
	SegmentTree() = default;

	explicit SegmentTree(std::size_t const &n) :
		m_size(static_cast<std::size_t>(std::pow(2, std::ceil(std::log2(n))))),
		m_array(new bool[2 * m_size - 1]())
	{}

	void add(std::size_t const &l, std::size_t const &r) { add(l, r, 0, 0, m_size); }
	bool get(std::size_t const &i) { return get(i, 0, 0, m_size); }
};

class HLD {
private:
	SegmentTree m_paths_tree;
	std::map<int, int> m_paths;
	std::vector<std::vector<int>> m_children;
	std::map<int, std::pair<int, int>> m_ribs;
	Node *m_nodes = nullptr;
	int *m_height = nullptr;
	int *m_depth = nullptr;
	int *m_maximums = nullptr;
	int *m_parent = nullptr;
	int *m_top = nullptr;
	int *m_index = nullptr;
	bool *m_checked = nullptr;

	int dfs_children(int const &parent = 1, int const &depth = 0, int const &x = 1) {
		if (m_checked[x]) return 0;
		int mx = std::numeric_limits<int>::min();
		int mx_id = x;
		m_height[x] = 1;
		m_depth[x] = depth;
		m_parent[x] = parent;
		m_checked[x] = true;
		for (int const &y : m_nodes[x].get()) {
			if (!m_checked[y]) m_children[x].push_back(y);
			int const inc = dfs_children(x, depth + 1, y);
			m_height[x] += inc;
			if (inc > mx) {
				mx = inc;
				mx_id = y;
			}
		}
		m_maximums[x] = mx_id;
		return m_height[x];
	}

	void dfs_build(int &id, int &state_index_pos, int const &x = 1,  int const &top = 1, int const &prev = 1) {
		if (x != prev) m_ribs[id++] = {prev, x};
		m_index[x] = state_index_pos++;
		m_top[x] = top;
		if (m_children[x].empty()) m_paths[m_top[x]] = top + 1;
		else {
			for (int const &y : m_children[x]) {
				if (m_maximums[x] == y) dfs_build(id, state_index_pos, y, top, x);
			}
			for (int const &y : m_children[x]) {
				if (m_maximums[x] != y) dfs_build(id, state_index_pos, y, y, x);
			}
		}
	}
public:
	HLD() = default;

	explicit HLD(std::size_t const &n) :
		m_paths_tree(n),
		m_children(n + 1, std::vector<int>()),
		m_nodes(new Node[n + 1]()),
		m_height(new int[n + 1]()),
		m_depth(new int[n + 1]()),
		m_maximums(new int[n + 1]()),
		m_parent(new int[n + 1]()),
		m_top(new int[n + 1]()),
		m_index(new int[n + 1]()),
		m_checked(new bool[n + 1]())
	{
		m_parent[0] = 1;
		m_parent[1] = 1;
	}

	Node *operator[] (std::size_t const &i) { return &m_nodes[i]; }
	std::map<int, std::pair<int, int>> const &operator() () const { return m_ribs; }
	SegmentTree &get_tree() { return m_paths_tree; }

	void calc() {
		m_height[1] = dfs_children();
		int id = 0, state_index_pos = 0;
		dfs_build(id, state_index_pos);
	}

	void act(int &u, int &v) {
		while (true) {
			if (m_top[u] == m_top[v]) {
				if (m_depth[u] > m_depth[v]) m_paths_tree.add(m_index[v], m_index[u]);
				else m_paths_tree.add(m_index[u], m_index[v]);
				break;
			}
			if (m_depth[m_top[u]] > m_depth[m_top[v]]) {
				if (m_top[u] == u) m_paths_tree.add(m_index[m_top[u]] - 1, m_index[u]);
				else {
					if (m_index[m_top[u]] < m_index[u]) m_paths_tree.add(m_index[m_top[u]], m_index[u]);
					else m_paths_tree.add(m_index[u], m_index[m_top[u]]);
				}
				m_paths_tree.add(m_index[m_top[u]] - 1, m_index[m_top[u]]);
				u = m_parent[m_top[u]];
			} else {
				if (m_top[v] == v) m_paths_tree.add(m_index[m_top[v]] - 1, m_index[v]);
				else {
					if (m_index[m_top[v]] < m_index[v]) m_paths_tree.add(m_index[m_top[v]], m_index[v]);
					else m_paths_tree.add(m_index[v], m_index[m_top[v]]);
				}
				m_paths_tree.add(m_index[m_top[v]] - 1, m_index[m_top[v]]);
				v = m_parent[m_top[v]];
			}
		}
	}
};

int main() {
	std::size_t n, m;
	std::scanf("%zu", &n);
	std::vector<std::vector<int>> children(n + 1, std::vector<int>());
	HLD hld(n);
	for (std::size_t i = 2; i < n + 1; i++) {
		int b, e;
		std::scanf("%i %i", &b, &e);
		hld[b]->add(e);
		hld[e]->add(b);
	}
	hld.calc();
	std::scanf("%zu", &m);
	for (std::size_t i = 0; i < m; i++) {
		int x, y;
		std::scanf("%i %i", &x, &y);
		hld.act(x, y);
	}
	int count = 0;
	for (auto const &item : hld()) {
		if (!hld.get_tree().get(item.first)) count++;
	}
	std::printf("%i", count);
}
