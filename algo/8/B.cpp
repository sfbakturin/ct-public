#include <iostream>
#include <memory>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define BEGIN                              \
	int main() {                           \
	std::ios_base::sync_with_stdio(false); \
	std::cin.tie(0);                       \
	std::cout.tie(0);
#define END }

struct AbstractSolver {
	constexpr AbstractSolver() noexcept = default;
	virtual ~AbstractSolver() noexcept = default;

	virtual void solve() noexcept = 0;
};

#include <vector>
#include <deque>

struct SolverB : AbstractSolver {
private:
	std::vector<std::vector<int>> edge;
	std::vector<std::vector<int>> edge_reversed;
	std::vector<int> win;
	std::vector<bool> vis;

public:
	constexpr SolverB() noexcept = delete;
	SolverB(int n) :
		edge(n, std::vector<int>()),
		edge_reversed(n, std::vector<int>()),
		win(n),
		vis(n) {}
	~SolverB() override = default;

	void add_edge(int u, int v) noexcept {
		edge[u].push_back(v);
		edge_reversed[v].push_back(u);
	}

	void solve() noexcept override {
		std::deque<int> q;
		for (int i = 0; i < edge.size(); i++) {
			if (!edge[i].size()) {
				q.push_back(i);
			}
		}
		while (!q.empty()) {
			int v = q.front();
			q.pop_front();
			vis[v] = true;
			for (int u : edge[v]) {
				if (!win[u]) {
					win[v] = 1;
					break;
				}
			}
			for (int u : edge_reversed[v]) {
				if (vis[u]) {
					continue;
				}
				bool undefined = false;
				bool is_zero = false;
				for (int w : edge[u]) {
					if (!vis[w]) {
						undefined = true;
					} else if (!win[w]) {
						is_zero = true;
					}
				}
				if (!undefined || is_zero) {
					q.push_back(u);
				}
			}
		}
		for (int i = 0; i < edge.size(); i++) {
			if (!vis[i]) {
				std::cout << "DRAW" << std::endl;
			} else if (win[i]) {
				std::cout << "FIRST" << std::endl;
			} else {
				std::cout << "SECOND" << std::endl;
			}
		}
		std::cout << std::endl;
	}
};

BEGIN
	int t;
	std::cin >> t;
	for (int i = 0; i < t; i++) {
		int n, m;
		std::cin >> n >> m;
		std::unique_ptr<SolverB> solver(new SolverB(n));
		for (int j = 0; j < m; j++) {
			int u, v;
			std::cin >> u >> v;
			solver->add_edge(u - 1, v - 1);
		}
		solver->solve();
	}
	return 0;
END
