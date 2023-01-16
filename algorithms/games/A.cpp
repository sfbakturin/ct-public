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

struct SolverA : AbstractSolver {
private:
	std::vector<std::vector<int>> edge;
	std::vector<std::vector<int>> edge_reversed;
	std::vector<int> win;
	std::vector<bool> vis;
	int s;

public:
	constexpr SolverA() noexcept = delete;
	SolverA(int n, int s) :
		edge(n, std::vector<int>()),
		edge_reversed(n, std::vector<int>()),
		win(n),
		vis(n),
		s(s) {}
	~SolverA() override = default;

	void add_edge(int u, int v) noexcept {
		edge[u].push_back(v);
		edge_reversed[v].push_back(u);
	}

	void solve() noexcept override {
		for (int i = 0; i < edge.size(); i++) {
			if (!edge[i].size()) {
				std::deque<int> q;
				for (int v : edge_reversed[i]) {
					q.push_back(v);
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
						if (!vis[u]) {
							q.push_back(u);
						}
					}
				}
			}
		}
		if (win[s]) {
			std::cout << "First player wins" << std::endl;
		} else {
			std::cout << "Second player wins" << std::endl;
		}
	}
};

BEGIN
	int n, m, s;
	std::cin >> n >> m >> s;
	std::unique_ptr<SolverA> solver(new SolverA(n, s - 1));
	for (int i = 0; i < m; i++) {
		int u, v;
		std::cin >> u >> v;
		solver->add_edge(u - 1, v - 1);
	}
	solver->solve();
	return 0;
END
