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

struct SolverF : AbstractSolver {
private:
	std::vector<std::vector<int>> edge;
	std::vector<int> win;
	std::vector<bool> vis;
	std::vector<bool> was;

	void build_graph(int n) noexcept {
		std::deque<int> q;
		q.push_back(n);
		while (!q.empty()) {
			int v = q.front();
			q.pop_front();
			if (v % 3 == 0) {
				if (v - 1 >= 0 && !was[v - 1]) {
					edge[v].push_back(v - 1);
					q.push_back(v - 1);
					was[v - 1] = true;
				}
				if (v - 2 >= 0 && !was[v - 2]) {
					edge[v].push_back(v - 2);
					q.push_back(v - 2);
					was[v - 2] = true;
				}
			} else if (v % 3 == 1) {
				if (v - 1 >= 0 && !was[v - 1]) {
					edge[v].push_back(v - 1);
					q.push_back(v - 1);
					was[v - 1] = true;
				}
				if (v - 3 >= 0 && !was[v - 3]) {
					edge[v].push_back(v - 3);
					q.push_back(v - 3);
					was[v - 3] = true;
				}
			} else {
				if (v - 1 >= 0 && !was[v - 1]) {
					edge[v].push_back(v - 1);
					q.push_back(v - 1);
					was[v - 1] = true;
				}
				if (v - 2 >= 0 && !was[v - 2]) {
					edge[v].push_back(v - 2);
					q.push_back(v - 2);
					was[v - 2] = true;
				}
				if (v - 3 >= 0 && !was[v - 3]) {
					edge[v].push_back(v - 3);
					q.push_back(v - 3);
					was[v - 3] = true;
				}
			}
		}
	}

public:
	constexpr SolverF() noexcept = delete;
	SolverF(int n) :
		edge(n + 1, std::vector<int>()),
		win(n + 1),
		vis(n + 1),
		was(n + 1) {
			build_graph(n);
		}
	~SolverF() override = default;

	void solve() noexcept override {
		std::deque<int> q;
		q.push_back(0);
		while (!q.empty()) {
			int v = q.front();
			q.pop_front();
			vis[v] = true;
			if (v % 3 == 0) {
				if (v - 1 >= 0 && !win[v - 1]) {
					win[v] = 1;
				}
				if (v - 2 >= 0 && !win[v - 2]) {
					win[v] = 1;
				}
			} else if (v % 3 == 1) {
				if (v - 1 >= 0 && !win[v - 1]) {
					win[v] = 1;
				}
				if (v - 3 >= 0 && !win[v - 3]) {
					win[v] = 1;
				}
			} else {
				if (v - 1 >= 0 && !win[v - 1]) {
					win[v] = 1;
				}
				if (v - 2 >= 0 && !win[v - 2]) {
					win[v] = 1;
				}
				if (v - 3 >= 0 && !win[v - 3]) {
					win[v] = 1;
				}
			}
			if (v + 1 < edge.size() && !vis[v + 1]) {
				q.push_back(v + 1);
			}
			if (v + 2 < edge.size() && !vis[v + 2]) {
				q.push_back(v + 2);
			}
			if (v + 3 < edge.size() && !vis[v + 3]) {
				q.push_back(v + 3);
			}
		}
		if (win[edge.size() - 1]) {
			std::cout << "1" << std::endl;
		} else {
			std::cout << "2" << std::endl;
		}
	}
};

BEGIN
	int n;
	std::cin >> n;
	std::unique_ptr<SolverF> solver(new SolverF(n));
	solver->solve();
	return 0;
END
