
/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define BEGIN int main() {
#define END }
#define OPTIMIZE std::ios_base::sync_with_stdio(false); std::cin.tie(0); std::cout.tie(0)

#include <algorithm>
#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <list>
#include <set>

void dfs_ts(std::vector<std::vector<int>> const& edge, int const& v, std::vector<int> &ts, std::vector<bool> &visited) {
	visited[v] = true;
	for (int const& u : edge[v]) if (!visited[u]) dfs_ts(edge, u, ts, visited);
	ts.push_back(v);
}
 
void dfs_scc(std::vector<std::vector<int>> const& edge_reverse, int const& v, int const& c, std::vector<int> &comp) {
	comp[v] = c;
	for (int const& u : edge_reverse[v]) if (!comp[u]) dfs_scc(edge_reverse, u, c, comp);
}

BEGIN
	OPTIMIZE;
	int n, m;
	std::cin >> n >> m;
	std::vector<std::string> id(n);
	std::map<std::string, int> names;
	std::vector<std::vector<int>> g(2 * n, std::vector<int>());
	std::vector<std::vector<int>> g_reverse(2 * n, std::vector<int>());
	std::vector<int> comp(2 * n);
	std::vector<bool> vis(2 * n);
	std::vector<int> ts;
	std::set<int> answer;
	int c = 1;
	for (int i = 0; i < n; i++) {
		std::string name;
		std::cin >> name;
		names[name] = i;
		id[i] = name;
	}
	for (int i = 0; i < m; i++) {
		char mark1, mark2;
		std::string name1, name2, temp;
		std::cin >> mark1 >> name1 >> temp >> mark2 >> name2;
		int index1 = names[name1];
		int index2 = names[name2];
		bool flag1 = mark1 == '+';
		bool flag2 = mark2 == '+';
		if (flag1 && flag2) {
			g[index1].push_back(index2);
			g_reverse[index2].push_back(index1);

			g[index2 + n].push_back(index1 + n);
			g_reverse[index1 + n].push_back(index2 + n);
		}
		if (flag1 && !flag2) {
			g[index1].push_back(index2 + n);
			g_reverse[index2 + n].push_back(index1);

			g[index2].push_back(index1 + n);
			g_reverse[index1 + n].push_back(index2);
		}
		if (!flag1 && flag2) {
			g[index1 + n].push_back(index2);
			g_reverse[index2].push_back(index1 + n);

			g[index2 + n].push_back(index1);
			g_reverse[index1].push_back(index2 + n);
		}
		if (!flag1 && !flag2) {
			g[index1 + n].push_back(index2 + n);
			g_reverse[index2 + n].push_back(index1 + n);

			g[index2].push_back(index1);
			g_reverse[index1].push_back(index2);
		}
	}
	for (int i = 0; i < 2 * n; i++) {
		if (!vis[i]) dfs_ts(g, i, ts, vis);
	}
	std::reverse(ts.begin(), ts.end());
	for (int const& v : ts) if (!comp[v]) dfs_scc(g_reverse, v, c++, comp);
	for (int i = 0; i < n; i++) {
		if (comp[i] == comp[i + n]) {
			std::cout << "-1" << std::endl;
			return 0;
		} else if (comp[i] > comp[i + n]) {
			answer.insert(i);
		}
	}
	std::cout << answer.size() << std::endl;
	for (int const& item : answer) std::cout << id[item] << std::endl;
END
