#include <stack>
#include <vector>
#include <string>
#include <iostream>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main(void) {
	int n, c = 1;
	std::stack<int> st;
	std::vector<std::string> ans;
	std::cin >> n;
	for (int i = 0; i < n; i++) {
		int a;
		std::cin >> a;
		st.push(a);
		ans.push_back("1 1");
		while (!st.empty() && st.top() == c) {
			st.pop();
			c++;
			ans.push_back("2 1");
		}
	}
	if (!st.empty()) {
		std::cout << "0" << std::endl;
	} else {
		for (const auto &x : ans) {
			std::cout << x << std::endl;
		}
	}
}
