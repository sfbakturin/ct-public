#include <stack>
#include <string>
#include <iostream>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main() {
	std::string in;
	std::cin >> in;
	std::stack<char> st;
	bool flag = true;
	for (char const c : in) {
		if (!flag) {
			break;
		}
		switch (c) {
			case '(':
			case '[':
			case '{':
				st.push(c);
				break;
			case ')': {
				if (st.empty() || st.top() != '(') {
					flag = false;
					break;
				}
				st.pop();
				break;
			}
			case ']': {
				if (st.empty() || st.top() != '[') {
					flag = false;
					break;
				}
				st.pop();
				break;
			}
			case '}': {
				if (st.empty() || st.top() != '{') {
					flag = false;
					break;
				}
				st.pop();
				break;
			}
		}
	}
	std::cout << (flag && st.empty() ? "YES" : "NO");
}
