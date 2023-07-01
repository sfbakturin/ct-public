#include <iostream>
#include <string>
#include <vector>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main(void) {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(0);
	std::cout.tie(0);
	std::string s;
	std::cin >> s;
	std::vector<int> p(s.size(), 0);
	printf("%i ", p[0]);
	for (int i = 1; i < s.size(); i++) {
		int k = p[i - 1];
		while (k > 0 && s[k] != s[i]) {
			k = p[k - 1];
		}
		if (s[k] == s[i]) {
			k++;
		}
		p[i] = k;
		printf("%i ", p[i]);
	}
	return 0;
}
