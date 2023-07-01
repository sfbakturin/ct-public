#include <iostream>
#include <string>
#include <vector>
#include <cstdint>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main(int, char**) {
	constexpr int p = 1440889;
	std::string s;
	int m;
	std::cin >> s >> m;
	std::vector<std::uint64_t> pw(s.size() + 1, 0);
	std::vector<std::uint64_t> hash(s.size() + 1, 0);
	pw[0] = 1;
	for (std::size_t i = 0; i < pw.size() - 1; i++) {
		pw[i + 1] = pw[i] * p;
		hash[i + 1] = hash[i] * p + s[i];
	}
	for (int i = 0; i < m; i++) {
		int a, b, c, d;
		std::cin >> a >> b >> c >> d;
		std::uint64_t l1 = hash[a - 1] * pw[b - a + 1];
		std::uint64_t r1 = hash[b];
		std::uint64_t l2 = hash[c - 1] * pw[d - c + 1];
		std::uint64_t r2 = hash[d];
		if (r1 - l1 != r2 - l2) {
			std::cout << "No\n";
		} else {
			std::cout << "Yes\n";
		}
	}
}