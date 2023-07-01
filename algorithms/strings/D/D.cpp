#include <cstdint>
#include <iostream>
#include <string>
#include <vector>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main()
{
	constexpr char delim = '$';
	std::string p, t;
	std::cin >> p >> t;
	std::string s = p + delim + t;
	std::vector< std::uint64_t > prefix(s.length());
	for (std::size_t i = 1; i < prefix.size(); i++)
	{
		std::uint64_t k = prefix[i - 1];
		while (k > 0 && s[k] != s[i])
		{
			k = prefix[k - 1];
		}
		if (s[k] == s[i])
		{
			k++;
		}
		prefix[i] = k;
	}
	std::vector< std::uint64_t > answer;
	for (std::size_t i = 0; i < t.length(); i++)
	{
		if (prefix[p.length() + i + 1] == p.length())
		{
			answer.push_back(2 + i - p.length());
		}
	}
	std::cout << answer.size() << "\n";
	for (std::uint64_t i : answer)
	{
		std::cout << i << " ";
	}
}
