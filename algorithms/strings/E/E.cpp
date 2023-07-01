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
	std::string s;
	std::cin >> s;
	std::vector< int > z(s.size(), 0);
	int l = 0, r = 0;
	for (int i = 1; i < s.size(); i++)
	{
		z[i] = std::max(0, std::min(r - i, z[i - l]));
		while (i + z[i] < s.size() && s[i + z[i]] == s[z[i]])
		{
			z[i]++;
		}
		if (i + z[i] > r)
		{
			l = i;
			r = i + z[i];
		}
	}
	for (int i = 1; i < s.size(); i++)
	{
		if ((i + z[i] == s.size()) && (s.size() % i == 0))
		{
			std::cout << i << "\n";
			return 0;
		}
	}
	std::cout << s.size() << "\n";
	return 0;
}
