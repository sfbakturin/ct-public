#include <string_view>

#include <algorithm>
#include <iostream>
#include <map>
#include <vector>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

int main()
{
	constexpr int p = 26;
	std::string s, t;
	std::cin >> s >> t;
	std::vector< std::uint64_t > pw(std::max(s.size(), t.size()) + 1, 0);
	std::vector< std::uint64_t > hash_s(s.size() + 1, 0);
	std::vector< std::uint64_t > hash_t(t.size() + 1, 0);
	pw[0] = 1;
	for (std::size_t i = 0; i < pw.size() - 1; i++)
	{
		pw[i + 1] = pw[i] * p;
		if ((i + 1) < hash_s.size())
		{
			hash_s[i + 1] = hash_s[i] * p + s[i];
		}
		if ((i + 1) < hash_t.size())
		{
			hash_t[i + 1] = hash_t[i] * p + t[i];
		}
	}
	int l = -1, r = static_cast< int >(std::max(s.size(), t.size())) + 1;
	std::string_view sv_s = s, sv_t = t;
	std::size_t min_max = 0;
	std::vector< std::vector< std::string_view > > answer(std::min(s.size(), t.size()) + 1, std::vector< std::string_view >());
	while (l + 1 < r)
	{
		int m = (l + r) / 2;
		std::map< std::uint64_t, std::string_view > sub_strings;
		for (int i = m; i < std::max(s.size(), t.size()) + 1; i++)
		{
			std::uint64_t lh, rh;
			if (s.size() > t.size())
			{
				lh = hash_s[i - m] * pw[i - (i - m)];
				rh = hash_s[i];
				sub_strings.insert({ rh - lh, sv_s.substr(i - m, m) });
			}
			else
			{
				lh = hash_t[i - m] * pw[i - (i - m)];
				rh = hash_t[i];
				sub_strings.insert({ rh - lh, sv_t.substr(i - m, m) });
			}
		}
		bool inserted = false;
		for (int i = m; i < std::min(s.size(), t.size()) + 1; i++)
		{
			std::uint64_t lh, rh;
			if (s.size() > t.size())
			{
				lh = hash_t[i - m] * pw[i - (i - m)];
				rh = hash_t[i];
			}
			else
			{
				lh = hash_s[i - m] * pw[i - (i - m)];
				rh = hash_s[i];
			}
			auto found = sub_strings.find(rh - lh);
			if (found != sub_strings.end() &&
				(s.size() > t.size() ? sv_t.substr(i - m, m) == (*found).second : sv_s.substr(i - m, m) == (*found).second))
			{
				if (s.size() > t.size())
				{
					std::string_view temp = sv_t.substr(i - m, m);
					answer[temp.size()].push_back(temp);
					min_max = std::max(min_max, temp.size());
				}
				else
				{
					std::string_view temp = sv_s.substr(i - m, m);
					answer[temp.size()].push_back(temp);
					min_max = std::max(min_max, temp.size());
				}
				inserted = true;
			}
		}
		if (inserted)
		{
			l = m;
		}
		else
		{
			r = m;
		}
	}
	std::sort(answer[min_max].begin(), answer[min_max].end());
	std::cout << answer[min_max][0] << "\n";
	return 0;
}
