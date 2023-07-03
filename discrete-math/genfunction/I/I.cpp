#include <iostream>
#include <vector>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

constexpr static inline int modulus = 104857601;

static std::int64_t get_modulus(std::int64_t e) noexcept
{
	std::int64_t r = e % modulus;
	return (r >= 0 ? r : r + modulus);
}

static std::int64_t get_or_default(std::vector< std::int64_t > const &v, std::size_t i) noexcept
{
	return (i < v.size() ? v[i] : 0);
}

static std::vector< std::int64_t > gen_q(std::vector< std::int64_t > const &v)
{
	std::vector< std::int64_t > res = { 1 };
	for (std::int64_t const &item : v)
	{
		res.push_back((-1) * item);
	}
	return res;
}

static std::vector< std::int64_t > prod(std::vector< std::int64_t > const &p, std::vector< std::int64_t > const &q)
{
	std::vector< std::int64_t > res(p.size() + q.size());
	for (std::size_t i = 0; i < p.size(); i++)
	{
		for (std::size_t j = 0; j < q.size() && i + j < res.size(); j++)
		{
			res[i + j] = get_modulus(res[i + j] + get_modulus(get_or_default(p, i) * get_or_default(q, j)));
		}
	}
	return res;
}

static std::vector< std::int64_t > prod_negate(std::vector< std::int64_t > const &p, std::vector< std::int64_t > const &q)
{
	std::vector< std::int64_t > res(p.size() + q.size());
	for (std::size_t i = 0; i < p.size(); i++)
	{
		for (std::size_t j = 0; j < q.size() && i + j < res.size(); j++)
		{
			res[i + j] = get_modulus(
				res[i + j] +
				get_modulus(get_modulus(get_or_default(p, i) * get_or_default(q, j)) * (j == 0 ? 1 : (j % 2 ? -1 : 1))));
		}
	}
	return res;
}

static void mod(std::vector< std::int64_t > &v, std::size_t i)
{
	while (!v.empty() && v.size() > i)
	{
		v.pop_back();
	}
}

static std::vector< std::int64_t > mod(std::vector< std::int64_t > &&v, std::size_t i)
{
	std::vector< std::int64_t > mod = std::move(v);
	while (!mod.empty() && mod.size() > i)
	{
		mod.pop_back();
	}
	return mod;
}

static void filter(std::vector< std::int64_t > &v, std::size_t m) noexcept
{
	std::size_t index = 0;
	for (std::size_t i = m % 2; i < v.size(); i += 2)
	{
		v[index++] = v[i];
	}
}

static std::vector< std::int64_t > generate(std::vector< std::int64_t > const &p, std::vector< std::int64_t > const &q, int k)
{
	std::vector< std::int64_t > an(k + 1);
	for (int i = 0; i <= k; i++)
	{
		std::int64_t t = get_or_default(p, i), s = 0;
		for (int j = 0; j <= i - 1; j++)
		{
			std::int64_t pr = get_modulus(get_or_default(an, j) * get_or_default(q, i - j));
			s = get_modulus(s + pr);
		}
		an[i] = get_modulus(t - s) / q[0];
	}
	return an;
}

int main()
{
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);
	int k;
	std::int64_t n;
	std::cin >> k >> n;
	std::vector< std::int64_t > a(k), c(k);
	for (int i = 0; i < k; i++)
	{
		std::cin >> a[i];
	}
	for (int i = 0; i < k; i++)
	{
		std::cin >> c[i];
	}
	std::vector< std::int64_t > qt = gen_q(c);
	std::vector< std::int64_t > pt = mod(prod(a, qt), k);
	std::int64_t nit = n - 1;
	while (nit > k)
	{
		pt = prod_negate(pt, qt);
		qt = prod_negate(qt, qt);
		filter(pt, nit % 2);
		filter(qt, 0);
		mod(pt, k);
		mod(qt, k + 1);
		nit /= 2;
	}
	std::vector< std::int64_t > answer = generate(pt, qt, k);
	std::cout << answer[nit];
}
