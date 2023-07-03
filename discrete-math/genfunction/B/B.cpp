#include <fstream>
#include <functional>
#include <iostream>
#include <vector>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct Reader
{
  private:
	std::ifstream input;

  public:
	Reader() noexcept {}

	explicit Reader(char *i) noexcept : input(i) {}

	~Reader() noexcept = default;

	template< typename T >
	T read() noexcept
	{
		T res;
		if (input.is_open())
			input >> res;
		else
			std::cin >> res;
		return res;
	}
};

struct Writer
{
  private:
	std::ofstream output;

  public:
	Writer() noexcept {}

	explicit Writer(char *o) noexcept : output(o) {}

	~Writer() noexcept = default;

	template< typename T >
	void write(T const &out) noexcept
	{
		if (output.is_open())
			output << out;
		else
			std::cout << out;
	}
};

struct Solver
{
  public:
	constexpr static int MODULUS = 998244353;

	template< typename T >
	static T getModulus(T i) noexcept
	{
		T res = i % MODULUS;
		return res >= 0 ? res : (res + MODULUS);
	}

  private:
	template< typename T >
	struct GeneratingFunction
	{
	  private:
		constexpr static inline T NEUTRAL = 0;

		std::vector< T > data;

		void swap(GeneratingFunction &other) noexcept { std::swap(data, other.data); }

	  public:
		GeneratingFunction() noexcept = default;
		GeneratingFunction(GeneratingFunction &&other) noexcept : data(std::move(other.data)) {}
		GeneratingFunction(GeneratingFunction const &other) noexcept : data(other.data) {}

		explicit GeneratingFunction(std::size_t preserve) noexcept : data(preserve, NEUTRAL) {}

		GeneratingFunction &operator=(GeneratingFunction &&other) noexcept
		{
			GeneratingFunction(std::move(other)).swap(*this);
			return *this;
		}

		GeneratingFunction &operator=(GeneratingFunction const &other) noexcept
		{
			GeneratingFunction(other).swap(*this);
			return *this;
		}

		void add(T e) noexcept { data.push_back(std::move(e)); }

		std::size_t size() const noexcept { return data.size(); }

		T getAt(std::size_t i) const noexcept { return i < size() ? data[i] : NEUTRAL; }

		void setAt(T e, std::size_t i) noexcept { data[i] = e; }

		std::size_t realSize() const noexcept
		{
			std::size_t it = size() - 1;
			while (it && !data[it])
			{
				it--;
			}
			return it;
		}

		friend GeneratingFunction operator+(GeneratingFunction const &lhs, GeneratingFunction const &rhs) noexcept
		{
			GeneratingFunction result;
			for (std::size_t i = 0; i <= std::max(lhs.size(), rhs.size()); i++)
			{
				T s = getModulus(lhs.getAt(i)) + getModulus(rhs.getAt(i));
				result.add(std::move(getModulus(s)));
			}
			return result;
		}

		friend GeneratingFunction operator*(GeneratingFunction const &lhs, GeneratingFunction const &rhs) noexcept
		{
			GeneratingFunction result(lhs.size() + rhs.size() - 1);
			for (std::size_t i = 0; i < lhs.size(); i++)
			{
				for (std::size_t j = 0; j < rhs.size() && i + j < result.size(); j++)
				{
					T p1 = lhs.getAt(i);
					T p2 = rhs.getAt(j);
					T p3 = getModulus(p1 * p2);
					T p4 = result.getAt(i + j);
					result.setAt(getModulus(p4 + p3), i + j);
				}
			}
			return result;
		}

		friend GeneratingFunction operator*(GeneratingFunction const &lhs, T rhs) noexcept
		{
			GeneratingFunction result = lhs;
			for (std::size_t i = 0; i < result.size(); i++)
			{
				result.setAt(getModulus(lhs.getAt(i) * rhs), i);
			}
			return result;
		}

		friend GeneratingFunction operator*=(GeneratingFunction const &lhs, GeneratingFunction const &rhs) noexcept
		{
			return lhs * rhs;
		}

		GeneratingFunction &operator+=(GeneratingFunction const &other) noexcept
		{
			GeneratingFunction(*this + other).swap(*this);
			return *this;
		}
	};

	using elem_t = std::int64_t;
	using gf_t = GeneratingFunction< elem_t >;

	Writer &writer;
	gf_t start;

	static elem_t binaryFuckingPowerfulPower(elem_t a, elem_t n)
	{
		elem_t res = 1;
		while (n)
		{
			if (n & 1)
			{
				res *= a;
				res %= MODULUS;
			}
			a *= a;
			a %= MODULUS;
			n >>= 1;
		}
		return res;
	}

	std::vector< gf_t > generatingFunctionTable(int m) noexcept
	{
		gf_t start0;
		start0.add(1);
		std::vector< gf_t > table = { start0, start };
		for (int i = 1; i <= m; i++)
		{
			table.push_back(table[table.size() - 1] * start);
		}
		return table;
	}

	static std::vector< elem_t > factorialTable(int m) noexcept
	{
		std::vector< elem_t > table = { 1, 1 };
		for (int i = 2; i <= 2 * m; i++)
		{
			elem_t p = (table[(table.size() - 1)] * i) % MODULUS;
			table.push_back(p);
		}
		return table;
	}

	static gf_t getSqrt(int m, std::vector< gf_t > const &gfTable, std::vector< elem_t > const &factTable) noexcept
	{
		gf_t result;
		elem_t f = -1;
		for (int n = 0; n < m; n++)
		{
			elem_t num = factTable[2 * n];
			elem_t pwr = binaryFuckingPowerfulPower(2, 2 * n);
			elem_t expr = (2 * n - 1);
			elem_t pwrExpr = getModulus(pwr * expr);
			elem_t factFact = getModulus(factTable[n] * factTable[n]);
			elem_t fct = getModulus(f * getModulus(pwrExpr * factFact));
			elem_t rev = getModulus(binaryFuckingPowerfulPower(fct, MODULUS - 2));
			elem_t cof = getModulus(num * rev);
//			f = getModulus((MODULUS - 1) * f);
			f *= -1;
			result += gfTable[n] * cof;
		}
		return result;
	}

	static gf_t getLn(int m, std::vector< gf_t > const &gfTable) noexcept
	{
		gf_t result;
		result.add(0);
		elem_t f = 1;
		for (int n = 1; n < m; n++)
		{
			elem_t num = 1;
			elem_t fct = getModulus(f * n);
			elem_t rev = getModulus(binaryFuckingPowerfulPower(fct, MODULUS - 2));
			elem_t cof = getModulus(num * rev);
//			f = getModulus((MODULUS - 1) * f);
			f *= -1;
			result += gfTable[n] * cof;
		}
		return result;
	}

	static gf_t getExp(int m, std::vector< gf_t > const &gfTable, std::vector< elem_t > const &factTable) noexcept
	{
		gf_t result;
		for (int n = 0; n < m; n++)
		{
			elem_t num = 1;
			elem_t fct = factTable[n];
			elem_t rev = getModulus(binaryFuckingPowerfulPower(fct, MODULUS - 2));
			elem_t cof = getModulus(num * rev);
			result += gfTable[n] * cof;
		}
		return result;
	}

  public:
	constexpr Solver() noexcept = delete;

	explicit Solver(Writer &writer) noexcept : writer(writer) {}

	~Solver() noexcept = default;

	void fill(std::int64_t e) noexcept { start.add(e); }

	void solve(int m) noexcept
	{
		std::vector< gf_t > gfTable = generatingFunctionTable(m);
		std::vector< elem_t > factTable = factorialTable(m);
		gf_t sqrtSeq = getSqrt(m, gfTable, factTable);
		gf_t lnSeq = getLn(m, gfTable);
		gf_t expSeq = getExp(m, gfTable, factTable);
		for (std::size_t i = 0; i < std::min(sqrtSeq.realSize() + 1ULL, m + 0ULL); i++)
		{
			writer.write(sqrtSeq.getAt(i));
			writer.write(" ");
		}
		writer.write("\n");
		for (std::size_t i = 0; i < std::min(sqrtSeq.realSize() + 1ULL, m + 0ULL); i++)
		{
			writer.write(expSeq.getAt(i));
			writer.write(" ");
		}
		writer.write("\n");
		for (std::size_t i = 0; i < std::min(sqrtSeq.realSize() + 1ULL, m + 0ULL); i++)
		{
			writer.write(lnSeq.getAt(i));
			writer.write(" ");
		}
	}
};

int main()
{
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);
	Reader reader{ /*"test.txt"*/ };
	Writer writer{ /*"test.out"*/ };
	int n = reader.read< int >(), m = reader.read< int >();
	Solver solver(writer);
	for (int i = 0; i <= n; i++)
	{
		solver.fill(reader.read< std::int64_t >());
	}
	solver.solve(m);
	return 0;
}
