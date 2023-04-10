#include <fstream>
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

	Reader(char *i) noexcept : input(i) {}

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

	Writer(char *o) noexcept : output(o) {}

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
  private:
	using seq_t = std::vector< int >;

	constexpr static int modulus = 998244353;
	Writer &writer;
	seq_t p, q;
	int n, m;

	static int getFrom(seq_t &v, int i) noexcept { return i < v.size() ? v[i] : 0; }

	static int getModulus(long long i) noexcept
	{
		int res = i % modulus;
		return res >= 0 ? res : (res + modulus);
	}

	static int getDeg(seq_t &v) noexcept
	{
		int it = v.size() - 1;
		while (it && !v[it])
		{
			it--;
		}
		return it;
	}

	seq_t seqSum() noexcept
	{
		seq_t result;
		for (int i = 0; i <= std::max(n, m); i++)
		{
			int s = getModulus(getFrom(p, i)) + getModulus(getFrom(q, i));
			result.push_back(getModulus(s));
		}
		return result;
	}

	seq_t seqProd() noexcept
	{
		seq_t result(p.size() + q.size() - 1);
		for (int i = 0; i < result.size(); i++)
		{
			for (int j = 0; j + i < result.size(); j++)
			{
				long long p1 = getModulus(getFrom(p, i));
				long long p2 = getModulus(getFrom(q, j));
				long long pr = p1 * p2;
				result[i + j] = getModulus(getModulus(result[i + j]) + getModulus(pr));
			}
		}
		return result;
	}

	seq_t seqFactor() noexcept
	{
		seq_t result(1000);
		for (int i = 0; i < 1000; i++)
		{
			int t = getModulus(getFrom(p, i)), s = 0;
			for (int j = 0; j <= i - 1; j++)
			{
				long long tt = getModulus(result[j]);
				long long tq = getModulus(getFrom(q, i - j));
				long long pr = tt * tq;
				s = getModulus(getModulus(s) + getModulus(pr));
			}
			result[i] = getModulus(getModulus(t) - getModulus(s));
		}
		return result;
	}

  public:
	constexpr Solver() noexcept = delete;

	explicit Solver(Writer &writer, int n, int m) noexcept : writer(writer), n(n), m(m) {}

	~Solver() noexcept = default;

	void fillSeqP(int e) noexcept { p.push_back(e); }

	void fillSeqQ(int e) noexcept { q.push_back(e); }

	void solve() noexcept
	{
		seq_t sum = seqSum();
		seq_t prd = seqProd();
		seq_t fct = seqFactor();
		int degSum = getDeg(sum);
		int degPrd = getDeg(prd);
		writer.write(degSum);
		writer.write("\n");
		for (int i = 0; i <= degSum; i++)
		{
			writer.write(sum[i]);
			writer.write(" ");
		}
		writer.write("\n");
		writer.write(degPrd);
		writer.write("\n");
		for (int i = 0; i <= degPrd; i++)
		{
			writer.write(prd[i]);
			writer.write(" ");
		}
		writer.write("\n");
		for (int &it : fct)
		{
			writer.write(getModulus(it));
			writer.write(" ");
		}
	}
};

int main()
{
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);
	Reader reader{};
	Writer writer{};
	int n = reader.read< int >(), m = reader.read< int >();
	Solver solver(writer, n, m);
	for (int i = 0; i <= n; i++)
	{
		solver.fillSeqP(reader.read< int >());
	}
	for (int i = 0; i <= m; i++)
	{
		solver.fillSeqQ(reader.read< int >());
	}
	solver.solve();
	return 0;
}
