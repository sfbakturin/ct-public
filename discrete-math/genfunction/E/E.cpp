#include <cmath>
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

constexpr static int binom[12][12] = {
	{ 1 },
	{ 1, 1 },
	{ 1, 2, 1 },
	{ 1, 3, 3, 1 },
	{ 1, 4, 6, 4, 1 },
	{ 1, 5, 10, 10, 5, 1 },
	{ 1, 6, 15, 20, 15, 6, 1 },
	{ 1, 7, 21, 35, 35, 21, 7, 1 },
	{ 1, 8, 28, 56, 70, 56, 28, 8, 1 },
	{ 1, 9, 36, 84, 126, 126, 84, 36, 9, 1 },
	{ 1, 10, 45, 120, 210, 252, 210, 120, 45, 10, 1 },
	{ 1, 11, 55, 165, 330, 462, 462, 330, 165, 55, 11, 1 },
};

struct Solver
{
  private:
	using elem_t = std::int64_t;
	using seq_t = std::vector< elem_t >;

	Writer &writer;
	elem_t r, d;
	seq_t p;

	seq_t getSeqQ() const noexcept
	{
		seq_t result;
		elem_t a = 1, b = 1, f = 1;
		for (int k = 0; k <= d + 1; k++)
		{
			result.push_back(binom[d + 1][k] * a * b * f);
			b *= r;
			f *= -1;
		}
		return result;
	}

	seq_t getSeqA() const noexcept
	{
		seq_t result;
		elem_t rr = 1;
		elem_t x = 0;
		for (int i = 0; i < d + 1; i++)
		{
			elem_t sum = 0;
			for (int j = 1; j <= d; j++)
			{
				sum += p[j] * static_cast< elem_t >(std::pow(x, j));
			}
			result.push_back((p[0] + sum) * rr);
			x++;
			rr *= r;
		}
		return result;
	}

	static elem_t getFrom(const seq_t &v, int i) noexcept { return i < v.size() ? v[i] : 0; }

	static std::size_t getDeg(seq_t &v, int m) noexcept
	{
		std::size_t it = (m < v.size() ? m : v.size() - 1);
		while (it && !v[it])
		{
			it--;
		}
		return it;
	}

	static seq_t getSeqP(const seq_t &p, const seq_t &q) noexcept
	{
		seq_t result(p.size() + q.size() - 1);
		for (int i = 0; i < result.size(); i++)
		{
			for (int j = 0; j + i < result.size(); j++)
			{
				elem_t p1 = getFrom(p, i);
				elem_t p2 = getFrom(q, j);
				elem_t pr = p1 * p2;
				result[i + j] = result[i + j] + pr;
			}
		}
		return result;
	}

  public:
	constexpr Solver() noexcept = delete;

	explicit Solver(Writer &writer, elem_t r, elem_t d) noexcept : writer(writer), r(r), d(d) {}

	~Solver() noexcept = default;

	void fill(int e) noexcept { p.push_back(e); }

	void solve() noexcept
	{
		seq_t Qt = getSeqQ();
		seq_t At = getSeqA();
		seq_t Pt = getSeqP(At, Qt);
		std::size_t deg_p = getDeg(Pt, d);
		writer.write(deg_p);
		writer.write("\n");
		for (std::size_t i = 0; i <= deg_p; i++)
		{
			writer.write(Pt[i]);
			writer.write(" ");
		}
		writer.write("\n");
		writer.write(d + 1);
		writer.write("\n");
		for (int i = 0; i <= d + 1; i++)
		{
			writer.write(Qt[i]);
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
	int r = reader.read< std::int64_t >(), d = reader.read< std::int64_t >();
	Solver solver(writer, r, d);
	for (int i = 0; i <= d; i++)
	{
		solver.fill(reader.read< std::int64_t >());
	}
	solver.solve();
	return 0;
}
