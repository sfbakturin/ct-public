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

struct Solver
{
  private:
	using elem_t = long long;
	using seq_t = std::vector< elem_t >;

	Writer &writer;
	int deg_ac;
	seq_t a, c;

	seq_t getSeqQt() noexcept
	{
		seq_t result = { 1 };
		for (int i = 0; i < deg_ac; i++)
		{
			result.push_back((-1) * getFrom(c, i));
		}
		return result;
	}

	static int getFrom(const seq_t &v, int i) noexcept { return i < v.size() ? v[i] : 0; }

	static int getDeg(seq_t &v) noexcept
	{
		int it = v.size() - 1;
		while (it && !v[it])
		{
			it--;
		}
		return it;
	}

	static seq_t seqProd(const seq_t &a, const seq_t &b) noexcept
	{
		seq_t result(a.size() + b.size() - 1);
		for (int i = 0; i < result.size(); i++)
		{
			for (int j = 0; j + i < result.size(); j++)
			{
				elem_t p1 = getFrom(a, i);
				elem_t p2 = getFrom(b, j);
				elem_t pr = p1 * p2;
				result[i + j] = result[i + j] + pr;
			}
		}
		return result;
	}

	static seq_t seqMod(seq_t &v, int deg) noexcept
	{
		for (int i = deg; i < v.size(); i++)
		{
			v[i] = 0;
		}
		return v;
	}

  public:
	constexpr Solver() noexcept = delete;

	explicit Solver(Writer &writer, int k) noexcept : writer(writer), deg_ac(k) {}

	~Solver() noexcept = default;

	void fillSeqA(int e) noexcept { a.push_back(e); }

	void fillSeqC(int e) noexcept { c.push_back(e); }

	void solve() noexcept
	{
		seq_t Qt = getSeqQt();
		seq_t At = a;
		seq_t ACt = seqProd(At, Qt);
		seq_t Pt = seqMod(ACt, deg_ac);
		int deg_p = getDeg(Pt);
		int deg_q = getDeg(Qt);
		writer.write(deg_p);
		writer.write("\n");
		for (int i = 0; i <= deg_p; i++)
		{
			writer.write(Pt[i]);
			writer.write(" ");
		}
		writer.write("\n");
		writer.write(deg_q);
		writer.write("\n");
		for (int i = 0; i <= deg_q; i++)
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
	int k = reader.read< int >();
	Solver solver(writer, k);
	for (int i = 0; i < k; i++)
	{
		solver.fillSeqA(reader.read< int >());
	}
	for (int i = 0; i < k; i++)
	{
		solver.fillSeqC(reader.read< int >());
	}
	solver.solve();
	return 0;
}
