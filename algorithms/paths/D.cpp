#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <utility>
#include <vector>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct Reader
{
  private:
	std::fstream input;

  public:
	Reader() noexcept {}
	Reader(char* i) noexcept : input(i, std::ios_base::in) {}
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
	std::fstream output;

  public:
	Writer() noexcept {}
	Writer(char* o) noexcept : output(o, std::ios_base::out) {}
	~Writer() noexcept = default;

	void newline() noexcept
	{
		if (output.is_open())
			output << std::endl;
		else
			std::cout << std::endl;
	}

	void whitespace() noexcept
	{
		if (output.is_open())
			output << " ";
		else
			std::cout << " ";
	}

	template< typename T >
	void write(T const & out) noexcept
	{
		if (output.is_open())
			output << out;
		else
			std::cout << out;
	}
};

struct AbstractSolver
{
	constexpr AbstractSolver() noexcept = default;
	virtual ~AbstractSolver() noexcept = default;

	virtual void solve() noexcept = 0;
};

struct SolverD : AbstractSolver
{
  private:
	Writer& writer;
	int n, k, s;
	std::map< std::pair< int, int >, int > edge;

  public:
	constexpr SolverD() noexcept = delete;
	SolverD(Writer& writer, int const & n, int const & k, int const & s) noexcept : writer(writer), n(n), k(k), s(s) {}
	~SolverD() noexcept override = default;

	void fill(int const & u, int const & v, int const & w) noexcept
	{
		long long weight = -1;
		std::pair< int, int > e = { u, v };
		auto f = edge.find(e);
		if (f != edge.end())
		{
			weight = std::min(w, (*f).second);
		}
		else
		{
			weight = w;
		}
		edge[std::move(e)] = weight;
	}

	void solve() noexcept override
	{
		constexpr int MAX = std::numeric_limits< int >::max();
		std::vector< long long > d((k + 1) * n, MAX);
		d[s] = 0;
		for (int c = 0; c < k; c++)
		{
			for (auto const & item : edge)
			{
				int u = item.first.first;
				int v = item.first.second;
				int w = edge[{ u, v }];
				if (d[c * n + u] != MAX)
				{
					d[(c + 1) * n + v] = std::min(d[(c + 1) * n + v], d[c * n + u] + w);
				}
			}
		}
		for (int i = 0; i < n; i++)
		{
			writer.write(d[k * n + i] == MAX ? -1 : d[k * n + i]);
			writer.newline();
		}
	}
};

int main()
{
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(0);
	std::cout.tie(0);
	Reader reader;
	Writer writer;
	auto read_int = [&]()
	{
		return reader.read< int >();
	};
	int n = read_int();
	int m = read_int();
	int k = read_int();
	int s = read_int();
	std::unique_ptr< SolverD > solver(new SolverD(writer, n, k, s - 1));
	for (int i = 0; i < m; i++)
	{
		int a = read_int();
		int b = read_int();
		int w = read_int();
		solver->fill(a - 1, b - 1, w);
	}
	solver->solve();
	return 0;
}
