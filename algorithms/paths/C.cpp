#include <deque>
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
	Reader(char *i) noexcept : input(i, std::ios_base::in) {}
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
	Writer(char *o) noexcept : output(o, std::ios_base::out) {}
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
	void write(T const &out) noexcept
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

struct SolverC : AbstractSolver
{
  private:
	Writer &writer;
	int n;
	std::map< std::pair< int, int >, int > edge;

	constexpr static int no_edge = 100000;

  public:
	constexpr SolverC() noexcept = delete;
	SolverC(Writer &writer, int const &n) noexcept : writer(writer), n(n) {}
	~SolverC() noexcept override = default;

	void fill(int const &u, int const &v, int const &w) noexcept
	{
		if (w != no_edge)
		{
			edge[{ u, v }] = w;
		}
	}

	void solve() noexcept override
	{
		int start = -1;
		std::vector< long long > d(n);
		std::vector< int > p(n);
		std::fill(d.begin(), d.end(), std::numeric_limits< int >::max());
		d[0] = 0;
		for (int k = 0; k < n - 1; k++)
		{
			for (auto const &item : edge)
			{
				int u = item.first.second;
				int v = item.first.first;
				int w = item.second;
				if (d[u] > d[v] + w)
				{
					d[u] = d[v] + w;
					p[u] = v;
				}
			}
		}
		for (auto const &item : edge)
		{
			int u = item.first.second;
			int v = item.first.first;
			int w = item.second;
			if (d[u] > d[v] + w)
			{
				d[u] = d[v] + w;
				p[u] = v;
				start = u;
			}
		}
		if (start < 0)
		{
			writer.write("NO");
			writer.newline();
		}
		else
		{
			writer.write("YES");
			writer.newline();
			int copy = start;
			for (int i = 0; i < n; i++)
			{
				copy = p[copy];
			}
			std::deque< int > cycle;
			for (int i = copy; true; i = p[i])
			{
				if (i == copy && cycle.size() > 0)
				{
					break;
				}
				cycle.push_back(i);
			}
			writer.write(cycle.size());
			writer.newline();
			while (!cycle.empty())
			{
				writer.write(cycle.back() + 1);
				writer.whitespace();
				cycle.pop_back();
			}
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
	std::unique_ptr< SolverC > solver(new SolverC(writer, n));
	for (int i = 0; i < n; i++)
	{
		for (int j = 0; j < n; j++)
		{
			solver->fill(i, j, reader.read< int >());
		}
	}
	solver->solve();
	return 0;
}
