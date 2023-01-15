#include <fstream>
#include <iostream>
#include <limits>
#include <memory>
#include <set>
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

struct SolverB : AbstractSolver
{
  private:
	Writer &writer;
	int n;
	std::vector< std::vector< std::pair< int, int > > > edges;

  public:
	constexpr SolverB() noexcept = delete;
	SolverB(Writer &writer, int const &n) noexcept : writer(writer), n(n), edges(n) {}
	~SolverB() noexcept override = default;

	void fill(int const &u, int const &v, int const &w) noexcept { edges[u].push_back({ v, w }); }

	void solve() noexcept override
	{
		std::vector< int > dist(n, std::numeric_limits< int >::max());
		std::set< std::pair< int, int > > queue;
		dist[0] = 0;
		queue.insert({ 0, 0 });
		while (!queue.empty())
		{
			std::pair< int, int > extracted = *(queue.begin());
			queue.erase(queue.begin());
			for (auto const &item : edges[extracted.first])
			{
				if (dist[item.first] > extracted.second + item.second)
				{
					queue.erase({ item.first, dist[item.first] });
					dist[item.first] = extracted.second + item.second;
					queue.insert({ item.first, dist[item.first] });
				}
			}
		}
		for (auto const &item : dist)
		{
			writer.write(item);
			writer.whitespace();
		}
		writer.newline();
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
	std::unique_ptr< SolverB > solver(new SolverB(writer, n));
	for (int i = 0; i < m; i++)
	{
		int u = read_int();
		int v = read_int();
		int w = read_int();
		solver->fill(u - 1, v - 1, w);
		solver->fill(v - 1, u - 1, w);
	}
	solver->solve();
	return 0;
}
