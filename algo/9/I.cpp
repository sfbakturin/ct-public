#include <fstream>
#include <iostream>
#include <memory>
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

struct SolverI : AbstractSolver
{
  private:
	Writer& writer;
	int n;
	std::vector< int > dec, win;
	std::vector< bool > painted;
	std::vector< std::vector< int > > edge;

	void paint(int const & v)
	{
		std::fill(painted.begin(), painted.end(), false);
		for (auto const & item : edge[v])
		{
			if (win[item] != -1)
			{
				painted[win[item]] = true;
			}
		}
		for (int i = 0; i < n; i++)
		{
			if (!painted[i])
			{
				win[v] = i;
				return;
			}
		}
	}

	void grandy(int const & v) noexcept
	{
		for (auto const & item : edge[v])
		{
			if (win[v] < 0)
			{
				grandy(item);
			}
		}
		paint(v);
	}

  public:
	constexpr SolverI() noexcept = delete;
	SolverI(Writer& writer, int const & n) noexcept :
		writer(writer), n(n), painted(n), dec(n), win(n, -1), edge(n, std::vector< int >())
	{
	}
	~SolverI() noexcept override = default;

	void fill(int const & u, int const & v) noexcept
	{
		edge[u].push_back(v);
		dec[v]++;
	}

	void solve() noexcept override
	{
		for (int i = 0; i < n; i++)
		{
			if (dec[i] == 0 && win[i] < 0)
			{
				grandy(i);
			}
		}
		for (auto const & item : win)
		{
			writer.write(item);
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
	std::unique_ptr< SolverI > solver(new SolverI(writer, n));
	for (int i = 0; i < m; i++)
	{
		int u = read_int();
		int v = read_int();
		solver->fill(u - 1, v - 1);
	}
	solver->solve();
	return 0;
}
