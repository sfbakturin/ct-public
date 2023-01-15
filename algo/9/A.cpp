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

struct SolverA : AbstractSolver
{
  private:
	Writer &writer;
	int n = 0;
	std::vector< int > matrix;

  public:
	constexpr SolverA() noexcept = delete;
	SolverA(Writer &writer, int const &n_) noexcept : writer(writer), n(n_), matrix(n * n) {}
	~SolverA() noexcept override = default;

	void fill(int const &i, int const &j, int const &w) noexcept { matrix[i * n + j] = w; }

	void solve() noexcept override
	{
		for (int k = 0; k < n; k++)
		{
			for (int u = 0; u < n; u++)
			{
				for (int v = 0; v < n; v++)
				{
					if (u != v)
					{
						matrix[u * n + v] = std::min(matrix[u * n + v], matrix[u * n + k] + matrix[k * n + v]);
					}
				}
			}
		}
		for (int i = 0; i < n; i++)
		{
			for (int j = 0; j < n; j++)
			{
				writer.write(matrix[i * n + j]);
				writer.whitespace();
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
	int n = reader.read< int >();
	std::unique_ptr< SolverA > solver(new SolverA(writer, n));
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
