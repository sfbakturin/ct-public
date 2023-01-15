#include <cstdio>
#include <limits>
#include <map>
#include <utility>
#include <vector>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

#define MAX 1000000000000000

bool comp(std::pair< int, long long > const & a, std::pair< int, long long > const & b) noexcept
{
	return a.first < b.first;
}

bool dijkstra(std::vector< std::vector< std::pair< int, int > > > const & edges, int const & start, int const & a, int const & b, int const & c, long long& result) noexcept
{
	std::vector< long long > dist(edges.size(), MAX);
	// std::set< std::pair< int, long long > > queue;
	std::map<int, long long> queue;
	dist[start] = 0;
	queue[start] = 0;
	while (!queue.empty())
	{
		std::pair< int, long long > extracted = *(queue.begin());
		queue.erase(queue.begin());
		for (auto const & item : edges[extracted.first])
		{
			if (dist[item.first] > extracted.second + item.second)
			{
				dist[item.first] = extracted.second + item.second;
				queue[item.first] = dist[item.first];
			}
		}
	}
	if (dist[a] >= MAX || dist[b] >= MAX || dist[c] >= MAX)
	{
		return false;
	}
	else
	{
		result = std::min(result, dist[a] + dist[b] + dist[c]);
		return true;
	}
}

int main() noexcept
{
	int n, m, a, b, c;
	scanf("%i %i", &n, &m);
	std::vector< std::vector< std::pair< int, int > > > edges(n, std::vector< std::pair< int, int > >());
	for (int i = 0; i < m; i++)
	{
		int u, v, w;
		scanf("%i %i %i", &u, &v, &w);
		edges[u - 1].emplace_back(v - 1, w);
		edges[v - 1].emplace_back(u - 1, w);
	}
	scanf("%i %i %i", &a, &b, &c);
	a--;
	b--;
	c--;
	long long result = std::numeric_limits< long long >::max();
	if (!dijkstra(edges, a, a, b, c, result))
	{
		printf("-1\n");
	}
	else
	{
		dijkstra(edges, b, a, b, c, result);
		dijkstra(edges, c, a, b, c, result);
		printf("%lld\n", result);
	}
	return 0;
}
