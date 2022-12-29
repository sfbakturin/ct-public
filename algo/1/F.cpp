#include <cstdio>
#include <vector>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

class Heap {
private:
	std::vector<int> v;

public:
	Heap() = default;
	~Heap() = default;

	void insert(int const x);
	int extract();
};

void Heap::insert(int const x) {
	v.push_back(x);
	int i = v.size() - 1;
	while (v[i] > v[(i - 1) / 2]) {
		std::swap(v[i], v[(i - 1) / 2]);
		i = (i - 1) / 2;
	}
}

int Heap::extract() {
	int const max = v[0];
	v[0] = v[v.size() - 1];
	int i = 0;
	while (2 * i + 1 < v.size()) {
		int left = 2 * i + 1;
		int right = 2 * i + 2;
		int j = left;
		if (right < v.size() && v[right] > v[left]) {
			j = right;
		}
		if (v[i] >= v[j]) {
			break;
		}
		std::swap(v[i], v[j]);
		i = j;
	}
	v.resize(v.size() - 1);
	return max;
}

int main() {
	int n;
	scanf("%i", &n);
	Heap heap;
	for (int i = 0; i < n; i++) {
		int op;
		scanf("%i", &op);
		if (op == 0) {
			int val;
			scanf("%i", &val);
			heap.insert(val);
		} else {
			printf("%i\n", heap.extract());
		}
	}
}
