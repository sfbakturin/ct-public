#include <cstdio>
#include <stack>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

class Queue {
private:
	std::stack<int> left, right, leftMin, rightMin;

	void print();
public:
	Queue() = default;
	~Queue() = default;

	void add(const int x);
	void remove();
};

void Queue::add(const int x) {
	left.push(x);
	if (leftMin.empty()) {
		leftMin.push(x);
	} else {
		leftMin.push(x > leftMin.top() ? leftMin.top() : x);
	}
	Queue::print();
};

void Queue::remove() {
	if (right.empty()) {
		while (!left.empty()) {
			int x = left.top();
			left.pop();
			leftMin.pop();
			right.push(x);
			if (rightMin.empty()) {
				rightMin.push(x);
			} else {
				rightMin.push(x > rightMin.top() ? rightMin.top() : x);
			}
		}
	}
	right.pop();
	rightMin.pop();
	Queue::print();
};

void Queue::print() {
	if (!leftMin.empty() && !rightMin.empty()) {
		printf("%i\n", std::min(leftMin.top(), rightMin.top()));
	} else {
		if (leftMin.empty() && !rightMin.empty()) {
			printf("%i\n", rightMin.top());
		} else {
			if (!leftMin.empty() && rightMin.empty()) {
				printf("%i\n", leftMin.top());
			} else {
				printf("-1\n");
			}
		}
	}
};

int main(void) {
	int q;
	scanf("%i", &q);
	Queue queue;
	for (int i = 0; i < q; i++) {
		char op;
		scanf(" %c", &op);
		if (op == '+') {
			int val;
			scanf("%i", &val);
			queue.add(val);
		} else {
			queue.remove();
		}
	}
}
