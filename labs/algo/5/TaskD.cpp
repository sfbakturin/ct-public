#include <cstdio>
#include <cstring>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

class SplayTree {
private:
	struct Node {
		Node *left = nullptr;
		Node *right = nullptr;
		Node *parent = nullptr;
		int val = 0;
	};

	Node *m_tree = nullptr;

	static Node *p(Node *const node) { return node->parent; }

	static Node *g(Node *const node) { return p(p(node)); }

	static void rotate_right(Node *const p) {
		Node *x = p->left;
		Node *b = x->right;
		if (p->parent) {
			if (p->parent->right == p) {
				p->parent->right = x;
			} else {
				p->parent->left = x;
			}
		}
		x->parent = p->parent;
		x->right = p;
		p->left = b;
		p->parent = x;
		if (b) b->parent = p;
	}

	static void rotate_left(Node *const p) {
		Node *x = p->right;
		Node *b = x->left;
		if (p->parent) {
			if (p->parent->right == p) {
				p->parent->right = x;
			} else {
				p->parent->left = x;
			}
		}
		x->parent = p->parent;
		x->left = p;
		p->right = b;
		p->parent = x;
		if (b) b->parent = p;
	}

	static Node *splay(Node *const current_node) {
		while (p(current_node)) {
			if (current_node == p(current_node)->left) {
				if (!g(current_node)) {
					rotate_right(p(current_node));
				} else if (p(current_node) == g(current_node)->left) {
					rotate_right(g(current_node));
					rotate_right(p(current_node));
				} else {
					rotate_right(p(current_node));
					rotate_left(p(current_node));
				}
			} else {
				if (!g(current_node)) {
					rotate_left(p(current_node));
				} else if (p(current_node) == g(current_node)->right) {
					rotate_left(g(current_node));
					rotate_left(p(current_node));
				} else {
					rotate_left(p(current_node));
					rotate_right(p(current_node));
				}
			}
		}
		return current_node;
	}

	static Node *get_max(Node *const current_node) {
		if (current_node->right) {
			return get_max(current_node->right);
		} else {
			return current_node;
		}
	}

	static Node *merge(Node *left, Node *const right) {
		if (!left && !right) {
			return nullptr;
		}
		if (left && !right) {
			left->parent = nullptr;
			return left;
		}
		if (!left) {
			right->parent = nullptr;
			return right;
		}
		left->parent = nullptr;
		left = splay(get_max(left));
		left->right = right;
		right->parent = left;
		return left;
	}

	Node *find(int const &value) {
		Node *track = m_tree;
		while (track) {
			if (track->val > value) {
				track = track->left;
			} else if (track->val < value) {
				track = track->right;
			} else return track;
		}
		return nullptr;
	}

public:
	SplayTree() = default;

	~SplayTree() { delete m_tree; }

	void insert(int const &value) {
		Node *new_node = m_tree;
		Node *parent_new_node = nullptr;
		while (new_node) {
			parent_new_node = new_node;
			if (new_node->val < value) {
				new_node = new_node->right;
			} else if (new_node->val > value) {
				new_node = new_node->left;
			} else return;
		}
		new_node = new Node();
		new_node->val = value;
		new_node->parent = parent_new_node;
		if (!parent_new_node) m_tree = new_node;
		else if (parent_new_node->val < new_node->val) parent_new_node->right = new_node;
		else parent_new_node->left = new_node;
		m_tree = splay(new_node);
	}

	void remove(int const &value) {
		Node *found = find(value);
		if (!found) return;
		splay(found);
		m_tree = merge(found->left, found->right);
		delete found;
	}

	bool exists(int const &value) {
		Node *const found = find(value);
		if (found) m_tree = splay(found);
		return found != nullptr;
	}

	void next(int const &value) {
		Node *prev = m_tree;
		Node *found = nullptr;
		while (prev) {
			if (prev->val > value) {
				found = prev;
				prev = prev->left;
			} else {
				prev = prev->right;
			}
		}
		if (found) {
			m_tree = splay(found);
			std::printf("%i\n", m_tree->val);
		} else {
			std::printf("none\n");
		}
	}

	void prev(int const &value) {
		Node *prev = m_tree;
		Node *found = nullptr;
		while (prev) {
			if (prev->val < value) {
				found = prev;
				prev = prev->right;
			} else {
				prev = prev->left;
			}
		}
		if (found) {
			m_tree = splay(found);
			std::printf("%i\n", m_tree->val);
		} else {
			std::printf("none\n");
		}
	}
};

int main() {
	SplayTree bst;
	char op[7];
	op[6] = '\0';
	while (std::scanf("%s", op) != EOF) {
		int v;
		std::scanf("%i", &v);
		if (!std::strcmp(op, "insert")) {
			bst.insert(v);
		} else if (!std::strcmp(op, "delete")) {
			bst.remove(v);
		} else if (!std::strcmp(op, "exists")) {
			std::printf("%s\n", (bst.exists(v) ? "true" : "false"));
		} else if (!std::strcmp(op, "next")) {
			bst.next(v);
		} else {
			bst.prev(v);
		}
	}
}
