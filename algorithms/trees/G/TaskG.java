import java.util.*;

import static java.lang.Math.max;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class TaskG {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt();
		final MaxTree tree = new MaxTree();
		final List<Integer> answer = new ArrayList<>();
		in.nextLine();
		for (int i = 0; i < n; i++) {
			final int c = in.nextInt(), k = in.nextInt();
			switch (c) {
				case 1: {
					tree.add(k);
					break;
				}
				case 0: {
					answer.add(tree.get(k));
					break;
				}
				case -1: {
					tree.del(k);
					break;
				}
				default: {
					throw new AssertionError();
				}
			}
		}
		in.close();
		for (final Integer item : answer) {
			System.out.println(item);
		}
	}

	private final static class MaxTree {
		private static Node ROOT;

		public MaxTree() {
			ROOT = null;
		}

		public void add(final int k) {
			ROOT = this.insert(ROOT, k);
		}

		private Node insert(Node root, final int x) {
			if (root != null) {
				if (root.x > x) {
					root.left = this.insert(root.left, x);
					root = this.rebalance(root);
					this.recalcSize(root.left);
					this.recalcSize(root.right);
					this.recalcSize(root);
				} else {
					if (root.x < x) {
						root.right = this.insert(root.right, x);
						root = this.rebalance(root);
						this.recalcSize(root.left);
						this.recalcSize(root.right);
						this.recalcSize(root);
					}
				}
			} else {
				root = new Node(x);
				this.recalcSize(root.left);
				this.recalcSize(root.right);
				this.recalcSize(root);
			}
			return root;
		}

		private Node rebalance(Node root) {
			root.heightLeft = this.recalcHeight(root.left);
			root.heightRight = this.recalcHeight(root.right);
			root.height = max(root.heightLeft, root.heightRight);

			this.recalcSize(root.left);
			this.recalcSize(root.right);
			this.recalcSize(root);

			if (root.heightRight >= root.heightLeft + 2) {
				if (root.right.right != null) {
					root = this.rotateLeft(root);
					this.recalcSize(root.left);
					this.recalcSize(root.right);
					this.recalcSize(root);
				} else {
					if (root.right.left != null) {
						root = this.bigRotateLeft(root);
						this.recalcSize(root.left);
						this.recalcSize(root.right);
						this.recalcSize(root);
					}
				}
			}

			if (root.heightLeft >= root.heightRight + 2) {
				if (root.left.left != null) {
					root = this.rotateRight(root);
					this.recalcSize(root.left);
					this.recalcSize(root.right);
					this.recalcSize(root);
				} else {
					if (root.left.right != null) {
						root = this.bigRotateRight(root);
						this.recalcSize(root.left);
						this.recalcSize(root.right);
						this.recalcSize(root);
					}
				}
			}
			return root;
		}

		public void del(final int k) {
			ROOT = this.deletePoint(ROOT, k);
		}

		private Node deletePoint(Node root, final int x) {
			if (root != null) {
				if (root.x > x) {
					root.left = this.deletePoint(root.left, x);
					root = this.rebalance(root);
					this.recalcSize(root.left);
					this.recalcSize(root.right);
					this.recalcSize(root);
				} else {
					if (root.x < x) {
						root.right = this.deletePoint(root.right, x);
						root = this.rebalance(root);
						this.recalcSize(root.left);
						this.recalcSize(root.right);
						this.recalcSize(root);
					} else {
						if (root.left != null && root.right != null) {
							root.x = getMin(root.right).x;
							root.right = deletePoint(root.right, root.x);
							root = this.rebalance(root);
							this.recalcSize(root.left);
							this.recalcSize(root.right);
							this.recalcSize(root);
						} else {
							if (root.left != null) {
								root = root.left;
								root = this.rebalance(root);
								this.recalcSize(root.left);
								this.recalcSize(root.right);
								this.recalcSize(root);
							} else {
								if (root.right != null) {
									root = root.right;
									root = this.rebalance(root);
									this.recalcSize(root.left);
									this.recalcSize(root.right);
									this.recalcSize(root);
								} else {
									root = null;
								}
							}
						}
					}
				}
			}
			return root;
		}

		public Integer get(final int k) {
			return this.getMax(ROOT, k);
		}

		private int getMax(final Node root, final int k) {
			if (k - 1 == 0) {
				if (root.right != null) {
					return this.getMax(root.right, k);
				} else {
					return root.x;
				}
			} else {
				if (root.right != null) {
					if (k == root.right.size + 1) {
						return root.x;
					}
					if (k < root.right.size + 1) {
						return this.getMax(root.right, k);
					} else {
						return this.getMax(root.left, k - root.right.size - 1);
					}
				} else {
					return this.getMax(root.left, k - 1);
				}
			}
		}

		private Node rotateLeft(final Node node) {
			final Node temp = node.right;
			node.right = temp.left;
			temp.left = node;
			return temp;
		}

		private Node rotateRight(final Node node) {
			final Node temp = node.left;
			node.left = temp.right;
			temp.right = node;
			return temp;
		}

		private Node bigRotateLeft(final Node node) {
			node.right = this.rotateRight(node.right);
			return this.rotateLeft(node);
		}

		private Node bigRotateRight(final Node node) {
			node.left = rotateLeft(node.left);
			return this.rotateRight(node);
		}

		private int recalcHeight(final Node node) {
			if (node == null) {
				return 0;
			}

			if (node.left == null && node.right == null) {
				return 1;
			}

			if (node.right == null) {
				return node.left.height + 1;
			}

			if (node.left == null) {
				return node.right.height + 1;
			}

			return max(node.right.height, node.left.height) + 1;
		}

		private Node getMin(final Node node) {
			if (node.left == null) {
				return node;
			} else {
				return this.getMin(node.left);
			}
		}

		private void recalcSize(final Node node) {
			if (node == null) {
				return;
			}

			node.size = 0;

			if (node.left != null) {
				node.size += node.left.size;
			}

			if (node.right != null) {
				node.size += node.right.size;
			}

			node.size++;
		}

		private final static class Node {
			private Node left, right;
			private int x, height, heightRight, heightLeft, size;

			public Node(final int v) {
				this.left = null;
				this.right = null;
				this.x = v;
				this.height = 1;
				this.heightRight = 0;
				this.heightLeft = 0;
				this.size = 0;
			}
		}
	}
}
