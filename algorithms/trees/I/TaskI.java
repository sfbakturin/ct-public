import java.util.Arrays;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class TaskI {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt(), m = in.nextInt();
		final Tree tree = new Tree(n);
		for (int i = 0; i < m; i++) {
			final int l = in.nextInt(), r = in.nextInt();
			tree.action(l, r);
		}
		in.close();
		tree.print();
	}

	private final static class Tree {
		public static Node ROOT;

		public Tree(final int n) {
			final int[] array = new int[n];
			for (int i = 0; i < n; i++) {
				array[i] = i + 1;
			}
			ROOT = this.build(array, (int) (Math.random() * Integer.MAX_VALUE));
		}

		private void print(final Node root) {
			if (root != null) {
				print(root.left);
				System.out.print(root.value + " ");
				print(root.right);
			}
		}

		private void action(final int l, final int r) {
			final Node[] splitL = this.split(ROOT, l - 1);
			final Node[] splitM = this.split(splitL[1], r - l + 1);
			final Node merged = this.merge(splitM[0], splitL[0]);
			ROOT = this.merge(merged, splitM[1]);
		}

		private Node build(final int[] array, final int pr) {
			if (array.length == 0) {
				return null;
			}
			final Node point = new Node(array[array.length / 2], pr);
			final int[] left = Arrays.copyOfRange(array, 0, array.length / 2);
			final int[] right = Arrays.copyOfRange(array, (array.length / 2) + 1, array.length);
			point.left = this.build(left, (pr / 4));
			point.right = this.build(right, (pr / 2));
			this.recalc(point);
			return point;
		}

		private Node merge(final Node a, final Node b) {
			if (a == null) {
				return b;
			}

			if (b == null) {
				return a;
			}

			if (a.pr > b.pr) {
				a.right = this.merge(a.right, b);
				this.recalc(a);
				return a;
			} else {
				b.left = this.merge(a, b.left);
				this.recalc(b);
				return b;
			}
		}

		private Node[] split(final Node node, final int x) {
			if (node == null) {
				return new Node[]{null, null};
			}

			if (size(node.left) >= x) {
				final Node[] split = this.split(node.left, x);
				node.left = split[1];
				recalc(node);
				return new Node[]{split[0], node};
			} else {
				final Node[] split = this.split(node.right, x - size(node.left) - 1);
				node.right = split[0];
				recalc(node);
				return new Node[]{node, split[1]};
			}
		}

		private void recalc(final Node node) {
			node.size = 1 + this.size(node.left) + this.size(node.right);
		}

		private int size(final Node node) {
			if (node == null) {
				return 0;
			}

			return node.size;
		}

		public void print() {
			this.print(ROOT);
		}
	}

	private final static class Node {
		private Node left, right;
		private final int pr, value;
		private int size;

		public Node(final int v, final int p) {
			this.left = null;
			this.right = null;
			this.pr = p;
			this.value = v;
			this.size = 1;
		}
	}
}
