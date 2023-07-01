import java.util.*;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class TaskH {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt(), m = in.nextInt();
		final int[] array = new int[n];
		in.nextLine();
		for (int i = 0; i < n; i++) {
			array[i] = in.nextInt();
		}
		Node root = build((int) (Math.random() * Integer.MAX_VALUE), array);
		in.nextLine();
		for (int i = 0; i < m; i++) {
			final String[] s = in.nextLine().split(" ");
			if (s[0].equals("add")) {
				root = add(root, Integer.parseInt(s[1]), Integer.parseInt(s[2]));
			} else {
				root = del(root, Integer.parseInt(s[1]) - 1);
			}
		}
		in.close();
		System.out.println(size(root));
		print(root);
	}

	private static Node del(final Node node, final int x) {
		final Node[] s1 = split(node, x);
		final Node[] s2 = split(s1[1], 1);
		return merge(s1[0], s2[1]);
	}

	private static Node build(final int priority, final int[] array) {
		if (array.length == 0) {
			return null;
		}
		final Node point = new Node();
		point.value = array[array.length / 2];
		point.pr = priority;
		final int[] left = Arrays.copyOfRange(array, 0, array.length / 2);
		final int[] right = Arrays.copyOfRange(array, (array.length / 2) + 1, array.length);
		point.left = build((priority / 4), left);
		point.right = build((priority / 3), right);
		recalc(point);
		return point;
	}

	private static Node add(final Node node, final int index, final int value) {
		final Node[] split = split(node, index);
		final Node point = new Node();
		point.pr = (int) (Math.random() * Integer.MAX_VALUE);
		point.size = 1;
		point.value = value;
		return merge(merge(split[0], point), split[1]);
	}

	private static void print(final Node root) {
		if (root != null) {
			print(root.left);
			System.out.print(root.value + " ");
			print(root.right);
		}
	}

	private static Node merge(final Node a, final Node b) {
		if (a == null) {
			return b;
		}

		if (b == null) {
			return a;
		}

		if (a.pr > b.pr) {
			a.right = merge(a.right, b);
			recalc(a);
			return a;
		} else {
			b.left = merge(a, b.left);
			recalc(b);
			return b;
		}
	}

	private static Node[] split(final Node node, final int x) {
		if (node == null) {
			return new Node[]{null, null};
		}

		if (size(node.left) >= x) {
			final Node[] split = split(node.left, x);
			node.left = split[1];
			recalc(node);
			return new Node[]{split[0], node};
		} else {
			final Node[] split = split(node.right, x - size(node.left) - 1);
			node.right = split[0];
			recalc(node);
			return new Node[]{node, split[1]};
		}
	}

	private static void recalc(final Node o) {
		o.size = 1 + size(o.left) + size(o.right);
	}

	private static int size(final Node o) {
		if (o == null) {
			return 0;
		}

		return o.size;
	}

	private final static class Node {
		private Node left, right;
		private int pr, value, size;

		public Node() {
			this.left = null;
			this.right = null;
		}
	}
}
