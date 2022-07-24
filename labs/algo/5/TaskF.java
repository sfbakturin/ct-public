import java.util.*;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class TaskF {
	public final static int MOD = 1000000000;
	public final static int MAX = Integer.MAX_VALUE;

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt();
		final SumTree tree = new SumTree();
		final List<Long> list = new ArrayList<>();
		boolean prevFlag = false;
		long prevSum = 0;
		in.nextLine();
		for (int i = 0; i < n; i++) {
			final String[] s = in.nextLine().split(" ");
			if (s.length == 2) {
				if (prevFlag) {
					tree.add((Long.parseLong(s[1]) + prevSum) % MOD);
				} else {
					tree.add(Integer.parseInt(s[1]));
				}
			} else {
				prevSum = tree.sum(Integer.parseInt(s[1]), Integer.parseInt(s[2]));
				list.add(prevSum);
			}
			prevFlag = s.length == 3;
		}
		in.close();
		for (final Long item : list) {
			System.out.println(item);
		}
	}

	private final static class SumTree {
		public static Node ROOT;
		private final static Set<Long> ELEMENTS = new HashSet<>();

		public SumTree() {
			ROOT = null;
		}

		public void add(final long x) {
			if (!ELEMENTS.contains(x)) {
				ELEMENTS.add(x);
				final Node[] split = this.split(ROOT, x);
				final Node point = new Node(x, (int) (Math.random() * MAX));
				ROOT = this.merge(this.merge(split[0], point), split[1]);
				this.recalc(ROOT);
			}
		}

		public long sum(final int l, final int r) {
			final Node[] splitR = this.split(ROOT, r);
			this.recalc(splitR[0]);
			this.recalc(splitR[1]);
			final Node[] splitL = this.split(splitR[0], l - 1);
			this.recalc(splitL[0]);
			this.recalc(splitL[1]);
			final long result = this.sum(splitL[1]);
			ROOT = this.merge(this.merge(splitL[0], splitL[1]), splitR[1]);
			this.recalc(ROOT);
			return result;
		}

		private Node[] split(final Node node, final long x) {
			if (node == null) {
				return new Node[]{null, null};
			}

			if (this.value(node) > x) {
				final Node[] split = this.split(node.left, x);
				node.left = split[1];
				this.recalc(node);
				return new Node[]{split[0], node};
			} else {
				final Node[] split = this.split(node.right, x);
				node.right = split[0];
				this.recalc(node);
				return new Node[]{node, split[1]};
			}
		}

		private Node merge(final Node a, final Node b) {
			if (a == null) {
				return b;
			}

			if (b == null) {
				return a;
			}

			if (a.pr <= b.pr) {
				a.right = this.merge(a.right, b);
				this.recalc(a);
				return a;
			} else {
				b.left = this.merge(a, b.left);
				this.recalc(b);
				return b;
			}
		}

		private void recalc(final Node node) {
			if (node != null) {
				node.sum = this.sum(node.left) + this.sum(node.right) + node.value;
			}
		}

		private long sum(final Node node) {
			if (node == null) {
				return 0;
			}

			return node.sum;
		}

		private long value(final Node node) {
			if (node == null) {
				return 0;
			}

			return node.value;
		}

		private final static class Node {
			private Node left, right;
			private long sum;
			private final long value, pr;

			public Node(final long v, final long pr) {
				this.left = null;
				this.right = null;
				this.sum = v;
				this.value = v;
				this.pr = pr;
			}
		}
	}
}
