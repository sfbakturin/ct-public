import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class TaskB {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt(), m = in.nextInt();
		final long[] a = new long[n];
		in.nextLine();
		for (int i = 0; i < n; i++) {
			a[i] = in.nextLong();
		}
		final SegmentTree st = new SegmentTree(a);
		final List<String> answer = new ArrayList<>();
		in.nextLine();
		for (int i = 0; i < m; i++) {
			final int op = in.nextInt(), x = in.nextInt(), y = in.nextInt();
			if (op == 1) {
				st.set(x, y);
			} else {
				final long[] z = st.min(x, y);
				answer.add(z[0] + " " + z[1]);
			}
		}
		in.close();
		for (final String item : answer) {
			System.out.println(item);
		}
	}

	private static class SegmentTree {
		private static Node[] SEGMENT_TREE;
		private final int SIZE;

		public SegmentTree(final long[] i) {
			final long[] array = new long[(int) Math.pow(2, Math.ceil(Math.log(i.length) / Math.log(2)))];
			SIZE = array.length;
			System.arraycopy(i, 0, array, 0, i.length);
			SEGMENT_TREE = new Node[2 * array.length - 1];
			this.build(0, SIZE, 0, array);
		}

		public void set(final int i, final int x) {
			this.set(i, x, 0, 0, SIZE);
		}

		private void set(final int i, final int x, final int v, final int lx, final int rx) {
			if (rx - lx == 1) {
				SEGMENT_TREE[v].value = x;
			} else {
				final int m = (lx + rx + 1) / 2;
				if (i < m) {
					this.set(i, x, 2 * v + 1, lx, m);
				} else {
					this.set(i, x, 2 * v + 2, m, rx);
				}
				this.recall(v);
			}
		}

		private void recall(int v) {
			if (SEGMENT_TREE[2 * v + 1].value > SEGMENT_TREE[2 * v + 2].value) {
				SEGMENT_TREE[v] = new Node(SEGMENT_TREE[2 * v + 2].value, SEGMENT_TREE[2 * v + 2].count);
				return;
			}
			if (SEGMENT_TREE[2 * v + 1].value < SEGMENT_TREE[2 * v + 2].value) {
				SEGMENT_TREE[v] = new Node(SEGMENT_TREE[2 * v + 1].value, SEGMENT_TREE[2 * v + 1].count);
				return;
			}
			SEGMENT_TREE[v] = new Node(SEGMENT_TREE[2 * v + 1].value,
					SEGMENT_TREE[2 * v + 2].count + SEGMENT_TREE[2 * v + 1].count);
		}

		public long[] min(final int l, final int r) {
			return this.min(l, r, 0, 0, SIZE);
		}

		private long[] min(final int l, final int r, final int v, final int lx, final int rx) {
			if (r <= lx || rx <= l) {
				return null;
			}

			if (l <= lx && rx <= r) {
				return new long[]{SEGMENT_TREE[v].value, SEGMENT_TREE[v].count};
			}

			final int m = (lx + rx + 1) / 2;
			final long[] a = this.min(l, r, 2 * v + 1, lx, m);
			final long[] b = this.min(l, r, 2 * v + 2, m, rx);
			if (a != null && b != null) {
				if (a[0] > b[0]) {
					return new long[]{b[0], b[1]};
				}

				if (a[0] < b[0]) {
					return new long[]{a[0], a[1]};
				}

				return new long[]{a[0], b[1] + a[1]};
			}

			if (a != null) {
				return new long[]{a[0], a[1]};
			}

			if (b != null) {
				return new long[]{b[0], b[1]};
			}

			return null;
		}

		@Override
		public String toString() {
			final StringBuilder sb = new StringBuilder();
			for (final Node item : SEGMENT_TREE) {
				sb.append(item.value).append(" -> ").append(item.count).append("\n");
			}
			return sb.toString();
		}

		private void build(final int lx, final int rx, final int v, final long[] a) {
			if (rx - lx == 1) {
				SEGMENT_TREE[v] = new Node(a[lx], 1);
			} else {
				final int m = (lx + rx + 1) / 2;
				this.build(lx, m, 2 * v + 1, a);
				this.build(m, rx, 2 * v + 2, a);
				this.recall(v);
			}
		}

		private static class Node {
			private long value;
			private final long count;

			public Node(final long v, final long c) {
				this.value = v;
				this.count = c;
			}
		}
	}
}
