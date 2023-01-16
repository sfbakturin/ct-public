import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import static java.lang.Math.max;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class TaskC {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt(), m = in.nextInt();
		final int[] a = new int[n];
		in.nextLine();
		for (int i = 0; i < n; i++) {
			a[i] = in.nextInt();
		}
		final SegmentTree st = new SegmentTree(a);
		final List<Long> answer = new ArrayList<>();
		answer.add(st.get());
		for (int i = 0; i < m; i++) {
			final int l = in.nextInt(), r = in.nextInt();
			st.set(l, r);
			answer.add(st.get());
		}
		in.close();
		for (final Long item : answer) {
			System.out.println(item);
		}
	}

	private static class SegmentTree {
		private static Node[] SEGMENT_TREE;
		private final int SIZE;

		public SegmentTree(final int[] i) {
			final int[] array = new int[(int) Math.pow(2, Math.ceil(Math.log(i.length) / Math.log(2)))];
			SIZE = array.length;
			System.arraycopy(i, 0, array, 0, i.length);
			SEGMENT_TREE = new Node[2 * array.length - 1];
			this.build(0, SIZE, 0, array);
		}

		public long get() {
			return SEGMENT_TREE[0].MAX_SUM;
		}

		public void set(final int i, final int x) {
			this.set(i, x, 0, SIZE, 0);
		}

		private void set(final int i, final int x, final int lx, final int rx, final int v) {
			if (rx - lx == 1) {
				SEGMENT_TREE[v] = new Node(x, max(x, 0), max(x, 0), max(x, 0));
			} else {
				final int m = (lx + rx + 1) / 2;
				if (i < m) {
					this.set(i, x, lx, m, 2 * v + 1);
				} else {
					this.set(i, x, m, rx, 2 * v + 2);
				}
				SEGMENT_TREE[v] = new Node(
						SEGMENT_TREE[2 * v + 1].CURRENT_SUM + SEGMENT_TREE[2 * v + 2].CURRENT_SUM,
						max(SEGMENT_TREE[2 * v + 1].PRE_SUM,
								SEGMENT_TREE[2 * v + 1].CURRENT_SUM + SEGMENT_TREE[2 * v + 2].PRE_SUM),
						max(SEGMENT_TREE[2 * v + 2].POST_SUM,
								SEGMENT_TREE[2 * v + 2].CURRENT_SUM + SEGMENT_TREE[2 * v + 1].POST_SUM),
						max(SEGMENT_TREE[2 * v + 1].POST_SUM + SEGMENT_TREE[2 * v + 2].PRE_SUM,
								max(SEGMENT_TREE[2 * v + 1].MAX_SUM, SEGMENT_TREE[2 * v + 2].MAX_SUM)));
			}
		}

		private void build(final int lx, final int rx, final int v, final int[] a) {
			if (rx - lx == 1) {
				SEGMENT_TREE[v] = new Node(a[lx], max(a[lx], 0), max(a[lx], 0), max(a[lx], 0));
			} else {
				final int m = (lx + rx + 1) / 2;
				this.build(lx, m, 2 * v + 1, a);
				this.build(m, rx, 2 * v + 2, a);
				SEGMENT_TREE[v] = new Node(
						SEGMENT_TREE[2 * v + 1].CURRENT_SUM + SEGMENT_TREE[2 * v + 2].CURRENT_SUM,
						max(SEGMENT_TREE[2 * v + 1].PRE_SUM,
								SEGMENT_TREE[2 * v + 1].CURRENT_SUM + SEGMENT_TREE[2 * v + 2].PRE_SUM),
						max(SEGMENT_TREE[2 * v + 2].POST_SUM,
								SEGMENT_TREE[2 * v + 2].CURRENT_SUM + SEGMENT_TREE[2 * v + 1].POST_SUM),
						max(SEGMENT_TREE[2 * v + 1].POST_SUM + SEGMENT_TREE[2 * v + 2].PRE_SUM,
								max(SEGMENT_TREE[2 * v + 1].MAX_SUM, SEGMENT_TREE[2 * v + 2].MAX_SUM)));
			}
		}

		private static class Node {
			private final long CURRENT_SUM, MAX_SUM, PRE_SUM, POST_SUM;

			public Node(final long c, final long pr, final long po, final long m) {
				this.CURRENT_SUM = c;
				this.PRE_SUM = pr;
				this.POST_SUM = po;
				this.MAX_SUM = m;
			}
		}
	}
}
