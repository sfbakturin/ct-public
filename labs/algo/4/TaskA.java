import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class TaskA {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt(), m = in.nextInt();
        final long[] a = new long[n];
        in.nextLine();
        for (int i = 0; i < n; i++) {
            a[i] = in.nextInt();
        }
        final SegmentTree st = new SegmentTree(a);
        final List<Long> answer = new ArrayList<>();
        in.nextLine();
        for (int i = 0; i < m; i++) {
            final int op = in.nextInt(), x = in.nextInt(), y = in.nextInt();
            if (op == 1) {
                st.set(x, y);
            } else {
                answer.add(st.sum(x, y));
            }
        }
        in.close();
        for (final Long item : answer) {
            System.out.println(item);
        }
    }

    private static class SegmentTree {
        private static long[] SEGMENT_TREE;
        private final int SIZE;

        public SegmentTree(final long[] i) {
            final long[] array = new long[(int) Math.pow(2, Math.ceil(Math.log(i.length) / Math.log(2)))];
            SIZE = array.length;
            System.arraycopy(i, 0, array, 0, i.length);
            SEGMENT_TREE = new long[2 * array.length - 1];
            this.build(0, SIZE, 0, array);
        }

        public long sum(final int l, final int r) {
            return this.sum(l, r, 0, 0, SIZE);
        }

        public void set(final int i, final int x) {
            this.set(i, x, 0, 0, SIZE);
        }

        private long sum(final int l, final int r, final int v, final int lx, final int rx) {
            if (r <= lx || rx <= l) {
                return 0;
            }

            if (l <= lx && rx <= r) {
                return SEGMENT_TREE[v];
            }

            final int m = (lx + rx + 1) / 2;
            return this.sum(l, r, 2 * v + 1, lx, m) + this.sum(l, r, 2 * v + 2, m, rx);
        }

        private void set(final int i, final int x, final int v, final int lx, final int rx) {
            if (rx - lx == 1) {
                SEGMENT_TREE[v] = x;
            } else {
                final int m = (lx + rx + 1) / 2;
                if (i < m) {
                    this.set(i, x, 2 * v + 1, lx, m);
                } else {
                    this.set(i, x, 2 * v + 2, m, rx);
                }
                SEGMENT_TREE[v] = SEGMENT_TREE[2 * v + 1] + SEGMENT_TREE[2 * v + 2];
            }
        }

        private void build(final int lx, final int rx, final int v, final long[] a) {
            if (rx - lx == 1) {
                SEGMENT_TREE[v] = a[lx];
            } else {
                final int m = (lx + rx + 1) / 2;
                this.build(lx, m, 2 * v + 1, a);
                this.build(m, rx, 2 * v + 2, a);
                SEGMENT_TREE[v] = SEGMENT_TREE[2 * v + 1] + SEGMENT_TREE[2 * v + 2];
            }
        }
    }
}
