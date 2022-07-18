import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class TaskD {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt(), m = in.nextInt();
        final int[] a = new int[n];
        in.nextLine();
        for (int i = 0; i < n; i++) {
            a[i] = in.nextInt();
        }
        final SegmentTree st = new SegmentTree(a);
        final List<Integer> answer = new ArrayList<>();
        in.nextLine();
        for (int i = 0; i < m; i++) {
            final int op = in.nextInt(), x = in.nextInt();
            if (op == 1) {
                st.set(x);
            } else {
                answer.add(st.find(x));
            }
        }
        in.close();
        for (final Integer item : answer) {
            System.out.println(item);
        }
    }

    private static class SegmentTree {
        private static int[] SEGMENT_TREE;
        private final int SIZE;

        public SegmentTree(final int[] i) {
            final int[] array = new int[(int) Math.pow(2, Math.ceil(Math.log(i.length) / Math.log(2)))];
            SIZE = array.length;
            System.arraycopy(i, 0, array, 0, i.length);
            SEGMENT_TREE = new int[2 * array.length - 1];
            this.build(0, SIZE, 0, array);
        }

        private void build(final int lx, final int rx, final int v, final int[] a) {
            if (rx - lx == 1) {
                SEGMENT_TREE[v] = a[lx];
            } else {
                final int m = (lx + rx + 1) / 2;
                this.build(lx, m, 2 * v + 1, a);
                this.build(m, rx, 2 * v + 2, a);
                SEGMENT_TREE[v] = SEGMENT_TREE[2 * v + 1] + SEGMENT_TREE[2 * v + 2];
            }
        }

        public void set(final int i) {
            this.set(i, 0, 0, SIZE);
        }

        public int find(final int k) {
            return this.find(k, 0, 0, SIZE);
        }

        private int find(final int k, final int v, final int lx, final int rx) {
            if (rx - lx == 1) {
                return lx;
            }

            final int m = (lx + rx + 1) / 2;
            if (SEGMENT_TREE[2 * v + 1] > k) {
                return this.find(k, 2 * v + 1, lx, m);
            } else {
                return this.find(k - SEGMENT_TREE[2 * v + 1], 2 * v + 2, m, rx);
            }
        }

        private void set(final int i, final int v, final int lx, final int rx) {
            if (rx - lx == 1) {
                SEGMENT_TREE[v] = SEGMENT_TREE[v] == 1 ? 0 : 1;
            } else {
                final int m = (lx + rx + 1) / 2;
                if (i < m) {
                    this.set(i, 2 * v + 1, lx, m);
                } else {
                    this.set(i, 2 * v + 2, m, rx);
                }
                SEGMENT_TREE[v] = SEGMENT_TREE[2 * v + 1] + SEGMENT_TREE[2 * v + 2];
            }
        }
    }
}
