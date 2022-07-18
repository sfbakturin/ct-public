import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class TaskF {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt(), m = in.nextInt();
        final SegmentTree st = new SegmentTree(n);
        final List<Integer> answer = new ArrayList<>();
        in.nextLine();
        for (int i = 0; i < m; i++) {
            final String[] s = in.nextLine().split(" ");
            if (s[0].equals("1")) {
                st.set(Integer.parseInt(s[1]), Integer.parseInt(s[2]), Integer.parseInt(s[3]));
            } else {
                answer.add(st.min(Integer.parseInt(s[1]), Integer.parseInt(s[2])));
            }
        }
        in.close();
        for (final Integer item : answer) {
            System.out.println(item);
        }
    }

    private static class SegmentTree {
        private static Node[] SEGMENT_TREE;
        private final int SIZE;

        private SegmentTree(final int size) {
            SIZE = (int) Math.pow(2, Math.ceil(Math.log(size) / Math.log(2)));
            SEGMENT_TREE = new Node[2 * SIZE - 1];
            this.build(0, SIZE, 0);
        }

        public int min(final int l, final int r) {
            return this.min(l, r, 0, SIZE, 0);
        }

        public void set(final int l, final int r, final int x) {
            this.set(l, r, x, 0, SIZE, 0);
        }

        private void set(final int l, final int r, final int x, final int lx, final int rx, final int v) {
            if (r <= lx || rx <= l) {
                return;
            }

            if (l <= lx && rx <= r) {
                SEGMENT_TREE[v].set = x;
                SEGMENT_TREE[v].min = x;
                return;
            }

            this.push(v);

            final int m = (lx + rx + 1) / 2;
            this.set(l, r, x, lx, m, 2 * v + 1);
            this.set(l, r, x, m, rx, 2 * v + 2);

            SEGMENT_TREE[v].min = Math.min(SEGMENT_TREE[2 * v + 1].min, SEGMENT_TREE[2 * v + 2].min);
        }

        private int min(final int l, final int r, final int lx, final int rx, final int v) {
            if (r <= lx || rx <= l) {
                return Integer.MAX_VALUE;
            }

            if (l <= lx && rx <= r) {
                return SEGMENT_TREE[v].min;
            }

            this.push(v);

            final int m = (lx + rx + 1) / 2;
            final int a = this.min(l, r, lx, m, 2 * v + 1);
            final int b = this.min(l, r, m, rx, 2 * v + 2);

            return Math.min(a, b);
        }

        private void push(final int v) {
            if (SEGMENT_TREE[v].set != null) {
                SEGMENT_TREE[2 * v + 1].min = SEGMENT_TREE[v].set;
                SEGMENT_TREE[2 * v + 2].min = SEGMENT_TREE[v].set;
                SEGMENT_TREE[2 * v + 1].set = SEGMENT_TREE[v].set;
                SEGMENT_TREE[2 * v + 2].set = SEGMENT_TREE[v].set;
                SEGMENT_TREE[v].set = null;
            }
        }

        private void build(final int lx, final int rx, final int v) {
            if (rx - lx == 1) {
                SEGMENT_TREE[v] = new Node(0);
            } else {
                final int m = (lx + rx + 1) / 2;
                this.build(lx, m, 2 * v + 1);
                this.build(m, rx, 2 * v + 2);
                SEGMENT_TREE[v] = new Node(0);
            }
        }

        private static class Node {
            private int min;
            private Integer set;

            public Node(final int m) {
                this.min = m;
                this.set = null;
            }
        }
    }
}
