import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class TaskG {
    public static void main(String[] args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt(), m = in.nextInt();
        final SegmentTree st = new SegmentTree(n);
        final List<Long> answer = new ArrayList<>();
        in.nextLine();
        for (int i = 0; i < m; i++) {
            final String[] s = in.nextLine().split(" ");
            if (s[0].equals("1")) {
                st.set(Integer.parseInt(s[1]), Integer.parseInt(s[2]), Integer.parseInt(s[3]));
            } else {
                if (s[0].equals("2")) {
                    st.add(Integer.parseInt(s[1]), Integer.parseInt(s[2]), Integer.parseInt(s[3]));
                } else {
                    answer.add(st.sum(Integer.parseInt(s[1]), Integer.parseInt(s[2])));
                }
            }
        }
        in.close();
        for (final Long item : answer) {
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

        public void set(final int l, final int r, final int x) {
            this.set(l, r, x, 0, SIZE, 0);
        }

        public long sum(final int l, final int r) {
            return this.sum(l, r, 0, SIZE, 0);
        }

        public void add(final int l, final int r, final int x) {
            this.add(l, r, x, 0, SIZE, 0);
        }

        private void add(final int l, final int r, final int x, final int lx, final int rx, final int v) {
            if (r <= lx || rx <= l) {
                return;
            }

            if (l <= lx && rx <= r) {
                if (rx - lx != 1) {
                    this.push(v);
                }
                SEGMENT_TREE[v].add += x;
                return;
            }

            this.push(v);

            final int m = (lx + rx + 1) / 2;
            this.add(l, r, x, lx, m, 2 * v + 1);
            this.add(l, r, x, m, rx, 2 * v + 2);

            SEGMENT_TREE[v].sum = SEGMENT_TREE[2 * v + 1].sum + SEGMENT_TREE[2 * v + 2].sum
                    + SEGMENT_TREE[2 * v + 1].add * SEGMENT_TREE[2 * v + 1].len
                    + SEGMENT_TREE[2 * v + 2].add * SEGMENT_TREE[2 * v + 2].len;
            SEGMENT_TREE[v].add = 0;
        }

        private long sum(final int l, final int r, final int lx, final int rx, final int v) {
            if (r <= lx || rx <= l) {
                return 0;
            }

            if (l <= lx && rx <= r) {
                if (rx - lx != 1) {
                    this.push(v);
                }
                return SEGMENT_TREE[v].sum + SEGMENT_TREE[v].add * SEGMENT_TREE[v].len;
            }

            this.push(v);

            final int m = (lx + rx + 1) / 2;
            return this.sum(l, r, lx, m, 2 * v + 1) + this.sum(l, r, m, rx, 2 * v + 2);
        }

        private void set(final int l, final int r, final long x, final int lx, final int rx, final int v) {
            if (r <= lx || rx <= l) {
                return;
            }

            if (l <= lx && rx <= r) {
                SEGMENT_TREE[v].set = x;
                SEGMENT_TREE[v].sum = SEGMENT_TREE[v].len * x;
                SEGMENT_TREE[v].add = 0;
                return;
            }

            this.push(v);

            final int m = (lx + rx + 1) / 2;
            this.set(l, r, x, lx, m, 2 * v + 1);
            this.set(l, r, x, m, rx, 2 * v + 2);

            SEGMENT_TREE[v].sum = SEGMENT_TREE[2 * v + 1].sum + SEGMENT_TREE[2 * v + 2].sum
                    + SEGMENT_TREE[2 * v + 1].add * SEGMENT_TREE[2 * v + 1].len
                    + SEGMENT_TREE[2 * v + 2].add * SEGMENT_TREE[2 * v + 2].len;
            SEGMENT_TREE[v].add = 0;
        }

        private void push(final int v) {
            if (SEGMENT_TREE[v].set != null) {
                SEGMENT_TREE[2 * v + 1].set = SEGMENT_TREE[v].set;
                SEGMENT_TREE[2 * v + 2].set = SEGMENT_TREE[v].set;
                SEGMENT_TREE[2 * v + 1].add = 0;
                SEGMENT_TREE[2 * v + 2].add = 0;
                SEGMENT_TREE[v].sum = SEGMENT_TREE[v].set * SEGMENT_TREE[v].len;
                SEGMENT_TREE[2 * v + 1].sum = SEGMENT_TREE[v].set * SEGMENT_TREE[2 * v + 1].len;
                SEGMENT_TREE[2 * v + 2].sum = SEGMENT_TREE[v].set * SEGMENT_TREE[2 * v + 2].len;
                SEGMENT_TREE[v].set = null;
            }
            if (SEGMENT_TREE[v].add != 0) {
                SEGMENT_TREE[v].sum += SEGMENT_TREE[v].add * SEGMENT_TREE[v].len;
                SEGMENT_TREE[2 * v + 1].add += SEGMENT_TREE[v].add;
                SEGMENT_TREE[2 * v + 2].add += SEGMENT_TREE[v].add;
                SEGMENT_TREE[v].add = 0;
            }
        }

        private void build(final int lx, final int rx, final int v) {
            if (rx - lx == 1) {
                SEGMENT_TREE[v] = new Node(0, 1);
            } else {
                final int m = (lx + rx + 1) / 2;
                this.build(lx, m, 2 * v + 1);
                this.build(m, rx, 2 * v + 2);
                SEGMENT_TREE[v] = new Node(0, rx - lx);
            }
        }

        private static class Node {
            private long sum, add;
            private Long set;
            private final int len;

            public Node(final long s, final int l) {
                this.sum = s;
                this.add = 0;
                this.set = null;
                this.len = l;
            }
        }
    }
}
