import java.util.*;

import static java.lang.Math.max;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class TaskE {
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
        for (int i = 0; i < m; i++) {
            final int op = in.nextInt(), x = in.nextInt(), y = in.nextInt();
            if (op == 1) {
                st.set(x, y);
            } else {
                answer.add(st.get(x, y));
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
        private final static List<Integer> ELEMENTS = new ArrayList<>();

        public SegmentTree(final int[] i) {
            final int[] array = new int[(int) Math.pow(2, Math.ceil(Math.log(i.length) / Math.log(2)))];
            SIZE = array.length;
            System.arraycopy(i, 0, array, 0, i.length);
            SEGMENT_TREE = new Node[2 * array.length - 1];
            this.build(0, SIZE, 0, array);
        }

        public void set(final int i, final int x) {
            this.set(i, x, 0, SIZE, 0);
        }

        public int get(final int x, final int index) {
            this.get(x, index, 0, SIZE, 0);
            Collections.sort(ELEMENTS);
            int v = -1;
            for (final Integer item : ELEMENTS) {
                v = max(v, item);
                if (v != -1) {
                    break;
                }
            }
            ELEMENTS.clear();
            return v;
        }

        private void get(final int x, final int index, final int lx, final int rx, final int v) {
            if (index > SEGMENT_TREE[v].right) {
                ELEMENTS.add(-1);
                return;
            }
            if (SEGMENT_TREE[v].left >= index) {
                if (rx - lx == 1) {
                    ELEMENTS.add(SEGMENT_TREE[v].max >= x ? SEGMENT_TREE[v].index : -1);
                    return;
                }
                final int m = (lx + rx + 1) / 2;
                if (x > SEGMENT_TREE[2 * v + 1].max) {
                    this.get(x, index, m, rx, 2 * v + 2);
                } else {
                    this.get(x, index, lx, m, 2 * v + 1);
                }
            } else {
                final int m = (lx + rx + 1) / 2;
                this.get(x, index, lx, m, 2 * v + 1);
                this.get(x, index, m, rx, 2 * v + 2);
            }
        }

        private void set(final int i, final int x, final int lx, final int rx, final int v) {
            if (rx - lx == 1) {
                SEGMENT_TREE[v].value = x;
                SEGMENT_TREE[v].max = x;
            } else {
                final int m = (lx + rx + 1) / 2;
                if (i < m) {
                    this.set(i, x, lx, m, 2 * v + 1);
                } else {
                    this.set(i, x, m, rx, 2 * v + 2);
                }
                SEGMENT_TREE[v].value = SEGMENT_TREE[2 * v + 1].value + SEGMENT_TREE[2 * v + 2].value;
                SEGMENT_TREE[v].max = max(SEGMENT_TREE[2 * v + 1].max, SEGMENT_TREE[2 * v + 2].max);
            }
        }

        private void build(final int lx, final int rx, final int v, final int[] a) {
            if (rx - lx == 1) {
                SEGMENT_TREE[v] = new Node(a[lx], a[lx], lx, lx, lx);
            } else {
                final int m = (lx + rx + 1) / 2;
                this.build(lx, m, 2 * v + 1, a);
                this.build(m, rx, 2 * v + 2, a);
                SEGMENT_TREE[v] = new Node(SEGMENT_TREE[2 * v + 1].value + SEGMENT_TREE[2 * v + 2].value,
                        max(SEGMENT_TREE[2 * v + 1].max, SEGMENT_TREE[2 * v + 2].max), -1, SEGMENT_TREE[2 * v + 1].left,
                        SEGMENT_TREE[2 * v + 2].right);
            }
        }

        private static class Node {
            public int value, max, index, left, right;

            public Node(final int v, final int m, final int ind, final int l, final int r) {
                this.value = v;
                this.max = m;
                this.index = ind;
                this.left = l;
                this.right = r;
            }
        }
    }
}
