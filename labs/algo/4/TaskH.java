import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import static java.lang.Math.min;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class TaskH {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt(), m = in.nextInt();
        final SegmentTree st = new SegmentTree(n);
        final List<Integer> answer = new ArrayList<>();
        in.nextLine();
        for (int i = 0; i < m; i++) {
            final String[] input = in.nextLine().split(" ");
            if (input[0].equals("1")) {
                st.set(Integer.parseInt(input[1]), Integer.parseInt(input[2]));
            } else {
                answer.add(st.act(Integer.parseInt(input[1]), Integer.parseInt(input[2]), Integer.parseInt(input[3])));
            }
        }
        in.close();
        for (final Integer item : answer) {
            System.out.printf("%d\n", item);
        }
    }

    private static final class SegmentTree {
        private static int[] SEGMENT_TREE;
        private static int MAX = Integer.MAX_VALUE;
        private final int SIZE;

        public SegmentTree(final int size) {
            SIZE = (int) Math.pow(2, Math.ceil(Math.log(size) / Math.log(2)));
            SEGMENT_TREE = new int[2 * SIZE - 1];
            for (int i = 0; i < 2 * SIZE - 1; i++) {
                SEGMENT_TREE[i] = MAX;
            }
        }

        public void set(final int i, final int x) {
            this.set(i, x, 0, SIZE, 0);
        }

        public int act(final int l, final int r, final int x) {
            return this.act(l, r, x, 0, SIZE, 0);
        }

        private int act(final int l, final int r, final int x, final int lx, final int rx, final int v) {
            if (r <= lx || rx <= l) {
                return 0;
            }

            if (SEGMENT_TREE[v] > x) {
                return 0;
            }

            if (rx - lx == 1) {
                if (SEGMENT_TREE[v] <= x) {
                    SEGMENT_TREE[v] = MAX;
                    return 1;
                } else {
                    return 0;
                }
            }

            int sum = 0;
            final int m = (lx + rx + 1) / 2;

            if (SEGMENT_TREE[2 * v + 1] <= x) {
                sum += this.act(l, r, x, lx, m, 2 * v + 1);
            }

            if (SEGMENT_TREE[2 * v + 2] <= x) {
                sum += this.act(l, r, x, m, rx, 2 * v + 2);
            }

            this.recall(v);
            return sum;
        }

        private void set(final int i, final int x, final int lx, final int rx, final int v) {
            if (rx - lx == 1) {
                SEGMENT_TREE[v] = x;
            } else {
                final int m = (lx + rx + 1) / 2;
                if (i < m) {
                    this.set(i, x, lx, m, 2 * v + 1);
                } else {
                    this.set(i, x, m, rx, 2 * v + 2);
                }
                this.recall(v);
            }
        }

        private void recall(final int v) {
            SEGMENT_TREE[v] = min(SEGMENT_TREE[2 * v + 1], SEGMENT_TREE[2 * v + 2]);
        }
    }
}
