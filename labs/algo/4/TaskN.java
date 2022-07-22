import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class TaskN {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt();
        in.nextLine();
        final List<Dot> list = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            final int h0 = in.nextInt(), m0 = in.nextInt(), s0 = in.nextInt(), h1 = in.nextInt(), m1 = in.nextInt(),
                    s1 = in.nextInt();
            final int c0 = h0 * 60 * 60 + m0 * 60 + s0;
            final int c1 = h1 * 60 * 60 + m1 * 60 + s1;
            if (c0 < c1) {
                list.add(new Dot(c0, Type.START));
                list.add(new Dot(c1, Type.END));
                continue;
            }
            list.add(new Dot(0, Type.START));
            list.add(new Dot(c1, Type.END));
            list.add(new Dot(c0, Type.START));
            list.add(new Dot(86400, Type.END));
        }
        in.close();
        list.sort(new DotSortComparator());
        int sum = 0, start = 0, balance = 0;
        boolean flag = false;
        for (final Dot item : list) {
            switch (item.type) {
                case START: {
                    balance++;
                    if (balance == n) {
                        start = item.value;
                        flag = true;
                    }
                    break;
                }
                case END: {
                    balance--;
                    if (flag) {
                        flag = false;
                        sum += (item.value - start);
                        start = 0;
                    }
                    break;
                }
            }
        }
        System.out.println(sum);
    }

    private static class Dot {
        private final int value;
        private final Type type;

        public Dot(final int v, final Type t) {
            this.value = v;
            this.type = t;
        }
    }

    private enum Type {
        START, END;
    }

    private static class DotSortComparator implements Comparator<Dot> {
        public int compare(final Dot x, final Dot y) {
            return Integer.compare(x.value, y.value);
        }
    }
}
