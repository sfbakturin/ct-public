import java.util.*;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class TaskM {
    private final static Map<Type, Integer> priority = new LinkedHashMap<>();

    static {
        priority.put(Type.START, 1);
        priority.put(Type.DOT, 2);
        priority.put(Type.END, 3);
    }

    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt(), m = in.nextInt();
        final List<Dot> list = new ArrayList<>();
        final int[] answer = new int[m];
        final Set<Integer> indexes = new HashSet<>();
        for (int i = 0; i < n; i++) {
            final int a = in.nextInt(), b = in.nextInt();
            if (a <= b) {
                list.add(new Dot(a, Type.START, i));
                list.add(new Dot(b, Type.END, i));
            } else {
                list.add(new Dot(b, Type.START, i));
                list.add(new Dot(a, Type.END, i));
            }
        }
        for (int i = 0; i < m; i++) {
            list.add(new Dot(in.nextInt(), Type.DOT, i));
        }
        in.close();
        list.sort(new Comparator<>() {
            @Override
            public int compare(final Dot o1, final Dot o2) {
                final int ans = Integer.compare(o1.value, o2.value);
                if (ans != 0) {
                    return ans;
                }
                return Integer.compare(priority.get(o1.type), priority.get(o2.type));
            }
        });
        for (final Dot item : list) {
            switch (item.type) {
                case START -> indexes.add(item.index);
                case END -> indexes.remove(item.index);
                case DOT -> answer[item.index] = indexes.size();
            }
        }
        for (final Integer item : answer) {
            System.out.print(item + " ");
        }
    }

    private static class Dot {
        private final int value, index;
        private final Type type;

        public Dot(final int v, final Type t, final int i) {
            this.value = v;
            this.type = t;
            this.index = i;
        }
    }

    private enum Type {
        START, END, DOT
    }
}
