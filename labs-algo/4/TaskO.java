import java.util.*;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class TaskO {
    private final static Map<Type, Integer> PRIORITY;

    static {
        PRIORITY = new LinkedHashMap<>();
        PRIORITY.put(Type.START, 0);
        PRIORITY.put(Type.END, 1);
    }

    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int M = in.nextInt(), N = in.nextInt();
        final List<Dot> list = new ArrayList<>();
        final Map<Integer, Dot> hash = new LinkedHashMap<>();
        for (int i = 0; i < N; i++) {
            final int a = in.nextInt(), b = in.nextInt();
            list.add(new Dot(a, i, Type.START, null));
            list.add(new Dot(b, i, Type.END, list.get(list.size() - 1)));
            hash.put(i, list.get(list.size() - 1));
            final Dot edit = list.get(list.size() - 2);
            edit.link = list.get(list.size() - 1);
            list.set(list.size() - 2, edit);
        }
        in.close();
        list.sort(new CustomComparator());
        final PriorityQueue<Integer> maximums = new PriorityQueue<>(Collections.reverseOrder());
        final Set<Integer> indexes = new HashSet<>();
        for (final Dot item : list) {
            if (maximums.isEmpty()) {
                maximums.add(item.queue);
                indexes.add(item.queue);
            } else {
                if (!indexes.contains(item.queue)) {
                    final int m = maximums.poll();
                    if (m > item.queue) {
                        maximums.add(m);
                        maximums.add(item.queue);
                        item.type = Type.NONE;
                        item.link.type = Type.NONE;
                        indexes.add(item.queue);
                    } else {
                        maximums.add(m);
                        maximums.add(item.queue);
                        indexes.add(item.queue);
                        hash.get(m).type = Type.NONE;
                        hash.get(m).link.type = Type.NONE;
                    }
                } else {
                    indexes.remove(item.queue);
                    maximums.remove(item.queue);
                }
            }
        }
        int count = 0;
        for (final Dot item : list) {
            count += item.type != Type.NONE ? 1 : 0;
        }
        System.out.println(count / 2);
    }

    private static class Dot {
        private final int position, queue;
        private Type type;
        private Dot link;

        public Dot(final int p, final int q, final Type t, final Dot l) {
            this.position = p;
            this.queue = q;
            this.type = t;
            this.link = l;
        }

    }

    private enum Type {
        START, END, NONE
    }

    private static class CustomComparator implements Comparator<Dot> {
        public int compare(final Dot x, final Dot y) {
            final int result = Integer.compare(x.position, y.position);

            if (result != 0) {
                return result;
            }

            return Long.compare(PRIORITY.get(x.type), PRIORITY.get(y.type));
        }
    }
}
