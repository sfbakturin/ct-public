import java.util.ArrayList;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class F {
    public static void insert(final ArrayList<Long> heap, final long x) {
        heap.add(x);
        int i = heap.size() - 1;
        while (heap.get(i) > heap.get((i - 1) / 2)) {
            long t = heap.get(i);
            heap.set(i, heap.get((i - 1) / 2));
            heap.set((i - 1) / 2, t);
            i = (i - 1) / 2;
        }
    }

    public static long extract(final ArrayList<Long> heap) {
        final long extracted = heap.get(0);
        heap.set(0, heap.get(heap.size() - 1));
        int index = 0;
        while (2 * index + 1 < heap.size()) {
            int left = 2 * index + 1;
            int right = 2 * index + 2;
            int j = left;
            if (right < heap.size() && heap.get(right) > heap.get(left)) {
                j = right;
            }
            if (heap.get(index) >= heap.get(j)) {
                break;
            }
            long t = heap.get(index);
            heap.set(index, heap.get(j));
            heap.set(j, t);
            index = j;
        }
        heap.remove(heap.size() - 1);
        return extracted;
    }

    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt();
        in.nextLine();
        final ArrayList<Long> heap = new ArrayList<>();
        final ArrayList<Long> answer = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            final String[] s = in.nextLine().split(" ");
            switch (s[0]) {
                case "0":
                    insert(heap, Long.parseLong(s[1]));
                    break;
                case "1":
                    answer.add(extract(heap));
                    break;
            }
        }
        in.close();
        for (final Long integer : answer) {
            System.out.println(integer);
        }
    }
}
