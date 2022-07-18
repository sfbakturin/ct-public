import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class M {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt(), k = in.nextInt();
        in.nextLine();
        final int[] a = new int[n];
        final int[] b = new int[k];
        for (int i = 0; i < n; i++) {
            a[i] = in.nextInt();
        }
        in.nextLine();
        for (int i = 0; i < k; i++) {
            b[i] = in.nextInt();
        }
        in.close();
        final List<Integer> answer = new ArrayList<>();
        for (int i = 0; i < k; i++) {
            int l, r, o, find;
            l = 0;
            r = n - 1;
            o = l;
            find = b[i];
            while (l < r) {
                int m = (l + r) / 2;
                if (a[m] < find) {
                    l = m + 1;
                    o = l;
                } else {
                    r = m;
                }
            }
            if (l != 0 && Math.abs(a[o - 1] - find) <= Math.abs(a[o] - find)) {
                answer.add(a[o - 1]);
            } else {
                answer.add(a[o]);
            }
        }
        for (final Integer integer : answer) {
            System.out.println(integer);
        }
    }
}
