import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class A {
    private static int[] merge(final int[] a, final int[] b) {
        final int[] ans = new int[a.length + b.length];
        int i = 0, j = 0;
        while (i < a.length && j < b.length) {
            if (a[i] <= b[j]) {
                ans[i + j] = a[i];
                i++;
            } else {
                ans[i + j] = b[j];
                j++;
            }
        }
        while (i < a.length) {
            ans[i + j] = a[i++];
        }
        while (j < b.length) {
            ans[i + j] = b[j++];
        }
        return ans;
    }

    private static int[] mergesort(final int[] s) {
        if (s.length == 1) {
            return s;
        }
        int[] a = new int[s.length / 2];
        int[] b = new int[s.length - s.length / 2];
        System.arraycopy(s, 0, a, 0, s.length / 2);
        System.arraycopy(s, s.length / 2, b, 0, s.length - s.length / 2);
        a = mergesort(a);
        b = mergesort(b);
        return merge(a, b);
    }

    public static void main(String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt();
        in.nextLine();
        final int[] a = new int[n];
        for (int i = 0; i < n; i++) {
            a[i] = in.nextInt();
        }
        in.close();
        final int[] sorted = mergesort(a);
        for(final Integer integer : sorted) {
            System.out.print(integer + " ");
        }
    }
}
