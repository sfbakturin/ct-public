import java.util.Arrays;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class Task24 {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int N = in.nextInt();
        in.nextLine();
        final int[] a = new int[N];
        for (int i = 0; i < N; i++) {
            a[i] = in.nextInt();
        }
        in.close();
        final String PREV = getPrev(a);
        final String NEXT = getNext(a);
        System.out.println(PREV);
        System.out.println(NEXT);
    }

    private static String getNext(final int[] a) {
        final StringBuilder sb = new StringBuilder();
        boolean flag = false;
        int errorIndex = -1, errorValue = -1, errorMinValue = Integer.MAX_VALUE, errorMinIndex = -1;
        for (int i = a.length - 2; i >= 0; i--) {
            if (a[i] < a[i + 1]) {
                errorIndex = i;
                errorValue = a[i];
                flag = true;
                break;
            }
        }
        if (flag) {
            for (int i = errorIndex + 1; i < a.length; i++) {
                if (errorMinValue > a[i] && a[i] > errorValue) {
                    errorMinValue = a[i];
                    errorMinIndex = i;
                }
            }
            final int t = a[errorMinIndex];
            a[errorMinIndex] = a[errorIndex];
            a[errorIndex] = t;
            for (int i = 0; i <= errorIndex; i++) {
                sb.append(a[i]).append(' ');
            }
            for (int i = a.length - 1; i >= errorIndex + 1; i--) {
                sb.append(a[i]).append(' ');
            }
        } else {
            sb.append("0 ".repeat(a.length));
        }
        return sb.toString();
    }

    private static String getPrev(final int[] a) {
        final int[] copy = Arrays.copyOf(a, a.length);
        final StringBuilder sb = new StringBuilder();
        boolean flag = false;
        int errorIndex = -1, errorValue = -1, errorMinValue = -1, errorMinIndex = -1;
        for (int i = copy.length - 2; i >= 0; i--) {
            if (copy[i] > copy[i + 1]) {
                errorIndex = i;
                errorValue = copy[i];
                flag = true;
                break;
            }
        }
        if (flag) {
            for (int i = errorIndex + 1; i < copy.length; i++) {
                if (errorMinValue < copy[i] && copy[i] < errorValue) {
                    errorMinValue = copy[i];
                    errorMinIndex = i;
                }
            }
            final int t = copy[errorMinIndex];
            copy[errorMinIndex] = copy[errorIndex];
            copy[errorIndex] = t;
            for (int i = 0; i <= errorIndex; i++) {
                sb.append(copy[i]).append(' ');
            }
            for (int i = copy.length - 1; i >= errorIndex + 1; i--) {
                sb.append(copy[i]).append(' ');
            }
        } else {
            sb.append("0 ".repeat(copy.length));
        }
        return sb.toString();
    }
}
