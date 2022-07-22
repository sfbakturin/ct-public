import java.util.LinkedHashSet;
import java.util.Scanner;
import java.util.Set;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class Task14 {
    public static byte[] a;
    public static int[] b;
    public static int index = 0, n = 0;
    public static long count = 0;
    public static Set<Integer> ALPHABET = new LinkedHashSet<>();

    public static long getFactorial(final long number) {
        long factorial = 1;
        for (long i = 1; i <= number; i++) {
            factorial *= i;
        }
        return factorial;
    }

    public static void gen() {
        if (index == n) {
            System.out.println(count);
            return;
        }
        for (final Integer item : ALPHABET) {
            final long t = getFactorial(n - index - 1);
            if (a[index] == item) {
                b[index++] = item;
                ALPHABET.remove(item);
                gen();
                return;
            } else {
                count += t;
            }
        }
    }

    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        n = in.nextByte();
        in.nextLine();
        a = new byte[n];
        b = new int[n];
        for (byte i = 0; i < n; i++) {
            a[i] = in.nextByte();
            ALPHABET.add(i + 1);
        }
        in.close();
        gen();
    }
}
