import java.math.BigInteger;
import java.util.LinkedHashSet;
import java.util.Scanner;
import java.util.Set;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class Task16 {
    public static int N, K;
    public static long[] a, b;
    public static BigInteger countBI = BigInteger.ZERO;
    public static int p = 0;
    public static final Set<Long> ALPHABET = new LinkedHashSet<>();

    public static BigInteger getFactorialBI(final long number) {
        BigInteger factorial = BigInteger.valueOf(1);
        for (long i = 1; i <= number; i++) {
            factorial = factorial.multiply(BigInteger.valueOf(i));
        }
        return factorial;
    }

    private static void gen() {
        if (p == K) {
            System.out.println(countBI);
            return;
        }
        for (final Long item : ALPHABET) {
            if (p != 0) {
                if (item > b[p - 1]) {
                    final BigInteger tBI = (getFactorialBI(N - item)).divide((getFactorialBI(K - p - 1)).multiply(getFactorialBI(N - item - K + p + 1)));
                    if (a[p] == item) {
                        b[p++] = item;
                        ALPHABET.remove(item);
                        gen();
                        return;
                    } else {
                        countBI = countBI.add(tBI);
                    }
                }
            } else {
                final BigInteger tBI = (getFactorialBI(N - item)).divide((getFactorialBI(K - p - 1)).multiply(getFactorialBI(N - item - K + p + 1)));
                if (a[p] == item) {
                    b[p++] = item;
                    ALPHABET.remove(item);
                    gen();
                    return;
                } else {
                    countBI = countBI.add(tBI);
                }
            }
        }
    }

    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        N = in.nextInt(); K = in.nextInt();
        a = new long[K];
        b = new long[K];
        in.nextLine();
        for (int i = 0; i < K; i++) {
            a[i] = in.nextInt();
        }
        for (long i = 1; i <= N; i++) {
            ALPHABET.add(i);
        }
        gen();
    }
}
