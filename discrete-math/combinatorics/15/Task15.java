import java.math.BigInteger;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Task15 {
	public static long n, m, k;
	public static long[] a;

	public final static Map<Long, Boolean> used = new LinkedHashMap<>();

	static {
		used.put(1L, false);
		used.put(2L, false);
		used.put(3L, false);
		used.put(4L, false);
		used.put(5L, false);
		used.put(6L, false);
		used.put(7L, false);
		used.put(8L, false);
		used.put(9L, false);
		used.put(10L, false);
		used.put(11L, false);
		used.put(12L, false);
		used.put(13L, false);
		used.put(14L, false);
		used.put(15L, false);
		used.put(16L, false);
		used.put(17L, false);
		used.put(18L, false);
		used.put(19L, false);
		used.put(20L, false);
		used.put(21L, false);
		used.put(22L, false);
		used.put(23L, false);
		used.put(24L, false);
		used.put(25L, false);
		used.put(26L, false);
		used.put(27L, false);
		used.put(28L, false);
		used.put(29L, false);
		used.put(30L, false);
	}

	public static BigInteger getFactorial(final long number) {
		BigInteger factorial = BigInteger.valueOf(1);
		for (long i = 1; i <= number; i++) {
			factorial = factorial.multiply(BigInteger.valueOf(i));
		}
		return factorial;
	}

	private static void gen(final int p) {
		if (p == m) {
			for (long l : a) {
				System.out.print(l + " ");
			}
			return;
		}
		final long start = p == 0 ? 1 : a[p - 1];
		for (long c = start; c <= n - m + p + 1; c++) {
			if (!used.get(c)) {
				final BigInteger t = (getFactorial(n - c)).divide((getFactorial(m - p - 1)).multiply(getFactorial(n - c - m + p + 1)));
				if (t.compareTo(BigInteger.valueOf(0)) > 0 && t.compareTo(BigInteger.valueOf(k)) <= 0) {
					final BigInteger temp = BigInteger.valueOf(k).subtract(t);
					k = temp.longValue();
				} else {
					used.put(c, true);
					a[p] = c;
					gen(p + 1);
					used.put(c, false);
					return;
				}
			}
		}
	}

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		n = in.nextLong();
		m = in.nextLong();
		k = in.nextLong();
		in.close();
		a = new long[(int) m];
		gen(0);
	}
}
