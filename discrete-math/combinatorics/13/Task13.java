import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Task13 {
	public static long k;
	public static int n;
	public final static Map<Integer, Boolean> used = new LinkedHashMap<>();

	static {
		used.put(1, false);
		used.put(2, false);
		used.put(3, false);
		used.put(4, false);
		used.put(5, false);
		used.put(6, false);
		used.put(7, false);
		used.put(8, false);
		used.put(9, false);
		used.put(10, false);
		used.put(11, false);
		used.put(12, false);
		used.put(13, false);
		used.put(14, false);
		used.put(15, false);
		used.put(16, false);
		used.put(17, false);
		used.put(18, false);
	}

	public static long getFactorial(final long number) {
		long factorial = 1;
		for (long i = 2; i <= number; i++) {
			factorial *= i;
		}
		return factorial;
	}

	private static void gen(final int p, final String[] a) {
		if (p == n) {
			for (int i = 0; i < n; i++) {
				System.out.print(a[i] + " ");
			}
			return;
		} else {
			for (int i = 1; i <= n; i++) {
				if (!used.get(i)) {
					final long o = getFactorial(n - p - 1);
					if (k >= o) {
						k -= o;
					} else {
						used.put(i, true);
						a[p] = String.valueOf(i);
						gen(p + 1, a);
						used.put(i, false);
						return;
					}
				}
			}
		}
	}

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		n = in.nextByte();
		k = in.nextLong();
		in.close();
		final String[] a = new String[n];
		gen(0, a);
	}
}
