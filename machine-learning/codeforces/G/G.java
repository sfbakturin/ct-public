import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class G {
	private static double getRankCorrelation(final long dSum, final int n) {
		final double num = 6.0 * dSum;
		final long den = (long) n * ((long) n * n - 1);
		return 1.0 - (num / (double) den);
	}

	private static long getDiffSum(final int[] x1s,
	                               final int[] x2s,
	                               final Map<Integer, Integer> rankX1,
	                               final Map<Integer, Integer> rankX2,
	                               final int n
	) {
		long sum = 0L;
		for (int i = 0; i < n; i++) {
			final long s = rankX1.get(x1s[i]) - rankX2.get(x2s[i]);
			sum += s * s;
		}
		return sum;
	}

	private static Map<Integer, Integer> getRank(final int[] xs, final int n) {
		final Map<Integer, Integer> rank = new HashMap<>();
		final int[] rearranged = xs.clone();
		Arrays.sort(rearranged);
		for (int i = 0; i < n; i++) {
			rank.put(rearranged[i], i + 1);
		}
		return rank;
	}

	public static void main(final String[] args) {
		final Scanner input = new Scanner(System.in);
		final int n = input.nextInt();
		final int[] x1s = new int[n];
		final int[] x2s = new int[n];
		for (int i = 0; i < n; i++) {
			final int x1 = input.nextInt();
			final int x2 = input.nextInt();
			x1s[i] = x1;
			x2s[i] = x2;
		}
		input.close();
		final Map<Integer, Integer> rankX1 = getRank(x1s, n);
		final Map<Integer, Integer> rankX2 = getRank(x2s, n);
		final long dSum = getDiffSum(x1s, x2s, rankX1, rankX2, n);
		final double corr = getRankCorrelation(dSum, n);
		System.out.println(corr);
	}
}
