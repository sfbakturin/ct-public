package L;
import java.util.Scanner;

import static java.lang.Math.max;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class L {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int LENGTH = in.nextInt();
		final int START = in.nextInt();
		final int K = in.nextInt();
		final int B = in.nextInt();
		final int M = in.nextInt();
		in.close();
		final int[] a = new int[LENGTH], dp = new int[LENGTH + 1];
		a[0] = START;
		dp[0] = Integer.MIN_VALUE;
		int answer = Integer.MIN_VALUE;
		for (int i = 1; i < LENGTH; i++) {
			a[i] = (K * a[i - 1] + B) % M;
		}
		for (int i = 1; i < LENGTH + 1; i++) {
			dp[i] = Integer.MAX_VALUE;
		}
		for (int i = 0; i < LENGTH; i++) {
			int l = 0, r = LENGTH;
			while (l < r - 1) {
				final int m = (l + r) / 2;
				if (dp[m] >= a[i]) {
					r = m;
				} else {
					l = m;
				}
			}
			if (dp[r - 1] < a[i] && a[i] < dp[r]) {
				dp[r] = a[i];
				answer = max(answer, r);
			}
		}
		System.out.println(answer);
	}
}
