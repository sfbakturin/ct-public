package M;
import java.util.Arrays;
import java.util.Scanner;

import static java.lang.Math.max;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class M {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final byte n = in.nextByte();
		final int s = in.nextInt();
		in.nextLine();
		final byte[] w = new byte[n];
		final long[][] backpack = new long[n + 1][10001];
		for (int i = 0; i < n; i++) {
			w[i] = in.nextByte();
		}
		in.close();
		Arrays.sort(w);
		boolean answer = false, loop = true;
		for (int i = 0; i < n; i++) {
			if (loop) {
				for (int j = 0; j < s + 1; j++) {
					if (i > 0) {
						backpack[i][j] = backpack[i - 1][j];
					}
					if (j >= w[i]) {
						if (i > 0) {
							backpack[i][j] = max(backpack[i][j], backpack[i - 1][j - w[i]] + w[i]);
						} else {
							backpack[i][j] = w[i];
						}
					}
				}
			} else {
				break;
			}
		}
		System.out.println(backpack[n - 1][s] == s ? "YES" : "NO");
	}
}
