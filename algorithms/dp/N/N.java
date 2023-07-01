package N;
import java.util.Arrays;
import java.util.Scanner;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.LinkedHashSet;

import static java.lang.Math.max;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class N {
	private static void find(final List<Long> answer, final int n, final int s, final long[][] backpack, final int[] weight, final boolean flag) {
		if (backpack[n][s] == 0) {
			return;
		}
		if (n - 1 == 0) {
			if (flag) {
				answer.add((long) weight[n]);
			}
			return;
		}
		if (backpack[n - 1][s] == backpack[n][s]) {
			find(answer, n - 1, s, backpack, weight, false);
		} else {
			find(answer, n - 1, s - weight[n], backpack, weight, true);
			answer.add((long) weight[n]);
		}
	}

	public static void main(final String... args) {
		final Set<Long> uniq = new LinkedHashSet<>();
		final Scanner in = new Scanner(System.in);
		final byte n = in.nextByte();
		final int s = in.nextInt();
		final int[] w = new int[n + 1];
		final long[][] backpack = new long[n + 3][10001];
		long maximumValue = Integer.MIN_VALUE;
		int maximumIndex1 = -1, maximumIndex2 = -1;
		final List<Long> answer = new ArrayList<>();
		for (int i = 1; i < n + 1; i++) {
			w[i] = in.nextInt();
		}
		for (int i = 0; i < n + 1; i++) {
			backpack[i][0] = 0;
		}
		for (int i = 0; i < s; i++) {
			backpack[0][i] = 0;
		}
		Arrays.sort(w);
		for (int i = 1; i < n + 1; i++) {
			for (int j = 1; j <= s; j++) {
				if (j >= w[i]) {
					if (i > 0) {
						backpack[i][j] = max(backpack[i - 1][j], backpack[i - 1][j - w[i]] + w[i]);
					} else {
						backpack[i][j] = w[i];
					}
				} else {
					if (i > 0) {
						backpack[i][j] = backpack[i - 1][j];
					} else {
						backpack[i][j] = w[i];
					}
				}
				uniq.add(backpack[i][j]);
			}
		}
		for (int i = 0; i < n + 2; i++) {
			for (int j = 0; j < s + 2; j++) {
				if (backpack[i][j] >= maximumValue && backpack[i][j] <= s) {
					maximumValue = backpack[i][j];
					maximumIndex2 = i;
					maximumIndex1 = j;
				}
			}
		}
		find(answer, maximumIndex2, maximumIndex1, backpack, w, true);
		System.out.println(maximumValue);
		if (uniq.size() != 2) {
			System.out.println(answer.size());
			for (final Long item : answer) {
				System.out.print(item + " ");
			}
		} else {
			System.out.println(1);
			System.out.println(maximumValue);
		}
	}
}
