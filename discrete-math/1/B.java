import java.util.Arrays;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class B {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt();
		in.nextLine();
		final int[] k = new int[n];
		final int[][] f = new int[n][];
		final int[][] f2 = new int[n][];
		final int[][] c = new int[n][5];
		for (int i = 0; i < n; i++) {
			final String[] s = in.nextLine().split(" ");
			k[i] = Integer.parseInt(s[0]);
			final String[] s1 = s[1].split("(?!^)");
			f[i] = new int[(int) Math.pow(2, k[i])];
			f2[i] = new int[(int) Math.pow(2, k[i])];
			for (int j = 0; j < ((int) Math.pow(2, k[i])); j++) {
				f[i][j] = Integer.parseInt(String.valueOf(s1[j]));
				if (f[i][j] == 1) {
					f2[i][(int) Math.pow(2, k[i]) - 1 - j] = 0;
				} else {
					f2[i][(int) Math.pow(2, k[i]) - 1 - j] = 1;
				}
			}
		}
		in.close();
		for (int i = 0; i < n; i++) {
			final int[] t = new int[(int) Math.pow(2, k[i])];
			final String[] vector = new String[(int) Math.pow(2, k[i])];
			for (int j = 0; j < (int) Math.pow(2, k[i]); j++) {
				StringBuilder s = new StringBuilder(Integer.toBinaryString(j));
				while (s.length() < k[i]) {
					s.insert(0, " ");
				}
				vector[j] = s.toString();
				int count = 0;
				for (int m = 0; m < s.length(); m++) {
					if (s.charAt(m) == '1') {
						count++;
					}
				}
				t[j] = count;
			}
			int count = -1;
			final int[][] pascal = new int[(int) Math.pow(2, k[i])][];
			int u = (int) Math.pow(2, k[i]) - 1;
			count = pascal(k[i], f, i, t, count, pascal, u);
			if (f[i][0] == 0) {
				c[i][0] = 1;
			}
			if (f[i][(int) Math.pow(2, k[i]) - 1] == 1) {
				c[i][1] = 1;
			}
			boolean flagself = true;
			for (int j = 0; j < (int) Math.pow(2, k[i]); j++) {
				if (f2[i][j] != f[i][j]) {
					flagself = false;
					break;
				}
			}
			if (flagself) {
				c[i][2] = 1;
			}
			boolean flagmono = true;
			for (int j = 0; j < (int) Math.pow(2, k[i]); j++) {
				boolean tmpf = true;
				String s1 = vector[j];
				for (int o = j - 1; o >= 0; o--) {
					String s2 = vector[o];
					boolean flag = true;
					for (int q = 0; q < s1.length(); q++) {
						if (s1.charAt(q) < s2.charAt(q)) {
							flag = false;
							break;
						}
					}
					if (!(!flag || (f[i][o] <= f[i][j]))) {
						tmpf = false;
						break;
					}
				}
				if (!tmpf) {
					flagmono = false;
					break;
				}
			}
			if (flagmono) {
				c[i][3] = 1;
			}
			if (count <= 1) {
				c[i][4] = 1;
			}
		}
		boolean flag = true;
		for (int i = 0; i < 5; i++) {
			int tmp = 1;
			for (int j = 0; j < n; j++) {
				tmp = tmp & c[j][i];
			}
			if (tmp == 1) {
				flag = false;
				break;
			}
		}
		print(flag);
	}

	private static int pascal(final int b, final int[][] f, final int i, final int[] t, int count, final int[][] pascal, int u) {
		pascal[0] = Arrays.copyOf(f[i], f[i].length);
		for (int j = 1; j < (int) Math.pow(2, b); j++) {
			pascal[j] = new int[u];
			u--;
		}
		for (int j = 1; j < (int) Math.pow(2, b); j++) {
			for (int m = 0; m < pascal[j].length; m++) {
				pascal[j][m] = pascal[j - 1][m] ^ pascal[j - 1][m + 1];
			}
		}
		for (int j = 0; j < (int) Math.pow(2, b); j++) {
			if (pascal[j][0] == 1) {
				if (t[j] > count) {
					count = t[j];
				}
			}
		}
		return count;
	}

	private static void print(final boolean flag) {
		if (flag) {
			System.out.println("YES");
		} else {
			System.out.println("NO");
		}
	}
}
