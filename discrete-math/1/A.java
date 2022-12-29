import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class A {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt();
		in.nextLine();
		final int[][] o1 = new int[n][n];
		final int[][] o2 = new int[n][n];
		final int[][] o3 = new int[n][n];
		input(in, n, o1, o2);
		in.close();
		final int[] ans1 = new int[5];
		final int[] ans2 = new int[5];
		ref(n, o1, o2, ans1, ans2);
		simm(n, o1, o2, ans1, ans2);
		trans(n, o1, o2, ans1, ans2);
		rel(n, o1, o2, o3);
		print(n, o3, ans1, ans2);
	}

	private static void rel(final int n, final int[][] o1, final int[][] o2, final int[][] o3) {
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				for (int k = 0; k < n; k++) {
					o3[i][j] = o3[i][j] | (o1[i][k] & o2[k][j]);
				}
			}
		}
	}

	private static void trans(final int n, final int[][] o1, final int[][] o2, final int[] ans1, final int[] ans2) {
		boolean Trans1 = true, Trans2 = true;
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				for (int k = 0; k < n; k++) {
					if (!(((o1[i][k] == 0) || (o1[k][j] == 0)) || (o1[i][j] == 1))) {
						Trans1 = false;
						break;
					}
				}
			}
		}
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				for (int k = 0; k < n; k++) {
					if (!(((o2[i][k] == 0) || (o2[k][j] == 0)) || (o2[i][j] == 1))) {
						Trans2 = false;
						break;
					}
				}
			}
		}
		if (Trans1) {
			ans1[4] = 1;
		}
		if (Trans2) {
			ans2[4] = 1;
		}
	}

	private static void simm(final int n, final int[][] o1, final int[][] o2, final int[] ans1, final int[] ans2) {
		boolean Simm1 = true, Simm2 = true, AntiSimm1 = true, AntiSimm2 = true;
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				if (i < j) {
					if (!((o1[i][j] == 0) || (o1[j][i] == 1))) {
						Simm1 = false;
						break;
					}
				}
			}
		}
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				if (i < j) {
					if (!((o2[i][j] == 0) || (o2[j][i] == 1))) {
						Simm2 = false;
						break;
					}
				}
			}
		}
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				if (i < j) {
					if (o1[i][j] == 1 && o1[j][i] == 1) {
						AntiSimm1 = false;
						break;
					}
				}
			}
		}
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				if (i < j) {
					if (o2[i][j] == 1 && o2[j][i] == 1) {
						AntiSimm2 = false;
						break;
					}
				}
			}
		}
		if (Simm1) {
			ans1[2] = 1;
		}
		if (Simm2) {
			ans2[2] = 1;
		}
		if (AntiSimm1) {
			ans1[3] = 1;
		}
		if (AntiSimm2) {
			ans2[3] = 1;
		}
	}

	private static void ref(final int n, final int[][] o1, final int[][] o2, final int[] ans1, final int[] ans2) {
		boolean Ref1 = true, Ref2 = true;
		int kRef1 = 0, kRef2 = 0;
		for (int i = 0; i < n; i++) {
			if (o1[i][i] == 1) {
				kRef1++;
			} else {
				Ref1 = false;
			}
		}
		for (int i = 0; i < n; i++) {
			if (o2[i][i] == 1) {
				kRef2++;
			} else {
				Ref2 = false;
			}
		}
		if (Ref1) {
			ans1[0] = 1;
		} else {
			if (kRef1 == 0) {
				ans1[1] = 1;
			}
		}
		if (Ref2) {
			ans2[0] = 1;
		} else {
			if (kRef2 == 0) {
				ans2[1] = 1;
			}
		}
	}

	private static void input(final Scanner in, final int n, final int[][] o1, final int[][] o2) {
		for (int i = 0; i < n; i++) {
			final String[] s = in.nextLine().split(" ");
			for (int j = 0; j < n; j++) {
				o1[i][j] = Integer.parseInt(s[j]);
			}
		}
		for (int i = 0; i < n; i++) {
			final String[] s = in.nextLine().split(" ");
			for (int j = 0; j < n; j++) {
				o2[i][j] = Integer.parseInt(s[j]);
			}
		}
	}

	private static void print(final int n, final int[][] o3, final int[] ans1, final int[] ans2) {
		for (int i = 0; i < 5; i++) {
			System.out.print(ans1[i] + " ");
		}
		System.out.println();
		for (int i = 0; i < 5; i++) {
			System.out.print(ans2[i] + " ");
		}
		System.out.println();
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				System.out.print(o3[i][j] + " ");
			}
			System.out.println();
		}
	}
}
