import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class TaskK {
	private static int nextU(final int u, final int r, final int i, final int n) {
		return ((17 * u + 751 + r + 2 * i) % n) + 1;
	}

	private static int nextV(final int v, final int r, final int i, final int n) {
		return ((13 * v + 593 + r + 5 * i) % n) + 1;
	}

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);

		final int n = in.nextInt(), m = in.nextInt(), start = in.nextInt();

		final SparseTable sp = new SparseTable(n);
		sp.fill(0, start);
		for (int i = 1; i < n; i++) {
			sp.fill(i, (23 * sp.get(i - 1) + 21563) % 16714589);
		}
		sp.generate();

		in.nextLine();

		int u = in.nextInt(), v = in.nextInt();

		in.close();

		int answer;
		if (u < v) {
			answer = sp.get(u - 1, v - 1);
		} else {
			answer = sp.get(v - 1, u - 1);
		}

		for (int i = 1; i < m; i++) {
			u = nextU(u, answer, i, n);
			v = nextV(v, answer, i, n);
			if (u < v) {
				answer = sp.get(u - 1, v - 1);
			} else {
				answer = sp.get(v - 1, u - 1);
			}
		}

		System.out.println(u + " " + v + " " + answer);
	}

	private static class SparseTable {
		private static int[][] SPARSE_TABLE;
		private final int SIZE_LOG, SIZE;

		public SparseTable(final int n) {
			SIZE_LOG = (int) (Math.log(n) / Math.log(2));
			SIZE = n;
			SPARSE_TABLE = new int[n + 1][SIZE_LOG + 1];
		}

		public void fill(final int index, final int element) {
			SPARSE_TABLE[index][0] = element;
		}

		public int get(final int index) {
			return SPARSE_TABLE[index][0];
		}

		public void generate() {
			for (int j = 1; j <= SIZE_LOG; j++) {
				for (int i = 0; i <= SIZE - (int) Math.pow(2, j); i++) {
					SPARSE_TABLE[i][j] = Math.min(SPARSE_TABLE[i][j - 1],
							SPARSE_TABLE[i + (int) Math.pow(2, (j - 1))][j - 1]);
				}
			}
		}

		public int get(final int l, final int r) {
			final int d = (int) (Math.log(r - l + 1) / Math.log(2));
			return Math.min(SPARSE_TABLE[l][d], SPARSE_TABLE[r - (int) Math.pow(2, d) + 1][d]);
		}
	}
}
