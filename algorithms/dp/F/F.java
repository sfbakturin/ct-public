import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.StringTokenizer;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class F {
	FastScanner in;
	PrintWriter out;

	private void solve() throws IOException {
		final String[] s = in.nextLine().split(" ");
		final int n = Integer.parseInt(s[0]), m = Integer.parseInt(s[1]);
		final int[][] board = new int[2 * n][2 * m];
		board[n][m] = 1;
		for (int i = n; i < 2 * n; i++) {
			for (int j = m; j < 2 * m; j++) {
				if (i != n && j != m) {
					board[i][j] = board[i - 2][j - 1] + board[i - 1][j - 2];
				}
			}
		}
		out.println(board[2 * n - 1][2 * m - 1]);
	}

	private void run() {
		try {
			in = new FastScanner(new File("knight.in"));
			out = new PrintWriter("knight.out");

			solve();

			out.close();
		} catch (final IOException err) {
			err.printStackTrace();
		}
	}

	private static class FastScanner {
		BufferedReader br;
		StringTokenizer st;
		String s;

		private FastScanner(final File f) {
			try {
				br = new BufferedReader(new FileReader(f));
			} catch (final FileNotFoundException err) {
				err.printStackTrace();
			}
		}

		private String next() {
			while (st == null || !st.hasMoreTokens()) {
				try {
					st = new StringTokenizer(br.readLine());
				} catch (final IOException err) {
					err.printStackTrace();
				}
			}
			return st.nextToken();
		}

		private String nextLine() {
			try {
				s = br.readLine();
			} catch (final IOException err) {
				err.printStackTrace();
			}
			return s;
		}

		private int nextInt() {
			return Integer.parseInt(next());
		}
	}

	public static void main(final String... args) {
		new F().run();
	}
}
