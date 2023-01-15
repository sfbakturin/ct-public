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

public class G {
	FastScanner in;
	PrintWriter out;
	private final static long MOD = (7 + (long) Math.pow(10, 6));

	private void solve() throws IOException {
		final String[] s = in.nextLine().split(" ");
		final int n = Integer.parseInt(s[0]), m = Integer.parseInt(s[1]);
		final long[][] board = new long[n + 1][m + 1];
		board[0][0] = 1;
		for (int j = 0; j < m; j++) {
			int i = 0;
			int q = j;
			while (i < n && q >= 0) {
				if ((i + 2) <= n && (q + 1) <= m) {
					board[i + 2][q + 1] = (board[i + 2][q + 1] + board[i][q]) % MOD;
				}
				if ((i + 1) <= n && (q + 2) <= m) {
					board[i + 1][q + 2] = (board[i + 1][q + 2] + board[i][q]) % MOD;
				}
				if ((i + 2) <= n && (q - 1) >= 0) {
					board[i + 2][q - 1] = (board[i + 2][q - 1] + board[i][q]) % MOD;
				}
				if ((i - 1) >= 0 && (q + 2) <= m) {
					board[i - 1][q + 2] = (board[i - 1][q + 2] + board[i][q]) % MOD;
				}
				i++;
				q--;
			}
		}
		for (int i = 1; i < n; i++) {
			int q = i;
			int j = m - 1;
			while (q < n && j >= 0) {
				if ((q + 2) <= n && (j + 1) <= m) {
					board[q + 2][j + 1] = (board[q + 2][j + 1] + board[q][j]) % MOD;
				}
				if ((q + 1) <= n && (j + 2) <= m) {
					board[q + 1][j + 2] = (board[q + 1][j + 2] + board[q][j]) % MOD;
				}
				if ((q + 2) <= n && (j - 1) >= 0) {
					board[q + 2][j - 1] = (board[q + 2][j - 1] + board[q][j]) % MOD;
				}
				if ((q - 1) >= 0 && (j + 2) <= m) {
					board[q - 1][j + 2] = (board[q - 1][j + 2] + board[q][j]) % MOD;
				}
				q++;
				j--;
			}
		}
		out.println((board[n - 1][m - 1]) % MOD);
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
		new G().run();
	}
}
