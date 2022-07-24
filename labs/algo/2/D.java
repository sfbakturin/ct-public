import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class D {
	FastScanner in;
	PrintWriter out;

	private void solve() throws IOException {
		final int n = in.nextInt(), m = in.nextInt();
		final long[][] board = new long[n][m];
		for (int i = 0; i < n; i++) {
			final String[] s = in.nextLine().split(" ");
			for (int j = 0; j < m; j++) {
				board[i][j] = Long.parseLong(s[j]);
			}
		}
		final long[][] dp = new long[n][m];
		dp[0][0] = board[0][0];
		for (int i = 1; i < n; i++) {
			dp[i][0] = dp[i - 1][0] + board[i][0];
		}
		for (int i = 1; i < m; i++) {
			dp[0][i] = dp[0][i - 1] + board[0][i];
		}
		for (int i = 1; i < n; i++) {
			for (int j = 1; j < m; j++) {
				dp[i][j] = board[i][j] + Math.max(dp[i][j - 1], dp[i - 1][j]);
			}
		}
		int i = n - 1, j = m - 1;
		final List<String> answer = new ArrayList<>();
		while (i != 0 || j != 0) {
			if (i != 0) {
				if (dp[i - 1][j] == dp[i][j] - board[i][j]) {
					i--;
					answer.add("D");
				} else {
					if (j != 0) {
						j--;
						answer.add("R");
					} else {
						answer.add("D");
					}
				}
			} else {
				j--;
				answer.add("R");
			}
		}
		final StringBuilder sb = new StringBuilder();
		for (int q = answer.size() - 1; q >= 0; q--) {
			sb.append(answer.get(q));
		}
		out.println(dp[n - 1][m - 1]);
		out.println(sb);
	}

	private void run() {
		try {
			in = new FastScanner(new File("input.txt"));
			out = new PrintWriter("output.txt");

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
		new D().run();
	}
}
