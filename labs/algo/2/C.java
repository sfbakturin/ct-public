import java.io.*;
import java.util.StringTokenizer;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class C {
	FastScanner in;
	PrintWriter out;

	private void solve() throws IOException {
		final int n = in.nextInt();
		final long[][] dp = new long[n + 1][3];
		dp[1][0] = 1;
		dp[1][1] = 1;
		dp[1][2] = 0;
		for (int i = 2; i < n + 1; i++) {
			for (int j = 0; j < 3; j++) {
				if (j == 0) {
					dp[i][j] = dp[i - 1][0] + dp[i - 1][1];
				}
				if (j > 0) {
					dp[i][j] = dp[i - 1][j - 1];
				}
			}
		}
		out.println(dp[n][0] + dp[n][1]);
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

		private int nextInt() {
			return Integer.parseInt(next());
		}
	}

	public static void main(final String... args) {
		new C().run();
	}
}
