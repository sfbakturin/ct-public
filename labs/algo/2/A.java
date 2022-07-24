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

public class A {
	FastScanner in;
	PrintWriter out;

	private void solve() throws IOException {
		final int n = in.nextInt();
		final long[] stairs = new long[n + 1];
		stairs[1] = 1;
		for (int i = 2; i < n + 1; i++) {
			stairs[i] += stairs[i - 1];
			if (i - 2 != 0) {
				stairs[i] += stairs[i - 2];
			}
		}
		out.println(stairs[n]);
	}

	private void run() {
		try {
			in = new FastScanner(new File("input.txt"));
			out = new PrintWriter("output.txt");

			solve();

			out.close();
		} catch (final IOException e) {
			e.printStackTrace();
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
		new A().run();
	}
}
