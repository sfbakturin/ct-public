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

public class B {
	FastScanner in;
	PrintWriter out;

	private void solve() throws IOException {
		String[] s = in.nextLine().split(" ");
		final int n = Integer.parseInt(s[0]), k = Integer.parseInt(s[1]);
		s = in.nextLine().split(" ");
		final int[] input = new int[n + 1];
		for (int i = 2; i < n; i++) {
			input[i] = Integer.parseInt(s[i - 2]);
		}
		final int[] stairs = new int[n + 1];
		final int[] indexes = new int[n + 1];
		final List<Integer> path = new ArrayList<>();
		stairs[1] = 0;
		for (int i = 2; i <= n; i++) {
			int local = Integer.MIN_VALUE, index = -1;
			int counter = 0, j = i - 1;
			while (counter < k && j > 0) {
				if (stairs[j] > local) {
					local = stairs[j];
					index = j;
				}
				counter++;
				j--;
			}
			indexes[i] = index;
			stairs[i] = input[i] + local;
		}
		int current = n;
		int count = 0;
		path.add(current);
		while (current > 1) {
			count++;
			current = indexes[current];
			path.add(current);
		}
		out.println(stairs[n]);
		out.println(count);
		final StringBuilder pathAnswer = new StringBuilder();
		for (int i = path.size() - 1; i >= 0; i--) {
			pathAnswer.append(path.get(i)).append(' ');
		}
		out.println(pathAnswer);
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
			} catch (final FileNotFoundException e) {
				e.printStackTrace();
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
			} catch (final IOException e) {
				e.printStackTrace();
			}
			return s;
		}

		private int nextInt() {
			return Integer.parseInt(next());
		}
	}

	public static void main(final String... args) {
		new B().run();
	}
}
