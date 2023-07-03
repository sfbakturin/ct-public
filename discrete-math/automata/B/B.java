import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class B {
	private final static String I = "problem2.in";
	private final static String O = "problem2.out";
	private final static String REGEX = "\u0020";

	public static void main(final String... args) {
		try {
			final BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(I), StandardCharsets.UTF_8));
			final String word = in.readLine();
			final Map<Character, Integer> map = new LinkedHashMap<>();
			int index = 1;
			for (int i = 0; i < word.length(); i++) {
				if (!map.containsKey(word.charAt(i))) {
					map.put(word.charAt(i), index++);
				}
			}
			final String[] nmk = in.readLine().split(REGEX);
			final int n = Integer.parseInt(nmk[0]);
			final int m = Integer.parseInt(nmk[1]);
			final int k = Integer.parseInt(nmk[2]);
			final NFA nfa = new NFA(index + 1, index + n + 1);
			final String[] accepts = in.readLine().split(REGEX);
			for (int i = 0; i < k; i++) {
				nfa.add(Integer.parseInt(accepts[i]));
			}
			for (int i = 0; i < m; i++) {
				final String[] s = in.readLine().split(REGEX);
				if (map.containsKey(s[2].charAt(0))) {
					nfa.add(Integer.parseInt(s[0]), Integer.parseInt(s[1]), map.get(s[2].charAt(0)));
				}
			}
			in.close();
			final BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(O), StandardCharsets.UTF_8));
			final int[] w = new int[word.length()];
			for (int i = 0; i < word.length(); i++) {
				w[i] = map.get(word.charAt(i));
			}
			if (nfa.accept(w)) {
				out.write("Accepts");
			} else {
				out.write("Rejects");
			}
			out.flush();
			out.close();
		} catch (final Exception e) {
			throw new RuntimeException(e);
		}
	}

	private final static class NFA {
		private final static int s = 1;
		private final static Set<Integer> TERMINAL = new HashSet<>();
		private static final List<List<Set<Integer>>> DELTA = new ArrayList<>();

		public NFA(final int x, final int y) {
			for (int i = 0; i < x + 1; i++) {
				for (int j = 0; j < y + 1; j++) {
					DELTA.add(new ArrayList<>());
					DELTA.get(j).add(new HashSet<>());
				}
			}
		}

		public void add(final int s, final int e, final int c) {
			DELTA.get(s).get(c).add(e);
		}

		public void add(final int t) {
			TERMINAL.add(t);
		}

		public boolean accept(final int[] w) {
			final boolean[][] can = new boolean[w.length + 1][DELTA.size() + 1];
			can[0][s] = true;
			for (int i = 0; i < w.length; i++) {
				for (int q = 0; q < DELTA.size(); q++) {
					if (can[i][q]) {
						for (final Integer item : DELTA.get(q).get(w[i])) {
							can[i + 1][item] = true;
						}
					}
				}
			}
			for (int q = 0; q < DELTA.size(); q++) {
				if (can[w.length][q] && TERMINAL.contains(q)) {
					return true;
				}
			}
			return false;
		}
	}
}
