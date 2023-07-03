import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class G {
	private final static String I = "equivalence.in";
	private final static String O = "equivalence.out";
	private final static String REGEX = "\u0020";
	private static boolean FLAG = true;
	private static boolean[] TERMINAL_1;
	private static boolean[] TERMINAL_2;
	private static boolean[][] CHECKED;

	public static void main(final String... args) {
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(I), StandardCharsets.UTF_8));
			String[] nmk = in.readLine().split(REGEX);
			final int n1 = Integer.parseInt(nmk[0]);
			final int m1 = Integer.parseInt(nmk[1]);
			TERMINAL_1 = new boolean[n1 + 1];
			String[] accepts = in.readLine().split(REGEX);
			for (final String item : accepts) {
				TERMINAL_1[Integer.parseInt(item)] = true;
			}
			final Set<Character> alphabet = new HashSet<>();
			for (int i = 0; i < m1; i++) {
				final String[] s = in.readLine().split(REGEX);
				alphabet.add(s[2].charAt(0));
			}
			nmk = in.readLine().split(REGEX);
			final int n2 = Integer.parseInt(nmk[0]);
			final int m2 = Integer.parseInt(nmk[1]);
			TERMINAL_2 = new boolean[n2 + 1];
			accepts = in.readLine().split(REGEX);
			for (final String item : accepts) {
				TERMINAL_2[Integer.parseInt(item)] = true;
			}
			for (int i = 0; i < m2; i++) {
				final String[] s = in.readLine().split(REGEX);
				alphabet.add(s[2].charAt(0));
			}
			in.close();
			final DFA dfa = new DFA();
			CHECKED = new boolean[n1 + 1][n2 + 1];
			in = new BufferedReader(new InputStreamReader(new FileInputStream(I), StandardCharsets.UTF_8));
			in.readLine();
			in.readLine();
			for (int i = 0; i < m1; i++) {
				final String[] s = in.readLine().split(REGEX);
				dfa.add(Integer.parseInt(s[0]), Integer.parseInt(s[1]), s[2].charAt(0), true);
			}
			in.readLine();
			in.readLine();
			for (int i = 0; i < m2; i++) {
				final String[] s = in.readLine().split(REGEX);
				dfa.add(Integer.parseInt(s[0]), Integer.parseInt(s[1]), s[2].charAt(0), false);
			}
			in.close();
			dfa.check(alphabet);
			final BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(O), StandardCharsets.UTF_8));
			if (FLAG) {
				out.write("YES");
			} else {
				out.write("NO");
			}
			out.flush();
			out.close();
		} catch (final Exception e) {
			throw new RuntimeException(e);
		}
	}

	private final static class DFA {
		private final static int s = 1;
		private final static Map<Integer, Map<Character, Integer>> TRANSITION_1 = new HashMap<>();
		private final static Map<Integer, Map<Character, Integer>> TRANSITION_2 = new HashMap<>();

		public DFA() {
		}

		public void add(final int s, final int e, final char c, final boolean flag) {
			if (flag) {
				final Map<Character, Integer> temp;
				if (TRANSITION_1.containsKey(s)) {
					temp = TRANSITION_1.get(s);
				} else {
					temp = new HashMap<>();
				}
				temp.put(c, e);
				TRANSITION_1.put(s, temp);
			} else {
				final Map<Character, Integer> temp;
				if (TRANSITION_2.containsKey(s)) {
					temp = TRANSITION_2.get(s);
				} else {
					temp = new HashMap<>();
				}
				temp.put(c, e);
				TRANSITION_2.put(s, temp);
			}
		}

		public void check(final Set<Character> map) {
			this.check(map, s, s);
		}

		private void check(final Set<Character> map, final Integer a, final Integer b) {
			if (TERMINAL_1[a] && !TERMINAL_2[b]) {
				FLAG = false;
				return;
			}
			if (!TERMINAL_1[a] && TERMINAL_2[b]) {
				FLAG = false;
				return;
			}
			CHECKED[a][b] = true;
			for (final Character item : map) {
				Integer trans1, trans2;
				if (TRANSITION_1.containsKey(a) && TRANSITION_1.get(a).containsKey(item)) {
					trans1 = TRANSITION_1.get(a).get(item);
				} else {
					trans1 = 0;
				}
				if (TRANSITION_2.containsKey(b) && TRANSITION_2.get(b).containsKey(item)) {
					trans2 = TRANSITION_2.get(b).get(item);
				} else {
					trans2 = 0;
				}
				if (!CHECKED[trans1][trans2]) {
					check(map, trans1, trans2);
				}
			}
		}
	}
}
