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

public class C {
	private final static String I = "problem3.in";
	private final static String O = "problem3.out";
	private final static String REGEX = "\u0020";
	private final static int MOD = 1000000000 + 7;
	private static boolean FLAG = true;
	private static boolean[] LOOPED;
	private static boolean[] COUNTER;
	private static boolean[] CAN;
	private static boolean[] CHECKED;

	public static void main(final String... args) {
		try {
			final BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(I), StandardCharsets.UTF_8));
			final String[] nmk = in.readLine().split(REGEX);
			final int n = Integer.parseInt(nmk[0]);
			final int m = Integer.parseInt(nmk[1]);
			LOOPED = new boolean[n + 1];
			COUNTER = new boolean[n + 1];
			CAN = new boolean[n + 1];
			CHECKED = new boolean[n + 1];
			final String[] accepts = in.readLine().split(REGEX);
			final Node[] dfa = new Node[n + 1];
			for (int i = 0; i < n + 1; i++) {
				dfa[i] = new Node();
			}
			for (int i = 0; i < m; i++) {
				final String[] s = in.readLine().split(REGEX);
				dfa[Integer.parseInt(s[0])].addChild(Integer.parseInt(s[1]));
				dfa[Integer.parseInt(s[1])].addParent(Integer.parseInt(s[0]));
			}
			in.close();
			Node.checkLoop(dfa);
			for (final String item : accepts) {
				Node.checkTerminal(dfa, Integer.parseInt(item));
				if (!FLAG) {
					break;
				}
			}
			final BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(O), StandardCharsets.UTF_8));
			if (FLAG) {
				long sum = 0;
				for (final String item : accepts) {
					dfa[1].reset();
					Node.count(dfa, Integer.parseInt(item));
					dfa[Integer.parseInt(item)].set();
					sum += dfa[1].COUNT;
				}
				sum %= MOD;
				out.write(String.valueOf(sum));
			} else {
				out.write("-1");
			}
			out.flush();
			out.close();
		} catch (final Exception e) {
			throw new RuntimeException(e);
		}
	}

	private final static class Node {
		private final Set<Integer> CHILD = new HashSet<>();
		private final Map<Integer, Integer> PARENT = new HashMap<>();
		private long COUNT = 0;
		private boolean CALCULATED = false;

		public Node() {
		}

		public void addChild(final int c) {
			CHILD.add(c);
		}

		public void addParent(final int c) {
			if (PARENT.containsKey(c)) {
				PARENT.put(c, PARENT.get(c) + 1);
			} else {
				PARENT.put(c, 1);
			}
		}

		public static void checkLoop(final Node[] dfa) {
			CAN[1] = true;
			Node.checkLoop(dfa, dfa[1], 1);
		}

		public static void checkTerminal(final Node[] dfa, final int item) {
			Node.checkTerminal(dfa, dfa[item], item);
		}

		private static void checkLoop(final Node[] dfa, final Node root, final int current) {
			if (COUNTER[current]) {
				LOOPED[current] = true;
				CHECKED[current] = true;
			}
			COUNTER[current] = true;
			CAN[current] = true;
			for (final Integer item : root.CHILD) {
				if (LOOPED[current]) {
					if (item != current && !CHECKED[item]) {
						Node.checkLoop(dfa, dfa[item], item);
					}
					CHECKED[item] = true;
				} else {
					if (!CHECKED[item]) {
						Node.checkLoop(dfa, dfa[item], item);
						CHECKED[item] = true;
					}
				}
			}
			CHECKED[current] = true;
			COUNTER[current] = false;
		}

		private static void checkTerminal(final Node[] dfa, final Node root, final int current) {
			if (LOOPED[current]) {
				FLAG = false;
				return;
			}
			if (COUNTER[current]) {
				return;
			}
			COUNTER[current] = true;
			for (final Map.Entry<Integer, Integer> item : root.PARENT.entrySet()) {
				Node.checkTerminal(dfa, dfa[item.getKey()], item.getKey());
				if (!FLAG) {
					return;
				}
			}
			COUNTER[current] = false;
		}

		public void reset() {
			this.COUNT = 0;
		}

		public void set() {
			this.CALCULATED = true;
		}

		public static void count(final Node[] dfa, final int c) {
			if (dfa[c].CALCULATED) {
				return;
			}
			Node.count(dfa, dfa[c], c, 1);
		}

		private static void count(final Node[] dfa, final Node root, final int c, final long count) {
			if (c == 1) {
				root.COUNT += count;
			} else {
				root.COUNT = count;
			}
			for (final Map.Entry<Integer, Integer> item : root.PARENT.entrySet()) {
				if (CAN[item.getKey()]) {
					Node.count(dfa, dfa[item.getKey()], item.getKey(), root.COUNT * item.getValue());
				}
			}
		}
	}
}
