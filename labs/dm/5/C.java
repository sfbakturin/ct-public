import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class C {
	private final static String I = "useless.in";
	private final static String O = "useless.out";
	private final static String WHITESPACE = "\u0020";
	private final static String ARROW = WHITESPACE + "->" + WHITESPACE;

	public static void main(final String... args) {
		try {
			final BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(I), StandardCharsets.UTF_8));
			final String[] s = in.readLine().split(WHITESPACE);
			final int n = Integer.parseInt(s[0]);
			final char start = s[1].charAt(0);
			final Set<Character> alphabet = new HashSet<>();
			final Node[] nodes = new Node[26];
			for (int i = 0; i < 26; i++) {
				nodes[i] = new Node(i);
			}
			for (int i = 0; i < n; i++) {
				final String[] trans = in.readLine().split(ARROW);
				alphabet.add(trans[0].charAt(0));
				if (trans.length == 2) {
					for (final char c : trans[1].toCharArray()) {
						if (Character.isUpperCase(c)) {
							alphabet.add(c);
						}
					}
				}
				nodes[((int) (trans[0].charAt(0))) - 65].add((trans.length == 2) ? trans[1] : "");
			}
			in.close();
			final Set<Character> useless = new HashSet<>();
			Node.action(nodes, useless, start, alphabet);
			final List<Character> sorted = new ArrayList<>(useless);
			Collections.sort(sorted);
			final BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(O), StandardCharsets.UTF_8));
			for (final char item : sorted) {
				out.write(item);
				out.write(WHITESPACE);
			}
			out.flush();
			out.close();
		} catch (final Exception e) {
			throw new RuntimeException(e);
		}
	}

	private final static class Node {
		private final Set<String> CHILDREN;
		private final char ID;

		public Node(final int id) {
			this.CHILDREN = new HashSet<>();
			this.ID = (char) (id + 65);
		}

		public void add(final String s) {
			this.CHILDREN.add(s);
		}

		public static void action(final Node[] nodes, final Set<Character> useless, final char start, final Set<Character> alphabet) {
			final Set<Character> SET_GEN = new HashSet<>();
			final Set<Character> SET_CAN = new HashSet<>();
			SET_CAN.add(start);
			int sizeGenPrev = 0, sizeCanPrev = 0;
			while (true) {
				for (final Node node : nodes) {
					for (final String strings : node.CHILDREN) {
						boolean onlyTerminals = true;
						for (final char c : strings.toCharArray()) {
							if (Character.isUpperCase(c) && !SET_GEN.contains(c)) {
								onlyTerminals = false;
								break;
							}
						}
						if (onlyTerminals) {
							SET_GEN.add(node.ID);
						}
					}
				}
				if (sizeGenPrev != SET_GEN.size()) {
					sizeGenPrev = SET_GEN.size();
				} else {
					break;
				}
			}
			for (final Node node : nodes) {
				if (alphabet.contains(node.ID) && !SET_GEN.contains(node.ID)) {
					useless.add(node.ID);
				}
			}
			while (true) {
				for (final Node node : nodes) {
					for (final String strings : node.CHILDREN) {
						if (SET_CAN.contains(node.ID) && !useless.contains(node.ID)) {
							final Set<Character> temp = new HashSet<>();
							for (final char c : strings.toCharArray()) {
								if (Character.isUpperCase(c)) {
									if (!useless.contains(c)) {
										temp.add(c);
									} else {
										temp.clear();
										break;
									}
								}
							}
							SET_CAN.addAll(temp);
						}
					}
				}
				if (sizeCanPrev != SET_CAN.size()) {
					sizeCanPrev = SET_CAN.size();
				} else {
					break;
				}
			}
			for (final Node node : nodes) {
				if (alphabet.contains(node.ID) && !SET_CAN.contains(node.ID)) {
					useless.add(node.ID);
				}
			}
			if (nodes[((int) (start)) - 65].CHILDREN.isEmpty()) {
				useless.add(start);
			}
		}
	}
}
