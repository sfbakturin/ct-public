import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class A {
    private final static String I = "problem1.in";
    private final static String O = "problem1.out";
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
            final DFA dfa = new DFA(n + 1, index + 1);
            final String[] accepts = in.readLine().split(REGEX);
            for (int i = 0; i < k; i++) {
                dfa.add(Integer.parseInt(accepts[i]));
            }
            for (int i = 0; i < m; i++) {
                final String[] s = in.readLine().split(REGEX);
                if (map.containsKey(s[2].charAt(0))) {
                    dfa.add(Integer.parseInt(s[0]), Integer.parseInt(s[1]), map.get(s[2].charAt(0)));
                }
            }
            in.close();
            final BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(O), StandardCharsets.UTF_8));
            final int[] w = new int[word.length()];
            for (int i = 0; i < word.length(); i++) {
                w[i] = map.get(word.charAt(i));
            }
            if (dfa.accept(w)) {
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

    private final static class DFA {
        private final static int s = 1;
        private final static Set<Integer> TERMINAL = new HashSet<>();
        private static int[][] DELTA;

        public DFA(final int w, final int h) {
            DELTA = new int[w][h];
        }

        public void add(final int t) {
            TERMINAL.add(t);
        }

        public void add(final int s, final int e, final int c) {
            DELTA[s][c] = e;
        }

        public boolean accept(final int[] x) {
            int curr = s;
            for (final int j : x) {
                curr = DELTA[curr][j];
            }
            return TERMINAL.contains(curr);
        }
    }
}
