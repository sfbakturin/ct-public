import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class F {
    private final static String I = "isomorphism.in";
    private final static String O = "isomorphism.out";
    private final static String REGEX = "\u0020";
    private static boolean FLAG = true;
    private static boolean[] TERMINAL_1, TERMINAL_2, LOOPED;

    public static void main(final String... args) {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(I), StandardCharsets.UTF_8));
            String[] nmk = in.readLine().split(REGEX);
            final int n1 = Integer.parseInt(nmk[0]);
            final int m1 = Integer.parseInt(nmk[1]);
            TERMINAL_1 = new boolean[n1 + 1];
            String[] accepts = in.readLine().split(REGEX);
            for (final String item : accepts) {
                if (Integer.parseInt(item) < n1 + 1) {
                    TERMINAL_1[Integer.parseInt(item)] = true;
                }
            }
            final Map<Character, Integer> alphabet = new HashMap<>();
            int index = 1;
            for (int i = 0; i < m1; i++) {
                final String[] s = in.readLine().split(REGEX);
                if (!alphabet.containsKey(s[2].charAt(0))) {
                    alphabet.put(s[2].charAt(0), index++);
                }
            }
            nmk = in.readLine().split(" ");
            final int n2 = Integer.parseInt(nmk[0]);
            final int m2 = Integer.parseInt(nmk[1]);
            TERMINAL_2 = new boolean[n2 + 1];
            accepts = in.readLine().split(REGEX);
            for (final String item : accepts) {
                if (Integer.parseInt(item) < n2 + 1) {
                    TERMINAL_2[Integer.parseInt(item)] = true;
                }
            }
            for (int i = 0; i < m2; i++) {
                final String[] s = in.readLine().split(REGEX);
                if (!alphabet.containsKey(s[2].charAt(0))) {
                    alphabet.put(s[2].charAt(0), index++);
                }
            }
            in.close();
            final DFA dfa = new DFA();
            LOOPED = new boolean[Math.max(n1, n2) + 1];
            in = new BufferedReader(new InputStreamReader(new FileInputStream(I), StandardCharsets.UTF_8));
            in.readLine();
            in.readLine();
            for (int i = 0; i < m1; i++) {
                final String[] s = in.readLine().split(REGEX);
                dfa.add(Integer.parseInt(s[0]), Integer.parseInt(s[1]), alphabet.get(s[2].charAt(0)), true);
            }
            in.readLine();
            in.readLine();
            for (int i = 0; i < m2; i++) {
                final String[] s = in.readLine().split(REGEX);
                dfa.add(Integer.parseInt(s[0]), Integer.parseInt(s[1]), alphabet.get(s[2].charAt(0)), false);
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
        private final static Map<Integer, Map<Integer, Integer>> TRANSITION_1 = new HashMap<>();
        private final static Map<Integer, Map<Integer, Integer>> TRANSITION_2 = new HashMap<>();

        public DFA() {
        }

        public void add(final int s, final int e, final int c, final boolean flag) {
            if (flag) {
                if (TRANSITION_1.containsKey(s)) {
                    TRANSITION_1.get(s).put(c, e);
                } else {
                    final Map<Integer, Integer> temp = new HashMap<>();
                    temp.put(c, e);
                    TRANSITION_1.put(s, temp);
                }
            } else {
                if (TRANSITION_2.containsKey(s)) {
                    TRANSITION_2.get(s).put(c, e);
                } else {
                    final Map<Integer, Integer> temp = new HashMap<>();
                    temp.put(c, e);
                    TRANSITION_2.put(s, temp);
                }
            }
        }

        public void check(final Map<Character, Integer> map) {
            this.check(map, s, s);
        }

        private void check(final Map<Character, Integer> map, final Integer a, final Integer b) {
            if (a == null || b == null) {
                return;
            }
            if (LOOPED[a] && LOOPED[b]) {
                return;
            } else {
                LOOPED[a] = true;
                LOOPED[b] = true;
            }
            for (final Map.Entry<Character, Integer> item : map.entrySet()) {
                Integer trans1, trans2;
                if (TRANSITION_1.containsKey(a) && TRANSITION_1.get(a).containsKey(item.getValue())) {
                    trans1 = TRANSITION_1.get(a).get(item.getValue());
                } else {
                    trans1 = null;
                }
                if (TRANSITION_2.containsKey(b) && TRANSITION_2.get(b).containsKey(item.getValue())) {
                    trans2 = TRANSITION_2.get(b).get(item.getValue());
                } else {
                    trans2 = null;
                }
                if (trans1 == null && trans2 != null) {
                    FLAG = false;
                    return;
                }
                if (trans1 != null && trans2 == null) {
                    FLAG = false;
                    return;
                }
                if (trans1 != null && TERMINAL_1[trans1] && !TERMINAL_2[trans2]) {
                    FLAG = false;
                    return;
                }
                if (trans1 != null && !TERMINAL_1[trans1] && TERMINAL_2[trans2]) {
                    FLAG = false;
                    return;
                }
                this.check(map, trans1, trans2);
                if (!FLAG) {
                    return;
                }
            }
        }
    }
}
