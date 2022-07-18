import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class D {
    private final static String I = "problem4.in";
    private final static String O = "problem4.out";
    private final static String REGEX = "\u0020";
    private final static int MOD = 1000000000 + 7;
    private static boolean[] TERMINAL;

    public static void main(final String... args) {
        try {
            final BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(I), StandardCharsets.UTF_8));
            final String[] nmkl = in.readLine().split(REGEX);
            final int n = Integer.parseInt(nmkl[0]);
            final int m = Integer.parseInt(nmkl[1]);
            final int k = Integer.parseInt(nmkl[2]);
            final int l = Integer.parseInt(nmkl[3]);
            TERMINAL = new boolean[n + 1];
            final DFA dfa = new DFA(n, l);
            final String[] accepts = in.readLine().split(REGEX);
            for (int i = 0; i < k; i++) {
                if (Integer.parseInt(accepts[i]) < n + 1) {
                    TERMINAL[Integer.parseInt(accepts[i])] = true;
                }
            }
            final Map<Character, Integer> alphabet = new HashMap<>();
            int index = 1;
            for (int i = 0; i < m; i++) {
                final String[] s = in.readLine().split(REGEX);
                if (!alphabet.containsKey(s[2].charAt(0))) {
                    alphabet.put(s[2].charAt(0), index++);
                }
                dfa.add(Integer.parseInt(s[0]), Integer.parseInt(s[1]), alphabet.get(s[2].charAt(0)));
            }
            dfa.add(1, 0, 0);
            in.close();
            final BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(O), StandardCharsets.UTF_8));
            out.write(String.valueOf(dfa.sum()));
            out.flush();
            out.close();
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }

    private final static class DFA {
        private final static int s = 1;
        private final static Map<Integer, Map<Integer, Integer>> DELTA = new HashMap<>();
        private static long[][] LENGTH;
        private final int L;
        private final int N;

        public DFA(final int n, final int l) {
            LENGTH = new long[l + 2][n + 2];
            this.L = l;
            this.N = n;
        }

        public void add(final int s, final int e, final int c) {
            final Map<Integer, Integer> temp;
            if (DELTA.containsKey(s)) {
                temp = DELTA.get(s);
            } else {
                temp = new HashMap<>();
            }
            temp.put(c, e);
            DELTA.put(s, temp);
        }

        public long sum() {
            long sum = 0;
            final Map<Integer, Integer> first = DELTA.get(s);
            for (final Map.Entry<Integer, Integer> pivot : first.entrySet()) {
                LENGTH[s][pivot.getValue()]++;
            }
            for (int y = 1; y < this.L; y++) {
                for (int x = 1; x <= this.N; x++) {
                    if (DELTA.containsKey(x)) {
                        final Map<Integer, Integer> item = DELTA.get(x);
                        for (final Map.Entry<Integer, Integer> pivot : item.entrySet()) {
                            LENGTH[y + 1][pivot.getValue()] += LENGTH[y][x];
                            LENGTH[y + 1][pivot.getValue()] %= MOD;
                        }
                    }
                }
            }
            for (int x = 1; x <= this.N; x++) {
                if (TERMINAL[x]) {
                    sum += LENGTH[this.L][x];
                }
            }
            return sum % MOD;
        }
    }
}
