import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class E {
    private final static String I = "problem5.in";
    private final static String O = "problem5.out";
    private final static String REGEX = "\u0020";
    private final static int MOD = 1000000000 + 7;
    private static boolean[] TERMINAL;

    public static void main(final String... args) {
        try {
            final BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(I), StandardCharsets.UTF_8));
            final String[] nmkl = in.readLine().split(REGEX);
            final byte n = Byte.parseByte(nmkl[0]);
            final byte m = Byte.parseByte(nmkl[1]);
            final short l = Short.parseShort(nmkl[3]);
            TERMINAL = new boolean[n + 1];
            final String[] accepts = in.readLine().split(REGEX);
            for (final String t : accepts) {
                TERMINAL[Integer.parseInt(t)] = true;
            }
            final Set<Character> alphabet = new HashSet<>();
            final NFA nfa = new NFA(l);
            for (byte i = 0; i < m; i++) {
                final String[] s = in.readLine().split(REGEX);
                alphabet.add(s[2].charAt(0));
                nfa.add(Byte.parseByte(s[0]), Byte.parseByte(s[1]), s[2].charAt(0));
            }
            in.close();
            nfa.convert(alphabet);
            nfa.init();
            final BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(O), StandardCharsets.UTF_8));
            out.write(String.valueOf(nfa.sum()));
            out.flush();
            out.close();
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }

    private final static class NFA {
        private final Stack<Set<Byte>> CHECKER;
        private final Set<Set<Byte>> CHECKED;
        private final Map<Byte, Map<Character, Set<Byte>>> DELTA_NFA;
        private final Map<Set<Byte>, Map<Character, Set<Byte>>> DELTA_DFA;
        private List<Map<Set<Byte>, Long>> LENGTH;
        private final static Set<Byte> START;
        private final short L;

        static {
            START = new HashSet<>();
            START.add((byte) 1);
        }

        public NFA(final short l) {
            this.CHECKER = new Stack<>();
            this.CHECKED = new HashSet<>();
            this.DELTA_NFA = new LinkedHashMap<>();
            this.DELTA_DFA = new LinkedHashMap<>();
            this.L = l;
        }

        public void add(final byte s, final byte e, final char c) {
            final Map<Character, Set<Byte>> temp1;
            if (this.DELTA_NFA.containsKey(s)) {
                temp1 = this.DELTA_NFA.get(s);
                final Set<Byte> temp2;
                if (temp1.containsKey(c)) {
                    temp2 = temp1.get(c);
                } else {
                    temp2 = new HashSet<>();
                }
                temp2.add(e);
                temp1.put(c, temp2);
            } else {
                temp1 = new LinkedHashMap<>();
                final Set<Byte> temp2 = new HashSet<>();
                temp2.add(e);
                temp1.put(c, temp2);
            }
            this.DELTA_NFA.put(s, temp1);
        }

        public void convert(final Set<Character> alphabet) {
            this.CHECKER.push(START);
            while (!this.CHECKER.empty()) {
                final Set<Byte> temp = this.CHECKER.pop();
                for (final char c : alphabet) {
                    final Set<Byte> set = new HashSet<>();
                    for (final byte d : temp) {
                        if (this.DELTA_NFA.containsKey(d) && this.DELTA_NFA.get(d).containsKey(c)) {
                            set.addAll(this.DELTA_NFA.get(d).get(c));
                        }
                    }
                    if (!this.DELTA_DFA.containsKey(temp)) {
                        this.DELTA_DFA.put(temp, new LinkedHashMap<>());
                    }
                    this.DELTA_DFA.get(temp).put(c, set);
                    if (!this.CHECKED.contains(set)) {
                        this.CHECKER.push(set);
                        this.CHECKED.add(set);
                    }
                }
            }
            this.DELTA_NFA.clear();
            this.CHECKED.clear();
            this.CHECKER.clear();
        }

        public void init() {
            this.LENGTH = new ArrayList<>();
            this.LENGTH.add(null);
            for (short y = 1; y <= this.L; y++) {
                this.LENGTH.add(new LinkedHashMap<>());
                this.LENGTH.add(new LinkedHashMap<>());
                for (final Map.Entry<Set<Byte>, Map<Character, Set<Byte>>> item : this.DELTA_DFA.entrySet()) {
                    this.LENGTH.get(y).put(item.getKey(), 0L);
                }
            }
            final Map<Character, Set<Byte>> first = this.DELTA_DFA.get(START);
            for (final Map.Entry<Character, Set<Byte>> item : first.entrySet()) {
                this.LENGTH.get(1).put(item.getValue(), this.LENGTH.get(1).get(item.getValue()) + 1);
            }
        }

        public long sum() {
            long sum = 0;
            for (short y = 1; y <= this.L; y++) {
                for (final Map.Entry<Set<Byte>, Map<Character, Set<Byte>>> transition : this.DELTA_DFA.entrySet()) {
                    final Map<Character, Set<Byte>> item = transition.getValue();
                    for (final Map.Entry<Character, Set<Byte>> to : item.entrySet()) {
                        if (this.LENGTH.get(y + 1).containsKey(to.getValue())) {
                            this.LENGTH.get(y + 1).put(to.getValue(), this.LENGTH.get(y).get(transition.getKey()) + this.LENGTH.get(y + 1).get(to.getValue()));
                            this.LENGTH.get(y + 1).put(to.getValue(), this.LENGTH.get(y + 1).get(to.getValue()) % MOD);
                        }
                    }
                }
            }
            boolean flag = false;
            Set<Byte> temp;
            for (final Map.Entry<Set<Byte>, Long> item : this.LENGTH.get(this.L).entrySet()) {
                temp = item.getKey();
                for (final byte t : temp) {
                    if (TERMINAL[t]) {
                        flag = true;
                        break;
                    }
                }
                sum += (flag ? item.getValue() : 0);
                flag = false;
            }
            return sum % MOD;
        }
    }
}
