import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class A {
    private final static String I = "automaton.in";
    private final static String O = "automaton.out";
    private final static String WHITESPACE = "\u0020";
    private final static String ARROW = WHITESPACE + "->" + WHITESPACE;
    private static boolean FLAG = false;
    public static void main(final String... args) {
        try {
            final BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(I), StandardCharsets.UTF_8));
            final String[] s = in.readLine().split(WHITESPACE);
            final int n = Integer.parseInt(s[0]);
            final char start = s[1].charAt(0);
            final Node[] nodes = new Node[26];
            for (int i = 0; i < 26; i++) {
                nodes[i] = new Node();
            }
            for (int i = 0; i < n; i++) {
                final String[] trans = in.readLine().split(ARROW);
                Node.add(nodes[((int) (trans[0].charAt(0))) - 65], trans[1]);
            }
            final int m = Integer.parseInt(in.readLine());
            final BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(O), StandardCharsets.UTF_8));
            for (int i = 0; i < m; i++) {
                final String word = in.readLine();
                nodes[((int) start) - 65].check(0, word, nodes);
                if (FLAG) {
                    out.write("yes");
                } else {
                    out.write("no");
                }
                out.newLine();
                FLAG = false;
            }
            out.flush();
            out.close();
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }

    private final static class Node {
        private final Set<String> CHILDREN;

        public Node() {
            this.CHILDREN = new HashSet<>();
        }

        public static void add(final Node node, final String s) {
            node.CHILDREN.add(s);
        }

        public void check(final int index, final String word, final Node[] nodes) {
            if (FLAG) {
                return;
            }
            for (final String item : this.CHILDREN) {
                final boolean flag = item.charAt(0) == word.charAt(index);
                if (item.length() == 2) {
                    if (flag && index + 1 < word.length()) {
                        nodes[((int) item.charAt(1)) - 65].check(index + 1, word, nodes);
                    }
                } else {
                    if (flag && index == word.length() - 1) {
                        FLAG = true;
                        return;
                    }
                }
            }
        }
    }
}
