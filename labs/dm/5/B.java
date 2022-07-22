import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class B {
    private final static String I = "epsilon.in";
    private final static String O = "epsilon.out";
    private final static String WHITESPACE = "\u0020";
    private final static String ARROW = WHITESPACE + "->" + WHITESPACE;

    public static void main(final String... args) {
        try {
            final BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(I), StandardCharsets.UTF_8));
            final String[] s = in.readLine().split(WHITESPACE);
            final int n = Integer.parseInt(s[0]);
            final Node[] nodes = new Node[26];
            for (int i = 0; i < 26; i++) {
                nodes[i] = new Node(i);
            }
            final Set<Character> alphabet = new HashSet<>();
            for (int i = 0; i < n; i++) {
                final String[] trans = in.readLine().split(ARROW);
                if (trans.length == 1) {
                    alphabet.add(trans[0].charAt(0));
                }
                nodes[((int) trans[0].charAt(0)) - 65].add((trans.length == 2) ? trans[1] : null);
            }
            Node.check(alphabet, nodes);
            final List<Character> sorted = new ArrayList<>(alphabet);
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
        private final int ID;

        public Node(final int id) {
            this.CHILDREN = new HashSet<>();
            this.ID = id;
        }

        public void add(final String s) {
            this.CHILDREN.add(s);
        }

        public static void check(final Set<Character> list, final Node[] nodes) {
            int sizeBefore = list.size();
            while (true) {
                for (final Node node : nodes) {
                    for (final String item : node.CHILDREN) {
                        if (item == null) {
                            list.add((char) (node.ID + 65));
                        } else {
                            boolean isAllEpsilon = true;
                            for (int i = 0; i < item.length(); i++) {
                                if (!(list.contains(item.charAt(i)) && !Character.isLowerCase(item.charAt(i)))) {
                                    isAllEpsilon = false;
                                    break;
                                }
                            }
                            if (isAllEpsilon) {
                                list.add((char) (node.ID + 65));
                            }
                        }
                    }
                }
                if (list.size() != sizeBefore) {
                    sizeBefore = list.size();
                } else {
                    break;
                }
            }
        }
    }
}
