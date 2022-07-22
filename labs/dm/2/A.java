import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.TreeMap;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class A {
    private static boolean check(final CustomNode node) {
        return (node.left == null && node.right == null);
    }

    private static void lengthIncrease(final Map<Long, Long> h, final CustomNode node, final long length) {
        if (check(node)) {
            if (node.getCharacter() != -999) {
                h.put(node.getCharacter(), length);
            }
            return;
        }
        lengthIncrease(h, node.left, length + 1);
        lengthIncrease(h, node.right, length + 1);
    }

    public static void main(final String... args) {
        Scanner in = new Scanner(System.in);
        final int n = in.nextInt();
        in.nextLine();
        final Map<Long, Long> p = new TreeMap<>();
        for (int i = 0; i < n; i++) {
            final long input = in.nextLong();
            p.put((long) i + 1, input);
        }
        in.close();
        final List<CustomNode> huffman = new ArrayList<>();
        for (final Map.Entry<Long, Long> item : p.entrySet()) {
            huffman.add(new CustomNode(item.getKey(), item.getValue()));
        }
        huffman.sort(new CustomComparator());
        CustomNode start = null;
        if (huffman.size() != 1) {
            while (huffman.size() > 1) {
                final CustomNode minA = huffman.get(0);
                extractMin(huffman, minA);
                final CustomNode minB = huffman.get(0);
                extractMin(huffman, minB);
                CustomNode minAminB;
                final CustomNode nullSafe = new CustomNode(-999, -1);
                minAminB = addNode(minA, minB, nullSafe);
                huffman.add(minAminB);
                start = minAminB;
            }
            final Map<Long, Long> h = new TreeMap<>();
            long length = 0;
            lengthIncrease(h, start, length);
            long sum = 0;
            for (final Map.Entry<Long, Long> item : p.entrySet()) {
                sum += (long) item.getValue() * h.get(item.getKey());
            }
            System.out.println(sum);
        } else {
            long sum = 0;
            for (final Map.Entry<Long, Long> item : p.entrySet()) {
                sum += item.getValue();
            }
            System.out.println(sum);
        }
    }

    private static CustomNode addNode(final CustomNode minA, final CustomNode minB, final CustomNode nullSafe) {
        CustomNode minAminB;
        if (minB == null) {
            minAminB = new CustomNode(-1, minA.getFrequency(), minA, nullSafe);
        } else {
            minAminB = new CustomNode(-1, minA.getFrequency() + minB.getFrequency(), minA, minB);
        }
        return minAminB;
    }

    private static void extractMin(final List<CustomNode> huffman, final CustomNode minA) {
        if (minA != null) {
            huffman.remove(minA);
        }
        huffman.sort(new CustomComparator());
    }

    private static class CustomNode {
        private final long character;
        private final long frequency;
        private final CustomNode left;
        private final CustomNode right;

        public CustomNode(final long character, final long frequency) {
            this.character = character;
            this.frequency = frequency;
            this.left = null;
            this.right = null;
        }

        public CustomNode(final long character, final long frequency, final CustomNode left, final CustomNode right) {
            this.frequency = frequency;
            this.character = character;
            this.left = left;
            this.right = right;
        }

        public long getCharacter() {
            return this.character;
        }

        public long getFrequency() {
            return this.frequency;
        }
    }

    private static class CustomComparator implements Comparator<CustomNode> {
        public int compare(final CustomNode x, final CustomNode y) {
            return Long.compare(x.getFrequency(), y.getFrequency());
        }
    }
}
