import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class WsppPosition {
    public static void main(final String... args) {
        try {
            final FastScanner in = new FastScanner(args[0]);
            final Map<String, ArrayList<Integer>> stat = new LinkedHashMap<>();
            while (in.hasNextLine()) {
                String s = in.nextWord();
                if (s != null && s.length() != 0) {
                    addElement(stat, s, in.getWord(), in.getLine());
                    in.addWord();
                    if (in.isCritical()) {
                        in.resetCriticalSituation();
                        in.resetWord();
                        in.addLine();
                    }
                } else {
                    if (s == null) {
                        in.resetWord();
                        in.addLine();
                    }
                }
            }
            String s = in.nextWord();
            if (s != null && s.length() != 0) {
                addElement(stat, s, in.getWord(), in.getLine());
            }
            in.close();
            try (final Writer out = new FileWriter(
                    args[1], StandardCharsets.UTF_8
            )) {
                for (Map.Entry<String, ArrayList<Integer>> item : stat.entrySet()) {
                    List<Integer> temp = item.getValue();
                    out.write(item.getKey() + " " + temp.get(0));
                    for (int i = 1; i < temp.size() - 1; i++) {
                        if (i % 2 != 0) {
                            out.write(" " + temp.get(i) + ":" + temp.get(i + 1));
                        }
                    }
                    out.write(System.lineSeparator().charAt(0));
                }
            }
        } catch (final FileNotFoundException err) {
            System.out.println("Cannot open file: " + err.getMessage());
        } catch (final IOException err) {
            System.out.println("Cannot read file: " + err.getMessage());
        }
    }

    private static void addElement(final Map<String, ArrayList<Integer>> stat, final String s, final int curr, final int line) {
        final ArrayList<Integer> temp;
        if (!stat.containsKey(s)) {
            temp = new ArrayList<>();
            temp.add(1);
        } else {
            temp = stat.get(s);
            temp.set(0, temp.get(0) + 1);
        }
        temp.add(line);
        temp.add(curr);
        stat.put(s, temp);
    }
}
