import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
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

public class Wspp {
    public static void main(final String... args) {
        try {
            final FastScanner in = new FastScanner(args[0]);
            final Map<String, ArrayList<Integer>> stat = new LinkedHashMap<>();
            while (in.hasNextLine()) {
                String s = in.nextWord();
                if (s != null && s.length() != 0) {
                    addElement(stat, s, in.getWord());
                    in.addWord();
                }
            }
            String s = in.nextWord();
            if (s != null && s.length() != 0) {
                addElement(stat, s, in.getWord());
            }
            in.close();
            try (final BufferedWriter out = new BufferedWriter
                    (new OutputStreamWriter
                            (new FileOutputStream
                                    (args[1]),
                                    StandardCharsets.UTF_8)
                    )) {
                for (Map.Entry<String, ArrayList<Integer>> item : stat.entrySet()) {
                    List<Integer> temp = item.getValue();
                    out.write(item.getKey());
                    for (Integer integer : temp) {
                        out.write(" " + integer);
                    }
                    out.newLine();
                }
            }
        } catch (final FileNotFoundException err) {
            System.out.println("Cannot open file: " + err.getMessage());
        } catch (final IOException err) {
            System.out.println("Cannot read file: " + err.getMessage());
        }
    }

    private static void addElement(final Map<String, ArrayList<Integer>> stat, final String s, final int curr) {
        final ArrayList<Integer> temp;
        if (!stat.containsKey(s)) {
            temp = new ArrayList<>();
            temp.add(1);
        } else {
            temp = stat.get(s);
            temp.set(0, temp.get(0) + 1);
        }
        temp.add(curr);
        stat.put(s, temp);
    }
}
