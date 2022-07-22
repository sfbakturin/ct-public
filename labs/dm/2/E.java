import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class E {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final String s = in.nextLine();
        in.close();
        final Map<String, Integer> alphabet = new LinkedHashMap<>();
        for (int i = 0; i < 26; i++) {
            alphabet.put(String.valueOf((char) (i + 97)), i);
        }
        int pointer = 26;
        StringBuilder t = new StringBuilder();
        String last = "";
        final List<Integer> answer = new ArrayList<>();
        t = lzw(s, alphabet, pointer, t, last, answer);
        answer.add(alphabet.get(t.toString()));
        for (Integer integer : answer) {
            System.out.print(integer + " ");
        }
    }

    private static StringBuilder lzw(final String s, final Map<String, Integer> alphabet, int pointer, StringBuilder t, String last, List<Integer> answer) {
        for (int i = 0; i < s.length(); i++) {
            final char c = s.charAt(i);
            if (alphabet.containsKey(t.toString() + c)) {
                last = t.toString() + c;
            } else {
                answer.add(alphabet.get(last));
                alphabet.put(t.toString() + c, pointer);
                pointer++;
                last = String.valueOf(c);
                t = new StringBuilder();
            }
            t.append(c);
        }
        return t;
    }
}
