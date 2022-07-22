import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class F {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt();
        in.nextLine();
        final int[] input = new int[n];
        for (int i = 0; i < n; i++) {
            input[i] = in.nextInt();
        }
        in.close();
        final Map<Integer, String> alphabet = new LinkedHashMap<>();
        final Map<String, Integer> alphabetByString = new LinkedHashMap<>();
        for (int i = 0; i < 26; i++) {
            alphabet.put(i, String.valueOf((char) (i + 97)));
            alphabetByString.put(String.valueOf((char) (i + 97)), i);
        }
        int pointer = 26;
        final StringBuilder answer = new StringBuilder();
        answer.append(alphabet.get(input[0]));
        int prev_index = 0;
        alphabet.get(input[0]);
        lzw(n, input, alphabet, alphabetByString, pointer, answer, prev_index);
        System.out.println(answer);
    }

    private static void lzw(final int n, final int[] input, final Map<Integer, String> alphabet, final Map<String, Integer> alphabetByString, int pointer, StringBuilder answer, int prev_index) {
        for (int i = 1; i < n; i++) {
            if (alphabet.containsKey(input[i])) {
                answer.append(alphabet.get(input[i]));
                String s1 = alphabet.get(input[prev_index]);
                String s2 = alphabet.get(input[i]);
                for (int j = 0; j < s2.length(); j++) {
                    String s3 = s1 + s2.charAt(j);
                    if (alphabetByString.containsKey(s3)) {
                        s1 = s3;
                    } else {
                        alphabetByString.put(s3, pointer);
                        alphabet.put(pointer, s3);
                        prev_index = i;
                        pointer++;
                        break;
                    }
                }
            } else {
                String s1 = alphabet.get(input[prev_index]);
                String s2 = s1 + s1.charAt(0);
                answer.append(s2);
                alphabet.put(pointer, s2);
                alphabetByString.put(s2, pointer);
                pointer++;
                prev_index = i;
            }
        }
    }
}
