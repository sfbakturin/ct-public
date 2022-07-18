import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class D {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final String s = in.nextLine();
        in.close();
        final int[] alphabet = new int[256];
        for (int i = 0; i < 26; i++) {
            alphabet[i + 97] = i + 1;
        }
        int index = -1;
        final List<Integer> answer = new ArrayList<>();
        mtf(s, alphabet, index, answer);
        for (Integer integer : answer) {
            System.out.print(integer + " ");
        }
    }

    private static void mtf(String s, int[] alphabet, int index, List<Integer> answer) {
        for (int i = 0; i < s.length(); i++) {
            answer.add(alphabet[s.charAt(i)]);
            if (alphabet[s.charAt(i)] != 1) {
                for (int j = s.charAt(i) - 1; j >= 97; j--) {
                    if (alphabet[s.charAt(i)] > alphabet[j]) {
                        alphabet[j]++;
                    }
                }
                for (int j = s.charAt(i) + 1; j <= 122; j++) {
                    if (alphabet[s.charAt(i)] > alphabet[j]) {
                        alphabet[j]++;
                    }
                }
                if (index != -1) {
                    alphabet[index] = 2;
                }
                alphabet[s.charAt(i)] = 1;
                index = s.charAt(i);
            }
        }
    }
}
