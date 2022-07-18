import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class Task05 {
    public static final List<String> ALPHABET = new ArrayList<>();

    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int N = in.nextInt();
        final byte K = in.nextByte();
        in.close();
        for (int i = 0; i < K; i++) {
            ALPHABET.add(String.valueOf(i));
        }
        for (int i = 1; i < N; i++) {
            final List<String> reverse = new ArrayList<>();
            final List<String> current = new ArrayList<>(ALPHABET);
            for (int j = ALPHABET.size() - 1; j >= 0; j--) {
                reverse.add(ALPHABET.get(j));
            }
            for (int j = 1; j < K; j++) {
                ALPHABET.addAll(j % 2 == 0 ? current : reverse);
            }
            int count = 0, curr = 0;
            for (int j = 0; j < ALPHABET.size(); j++) {
                ALPHABET.set(j, curr + ALPHABET.get(j));
                count++;
                if (count == ALPHABET.size() / K) {
                    count = 0;
                    curr++;
                }
            }
        }
        for (final String item : ALPHABET) {
            System.out.println(item);
        }
    }
}
