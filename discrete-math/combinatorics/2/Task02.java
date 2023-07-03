import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Task02 {
	public final static List<String> ALPHABET = new ArrayList<>();

	static {
		ALPHABET.add("0");
		ALPHABET.add("1");
	}

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final byte N = in.nextByte();
		in.close();
		for (int i = 1; i < N; i++) {
			final List<String> temp = new ArrayList<>();
			for (int j = ALPHABET.size() - 1; j >= 0; j--) {
				temp.add(ALPHABET.get(j));
			}
			ALPHABET.addAll(temp);
			for (int j = 0; j < ALPHABET.size() / 2; j++) {
				ALPHABET.set(j, "0" + ALPHABET.get(j));
			}
			for (int j = (ALPHABET.size() / 2); j < ALPHABET.size(); j++) {
				ALPHABET.set(j, "1" + ALPHABET.get(j));
			}
		}
		for (final String item : ALPHABET) {
			System.out.println(item);
		}
	}
}
