import java.util.LinkedHashSet;
import java.util.Scanner;
import java.util.Set;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Task04 {
	private static void gen(final byte n) {
		String s = "0".repeat(n);
		final Set<String> answer = new LinkedHashSet<>();
		while (true) {
			final String o = s.substring(1);
			if (!answer.contains(o + '1')) {
				s = o + '1';
			} else {
				if (!answer.contains(o + '0')) {
					s = o + '0';
				} else {
					break;
				}
			}
			answer.add(s);
		}
		for (final String item : answer) {
			System.out.println(item);
		}
	}

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final byte n = in.nextByte();
		in.close();
		gen(n);
	}
}
