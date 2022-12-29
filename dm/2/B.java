import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class B {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		String s = in.nextLine();
		in.close();
		final List<String> strings = new ArrayList<>();
		strings.add(s);
		for (int i = 0; i < s.length() - 1; i++) {
			s = shift(s);
			strings.add(s);
		}
		Collections.sort(strings);
		final StringBuilder answer = new StringBuilder();
		for (int i = 0; i < s.length(); i++) {
			answer.append(strings.get(i).charAt(s.length() - 1));
		}
		System.out.println(answer);
	}

	private static String shift(final String s) {
		final char first = s.charAt(0);
		return s.substring(1) + first;
	}
}
