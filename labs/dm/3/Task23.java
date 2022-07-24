import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Task23 {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final String CURRENT = in.nextLine();
		in.close();
		final String NEXT = getNext(CURRENT);
		final String PREV = getPrev(CURRENT);
		System.out.println(PREV);
		System.out.println(NEXT);
	}

	private static String getPrev(final String current) {
		final StringBuilder answer = new StringBuilder();
		int index = -1;
		for (int i = 0; i < current.length(); i++) {
			if (current.charAt(i) == '1') {
				index = i;
			}
		}
		if (index != -1) {
			for (int i = 0; i < index; i++) {
				answer.append(current.charAt(i));
			}
			if (index != 0 || current.equals("1")) {
				answer.append('0');
			}
			answer.append("1".repeat(Math.max(0, current.length() - (index + 1))));
		}
		return index != -1 && answer.toString().length() == current.length() ? answer.toString() : "-";
	}

	private static String getNext(final String current) {
		final StringBuilder reversed = new StringBuilder();
		final StringBuilder answer = new StringBuilder();
		boolean overflowed = false;
		boolean added = false;
		for (int i = current.length() - 1; i >= 0; i--) {
			final byte i1 = Byte.parseByte(String.valueOf(current.charAt(i)));
			if (!added) {
				if ((i1 + 1) % 2 == 0) {
					reversed.append('0');
					overflowed = true;
				} else {
					reversed.append('1');
				}
				added = true;
			} else {
				if (overflowed) {
					if ((i1 + 1) % 2 == 0) {
						reversed.append('0');
					} else {
						reversed.append('1');
						overflowed = false;
					}
				} else {
					reversed.append(i1);
				}
			}
		}
		if (overflowed) {
			reversed.append('1');
		}
		final String reversedAnswer = reversed.toString();
		for (int i = reversedAnswer.length() - 1; i >= 0; i--) {
			answer.append(reversedAnswer.charAt(i));
		}
		return answer.toString().length() == current.length() ? answer.toString() : "-";
	}
}
