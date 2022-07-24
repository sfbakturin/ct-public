import java.util.Scanner;

import static java.lang.Math.max;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Task27 {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final String CURRENT = in.nextLine();
		in.close();
		if (isLast(CURRENT)) {
			System.out.println("-");
		} else {
			System.out.println(getNext(CURRENT));
		}
	}

	private static String getNext(final String current) {
		final StringBuilder sb = new StringBuilder();
		int o = 0, c = 0;
		boolean loop = true;
		for (int i = current.length() - 1; i >= 0; i--) {
			if (loop) {
				switch (current.charAt(i)) {
					case ')' -> c++;
					case '(' -> {
						o++;
						if (c > o) {
							loop = false;
						}
					}
				}
			} else {
				break;
			}
		}
		for (int i = 0; i < current.length() - o - c; i++) {
			sb.append(current.charAt(i));
		}
		sb.append(')');
		sb.append("(".repeat(max(0, o)));
		sb.append(")".repeat(max(0, c - 1)));
		return sb.toString();
	}

	private static boolean isLast(final String s) {
		boolean flag = true;
		for (int i = 0; i < s.length(); i++) {
			if ((i % 2 == 0) && (s.charAt(i) != '(')) {
				flag = false;
				break;
			}
		}
		return flag;
	}
}
