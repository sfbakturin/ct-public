import java.util.Scanner;
import java.io.PrintWriter;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class J {
	private static int getGCD(final int a, final int b) {
		if (a == 0 || b == 0) {
			return a;
		} else {
			return getGCD(b, a % b);
		}
	}

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final PrintWriter out = new PrintWriter(System.out);
		final int COUNT = in.nextInt();
		in.nextLine();
		final int[] ALPHABET = new int[COUNT];
		for (int i = 0; i < COUNT; i++) {
			ALPHABET[i] = in.nextInt();
		}
		in.close();
		int PREV = ALPHABET[0];
		int NEXT = Integer.MIN_VALUE;
		for (int i = 1; i < COUNT; i++) {
			NEXT = ALPHABET[i];
			if (PREV > NEXT) {
				final int TEMP = getGCD(PREV, NEXT);
				PREV = TEMP;
			} else {
				final int TEMP = getGCD(NEXT, PREV);
				PREV = TEMP;
			}
		}
		try {
			out.print(PREV);
			out.flush();
		} catch (final Exception err) {
			System.out.println("Can't write: " + err.getMessage());
		}
		out.close();
	}
}
