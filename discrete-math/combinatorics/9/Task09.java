import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Task09 {
	private static void gen(final String p, final byte n, final byte o, final byte c) {
		if (o + c == 2 * n) {
			System.out.println(p);
			return;
		}
		if (o < n) {
			gen(p + '(', n, (byte) (o + 1), c);
		}
		if (o > c) {
			gen(p + ')', n, o, (byte) (c + 1));
		}
	}

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final byte n = in.nextByte();
		in.close();
		gen("", n, (byte) 0, (byte) 0);
	}
}
