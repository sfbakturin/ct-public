import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Task07 {
	private static void gen(final String p, final byte n) {
		if (p.length() == n) {
			for (final char ch : p.toCharArray()) {
				System.out.print(ch + " ");
			}
			System.out.println();
			return;
		}
		for (int i = 1; i <= n; i++) {
			if (!p.contains(String.valueOf(i))) {
				gen(p + i, n);
			}
		}
	}

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final byte n = in.nextByte();
		in.close();
		gen("", n);
	}
}
