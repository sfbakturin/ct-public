import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class FromZigZag {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final long n = in.nextLong();
		System.out.println(n >= 0 ? n * 2 : ((-2) * n) - 1);
		in.close();
	}
}
