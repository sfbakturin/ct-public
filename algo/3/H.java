import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class H {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		int n = in.nextInt();
		in.close();
		final StringBuilder answer = new StringBuilder();
		int div = 2;
		while (n > 1) {
			while (n % div == 0) {
				n /= div;
				answer.append(div);
				answer.append(" ");
			}
			div++;
		}
		System.out.println(answer.toString());
	}
}
