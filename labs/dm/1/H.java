import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class H {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt();
		in.close();
		final StringBuilder s = new StringBuilder("((A0" + '|' + "B0)" + '|' + "(A0" + '|' + "B0))");
		append(n, s);
		System.out.println(s);
	}

	private static void append(final int n, final StringBuilder s) {
		for (int i = 1; i < n; i++) {
			s.insert(0, "((");
			s.append('|').append("((A").append(i).append('|').append("A").append(i).append(")").append('|').append("(B").append(i).append('|').append("B").append(i).append(")))").append('|').append("(A").append(i).append('|').append("B").append(i).append("))");
		}
	}
}
