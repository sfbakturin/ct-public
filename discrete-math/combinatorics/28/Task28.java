import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Task28 {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int N = in.nextInt();
		in.nextLine();
		final String[] input = in.nextLine().split(" ");
		final int[] a = new int[input.length];
		in.close();
		for (int i = 0; i < input.length; i++) {
			a[i] = Integer.parseInt(input[i]);
		}
		System.out.println(getNext(a, N));
	}

	private static String getNext(final int[] a, final int N) {
		final StringBuilder sb = new StringBuilder();
		boolean flag = false;
		int errorIndex = -1, errorMinIndex = -1;
		for (int i = a.length - 2; i >= 0; i--) {
			if (a[i] < a[i + 1]) {
				errorIndex = i;
				flag = true;
				break;
			}
		}
		if (flag) {
			for (int i = errorIndex + 1; i < a.length; i++) {
				if (a[i] > a[errorIndex]) {
					errorMinIndex = i;
				}
			}
			final int t = a[errorMinIndex];
			a[errorMinIndex] = a[errorIndex];
			a[errorIndex] = t;
			for (int i = 0; i <= errorIndex; i++) {
				sb.append(a[i]).append(' ');
			}
			for (int i = a.length - 1; i >= errorIndex + 1; i--) {
				sb.append(a[i]).append(' ');
			}
		} else {
			sb.append("0 ".repeat(N));
		}
		return sb.toString();
	}
}
