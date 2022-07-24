import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class E {
	private static boolean check(final int[] a, final int n) {
		boolean flag = true;
		for (int i = 1; i < n + 1; i++) {
			if (2 * i <= n) {
				if (a[i] > a[2 * i]) {
					flag = false;
					break;
				}
			}
			if (2 * i + 1 <= n) {
				if (a[i] > a[2 * i + 1]) {
					flag = false;
					break;
				}
			}
		}
		return flag;
	}

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt();
		in.nextLine();
		final int[] a = new int[n + 1];
		for (int i = 1; i < n + 1; i++) {
			a[i] = in.nextInt();
		}
		in.close();
		System.out.println(check(a, n) ? "YES" : "NO");
	}
}
