import java.util.Arrays;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class J {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt();
		in.nextLine();
		final String[] s = new String[n];
		for (int i = 0; i < n; i++) {
			s[i] = in.nextLine();
		}
		in.close();
		Arrays.sort(s);
		String[] prev_s = new String[1024];
		int prev_l = 0;
		for (int i = 0; i < n; i++) {
			final String[] sp = s[i].split("/");
			if (sp.length == 1) {
				System.out.println(s[i]);
				continue;
			}
			if ((sp.length > prev_l) || (sp.length > 1 && sp.length < prev_l)) {
				System.out.println(" ".repeat(2 * (sp.length - 1)) + sp[sp.length - 1]);
			}
			if (sp.length == prev_l) {
				int count = 0;
				for (int j = 0; j < prev_l; j++) {
					if (prev_s[j].compareTo(sp[j]) == 0) {
						count++;
					}
				}
				if (count == prev_l - 1) {
					System.out.println(" ".repeat(2 * (sp.length - 1)) + sp[sp.length - 1]);
				}
			}
			prev_s = sp;
			prev_l = sp.length;
		}
	}
}
