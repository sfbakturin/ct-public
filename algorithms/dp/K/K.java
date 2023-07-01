package K;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class K {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt();
		in.nextLine();
		final long[] a = new long[n];
		final long[] d = new long[n];
		final long[] prev = new long[n];
		for (int i = 0; i < n; i++) {
			a[i] = in.nextLong();
		}
		in.close();
		if (a.length != 1) {
			for (int i = 0; i < n; i++) {
				d[i] = 1;
				prev[i] = -1;
				for (int j = 0; j < i; j++) {
					if (a[j] < a[i] && d[j] + 1 > d[i]) {
						d[i] = d[j] + 1;
						prev[i] = j;
					}
				}
			}
			int pos = 0;
			long ans = d[0];
			for (int i = 0; i < n; i++) {
				if (d[i] > ans) {
					pos = i;
					ans = d[i];
				}
			}
			System.out.println(ans);
			final long[] o = new long[(int) ans];
			int u = (int) ans - 1;
			while (pos != -1) {
				o[u] = a[pos];
				pos = (int) prev[pos];
				u--;
			}
			for (int i = 0; i < (int) ans; i++) {
				System.out.print(o[i] + " ");
			}
		} else {
			System.out.println(1);
			System.out.println(a[0]);
		}
	}
}
