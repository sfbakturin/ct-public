import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class L {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int n = in.nextInt();
		in.nextLine();
		final int[] a = new int[n];
		for (int i = 0; i < n; i++) {
			a[i] = in.nextInt();
		}
		in.nextLine();
		Arrays.sort(a);
		final int k = in.nextInt();
		final int[] l = new int[k];
		final int[] r = new int[k];
		in.nextLine();
		for (int i = 0; i < k; i++) {
			String[] o = in.nextLine().split(" ");
			l[i] = Integer.parseInt(o[0]);
			r[i] = Integer.parseInt(o[1]);
		}
		in.close();
		final List<Integer> answer = new ArrayList<>();
		for (int i = 0; i < k; i++) {
			int left = l[i], right = r[i], pl = -1, pr = a.length - 1;
			int posL = -1, posR = -1;
			while (pr - pl > 1) {
				int mid = (pl + pr) / 2;
				if (a[mid] >= left) {
					pr = mid;
				} else {
					pl = mid;
				}
			}
			posL = pr;
			pr = a.length;
			pl = 0;
			while (pr - pl > 1) {
				int mid = (pl + pr) / 2;
				if (a[mid] <= right) {
					pl = mid;
				} else {
					pr = mid;
				}
			}
			posR = pl;
			if (posR - posL == 0) {
				if (left <= a[posR] && a[posR] <= right) {
					answer.add(1);
				} else {
					answer.add(0);
				}
			} else {
				answer.add(posR - posL + 1);
			}
		}
		for (final Integer item : answer) {
			System.out.print(item + " ");
		}
	}
}
