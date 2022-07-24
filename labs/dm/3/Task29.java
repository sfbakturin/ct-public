import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Task29 {
	public static void gen(int[] a) {
		if (a.length != 2) {
			final List<Integer> list = new ArrayList<>();
			for (final Integer item : a) {
				list.add(item);
			}
			list.set(list.size() - 1, list.get(list.size() - 1) - 1);
			list.set(list.size() - 2, list.get(list.size() - 2) + 1);
			if (list.get(list.size() - 2) > list.get(list.size() - 1)) {
				list.set(list.size() - 2, list.get(list.size() - 2) + list.get(list.size() - 1));
				System.out.print(list.get(0) + "=" + list.get(1));
				for (int i = 2; i < list.size() - 1; i++) {
					System.out.print("+" + list.get(i));
				}
				return;
			}
			while (!(list.get(list.size() - 2) * 2 > list.get(list.size() - 1))) {
				final int A = list.get(list.size() - 2), B = list.get(list.size() - 1) - A;
				list.add(B);
				list.set(list.size() - 2, list.get(list.size() - 3));
			}
			System.out.print(list.get(0) + "=" + list.get(1));
			for (int i = 2; i < list.size(); i++) {
				System.out.print("+" + list.get(i));
			}
		} else {
			System.out.println("No solution");
		}
	}

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final int[] CURRENT = Arrays.stream(in.nextLine().split("[=+]")).mapToInt(Integer::parseInt).toArray();
		in.close();
		gen(CURRENT);
	}
}
