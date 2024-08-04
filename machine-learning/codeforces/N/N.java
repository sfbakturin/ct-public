import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class N {
	private static void show(final List<Integer> list) {
		System.out.printf("%d ", list.size());
		Collections.sort(list);
		System.out.print(list.get(0));
		for (int i = 1; i < list.size(); i++) {
			System.out.printf(" %d", list.get(i));
		}
		System.out.println();
	}

	public static void main(final String[] args) {
		final Scanner input = new Scanner(System.in);
		final int n = input.nextInt();
		final int m = input.nextInt();
		final int k = input.nextInt();
		final Map<Integer, List<Integer>> cnt = new HashMap<>();
		for (int i = 0; i < n; i++) {
			final int c = input.nextInt();
			if (!cnt.containsKey(c))
				cnt.put(c, new ArrayList<>());
			cnt.get(c).add(i + 1);
		}
		input.close();
		final Map<Integer, List<Integer>> obj = new HashMap<>();
		int x = 0;
		for (final Map.Entry<Integer, List<Integer>> c : cnt.entrySet()) {
			final List<Integer> classes = c.getValue();
			for (final int ci : classes) {
				if (!obj.containsKey(x))
					obj.put(x, new ArrayList<>());
				obj.get(x).add(ci);
				x = (x + 1) % k;
			}
		}
		for (final Map.Entry<Integer, List<Integer>> o : obj.entrySet())
			show(o.getValue());
	}
}
