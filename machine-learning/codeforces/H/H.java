import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class H {
	public static void main(final String[] args) {
		final Scanner input = new Scanner(System.in);
		final int k1 = input.nextInt();
		final int k2 = input.nextInt();
		final int n = input.nextInt();
		final int[] countsX1 = new int[k1];
		final int[] countsX2 = new int[k2];
		final Map<Integer, Map<Integer, Integer>> countsX1X2 = new HashMap<>();
		for (int i = 0; i < n; i++) {
			final int x1 = input.nextInt();
			final int x2 = input.nextInt();
			countsX1[x1 - 1]++;
			countsX2[x2 - 1]++;
			if (!countsX1X2.containsKey(x1))
				countsX1X2.put(x1, new HashMap<>());
			final int prevCountX1X2 = countsX1X2.get(x1).getOrDefault(x2, 0);
			countsX1X2.get(x1).put(x2, prevCountX1X2 + 1);
		}
		input.close();
		double difference = 0.0;
		double total = n;
		for (final Map.Entry<Integer, Map<Integer, Integer>> x1s : countsX1X2.entrySet()) {
			for (final Map.Entry<Integer, Integer> x2s : x1s.getValue().entrySet()) {
				final double mi = (double) ((long) countsX1[x1s.getKey() - 1] * countsX2[x2s.getKey() - 1]) / n;
				final double d = ((double) x2s.getValue() - mi);
				difference += (d * d) / mi;
				total -= mi;
			}
		}
		System.out.println(difference + total);
	}
}
