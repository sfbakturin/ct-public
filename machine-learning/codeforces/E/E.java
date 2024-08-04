import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

import static java.lang.Math.log;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class E {
	private static final Double ZERO = 0.0;

	public static void main(final String[] args) {
		final Scanner input = new Scanner(System.in);
		final int kx = input.nextInt();
		final int ky = input.nextInt();
		final int n = input.nextInt();
		final double single = 1.0 / n;
		final List<Integer> itemsX = new ArrayList<>();
		final List<Integer> itemsY = new ArrayList<>();
		for (int i = 0; i < n; i++) {
			final int x = input.nextInt();
			final int y = input.nextInt();
			itemsX.add(x);
			itemsY.add(y);
		}
		input.close();
		final Map<Integer, Map<Integer, Double>> probabilityXY = new HashMap<>();
		final double[] probabilityX = new double[kx];
		for (int i = 0; i < n; i++) {
			final int x = itemsX.get(i) - 1;
			final int y = itemsY.get(i) - 1;
			if (!probabilityXY.containsKey(y))
				probabilityXY.put(y, new HashMap<>());
			final double prev = probabilityXY.get(y).getOrDefault(x, ZERO);
			probabilityXY.get(y).put(x, prev + single);
			probabilityX[x] = probabilityX[x] + single;
		}
		double ent = 0.0;
		for (final Map.Entry<Integer, Map<Integer, Double>> ys: probabilityXY.entrySet()) {
			for (final Map.Entry<Integer, Double> xs: ys.getValue().entrySet()) {
				final double prob = xs.getValue();
				ent += (-prob) * (log(prob) - log(probabilityX[xs.getKey()]));
			}
		}
		System.out.println(ent);
	}
}
