import java.util.*;
import java.util.function.BiFunction;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class J {
	private static final double ZERO = 0.0;

	private static double getExponentialP(
			final Map<Integer, Double> probabilityX,
			final Map<Integer, Double> probabilityY
	) {
		double p = 0.0;
		for (final int i : probabilityX.keySet())
			p += (probabilityX.get(i) * probabilityX.get(i)) / probabilityY.get(i);
		return p;
	}

	private static double getDifference(final List<Integer> numbers, final int n) {
		double diff = 0.0;
		for (final int num : numbers) {
			diff += ((double) num * (double) num) / n;
		}
		return diff;
	}

	private static <T> double getDifference(final Map<T, List<Integer>> numbers, final int n) {
		double diff = 0.0;
		for (final Map.Entry<T, List<Integer>> number : numbers.entrySet()) {
			diff += getDifference(number.getValue(), n);
		}
		return diff;
	}

	private static Map<Integer, Double> getProbability(
			final Map<Integer, List<Integer>> objects,
			final BiFunction<Integer, Integer, Double> fun,
			final int n
	) {
		final Map<Integer, Double> probability = new HashMap<>();
		for (final Map.Entry<Integer, List<Integer>> object : objects.entrySet()) {
			final int x = object.getKey();
			final List<Integer> ys = object.getValue();
			for (final int y : ys) {
				final double v = fun.apply(y, n);
				final double p = probability.getOrDefault(x, ZERO);
				probability.put(x, p + v);
			}
		}
		return probability;
	}

	public static void main(final String[] args) {
		final Scanner input = new Scanner(System.in);
		final int k = input.nextInt();
		final int n = input.nextInt();
		final Map<Integer, List<Integer>> objects = new HashMap<>();
		for (int i = 0; i < n; i++) {
			final int x = input.nextInt();
			final int y = input.nextInt();
			if (!objects.containsKey(x))
				objects.put(x, new ArrayList<>());
			objects.get(x).add(y);
		}
		input.close();
		final Map<Integer, Double> probabilityX = getProbability(objects, (a, b) -> (double) a / b, n);
		final Map<Integer, Double> probabilityY = getProbability(objects, (a, b) -> 1.0 / b, n);
		final double p = getExponentialP(probabilityX, probabilityY);
		final double d = getDifference(objects, n);
		final double dispersion = d - p;
		System.out.println(dispersion);
	}
}
