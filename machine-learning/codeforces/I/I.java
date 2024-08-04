import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class I {
	private static long sum(final List<Integer> list) {
		long sum = 0;
		for (final int s : list)
			sum += s;
		return sum;
	}

	private static long sum(final long[] list) {
		long sum = 0;
		for (final long s : list)
			sum += s;
		return sum;
	}

	private static long getIntraClassDistance(final List<Integer> object, final long objectsSum) {
		long d = 0;
		long oldSum = objectsSum;
		long newSum = 0;
		final int size = object.size();
		for (int i = 0; i < size; i++) {
			final int singleObject = object.get(i);
			oldSum -= singleObject;
			d += ((long) i - size + 1 + i) * singleObject + (oldSum - newSum);
			newSum += singleObject;
		}
		return d;
	}

	private static long getIntraClassDistances(
			final Map<Integer, List<Integer>> objects,
			final long[] objectsSums,
			final int k
	) {
		long d = 0;
		for (int i = 0; i < k; i++) {
			final List<Integer> object = objects.get(i + 1);
			if (Objects.isNull(object))
				continue;
			d += getIntraClassDistance(object, objectsSums[i]);
		}
		return d;
	}

	private static long getInterClassDistances(
			final List<XY<Integer, Integer>> objects,
			final long[] objectsSums,
			final int[] objectsCounts,
			final int k
	) {
		long d = 0;
		final long[] newObjectsSums = new long[k];
		final int[] newObjectsCounts = new int[k];
		long oldSum = sum(objectsSums);
		long newSum = 0;
		final int size = objects.size();
		for (int i = 0; i < size; i++) {
			final XY<Integer, Integer> object = objects.get(i);
			final int X = object.x;
			final int Y = object.y - 1;
			oldSum -= X;
			objectsSums[Y] -= X;
			objectsCounts[Y]--;
			d += ((long) 2 * i - newObjectsCounts[Y] - size + 1 + objectsCounts[Y]) * X +
					(oldSum - objectsSums[Y]) - (newSum - newObjectsSums[Y]);
			newSum += X;
			newObjectsSums[Y] += X;
			newObjectsCounts[Y]++;
		}
		return d;
	}

	public static void main(final String[] args) {
		final Scanner input = new Scanner(System.in);
		final int k = input.nextInt();
		final int n = input.nextInt();
		final Map<Integer, List<Integer>> objects = new HashMap<>();
		final List<XY<Integer, Integer>> pairedObjects = new ArrayList<>();

		for (int i = 0; i < n; i++) {
			final int x = input.nextInt();
			final int y = input.nextInt();
			if (!objects.containsKey(y))
				objects.put(y, new ArrayList<>());
			objects.get(y).add(x);
			pairedObjects.add(new XY<>(x, y));
		}

		input.close();

		for (final Map.Entry<Integer, List<Integer>> object : objects.entrySet())
			Collections.sort(object.getValue());
		pairedObjects.sort(new XYComparator<>());

		final long[] objectsSums = new long[k];
		final int[] objectsCounts = new int[k];
		for (int i = 0; i < k; i++) {
			if (Objects.nonNull(objects.get(i + 1))) {
				objectsSums[i] = sum(objects.get(i + 1));
				objectsCounts[i] = objects.get(i + 1).size();
			}
		}

		final long intraClassDistances = getIntraClassDistances(objects, objectsSums, k);
		final long interClassDistances = getInterClassDistances(pairedObjects, objectsSums, objectsCounts, k);
		System.out.println(intraClassDistances);
		System.out.println(interClassDistances);
	}

	private record XY<L extends Comparable<L>, R extends Comparable<R>>(L x, R y) {
	}

	private static class XYComparator<
			L extends Comparable<L>,
			R extends Comparable<R>
			> implements Comparator<XY<L, R>> {
		@Override
		public int compare(final XY<L, R> o1, final XY<L, R> o2) {
			return o1.x.compareTo(o2.x);
		}
	}
}
