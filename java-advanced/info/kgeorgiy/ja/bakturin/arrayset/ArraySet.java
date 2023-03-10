package info.kgeorgiy.ja.bakturin.arrayset;

import java.util.*;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class ArraySet<E> extends AbstractSet<E> implements SortedSet<E> {
	private final List<E> values;
	private final Comparator<? super E> comparator;

	public ArraySet() {
		this(List.of(), null);
	}

	public ArraySet(final Comparator<? super E> cmp) {
		this(List.of(), cmp);
	}

	public ArraySet(final Collection<? extends E> c) {
		this(List.copyOf(c), null);
	}

	public ArraySet(final Collection<? extends E> c, final Comparator<? super E> cmp) {
		this(toList(c, cmp), cmp);
	}

	private static <E> List<E> toList(Collection<? extends E> c, Comparator<? super E> cmp) {
		SortedSet<E> set = new TreeSet<>(cmp);
		set.addAll(c);
		return set.stream().toList();
	}

	private ArraySet(final List<E> vs, final Comparator<? super E> cmp) {
		values = vs;
		comparator = cmp;
	}

	@SuppressWarnings("unchecked")
	private int compare(final E left, final E right) {
		return (Objects.isNull(comparator) ? ((Comparable<? super E>) left).compareTo(right) : comparator.compare(left, right));
	}

	private enum IntervalPositionState {
		RAY, DOT, END

	}

	private int getIndexOf(final E element) {
		final int result = Collections.binarySearch(values, element, comparator);
		return (result >= 0 ? result : (result + 1) * (-1));
	}

	private int getIndexOfState(final E element, final IntervalPositionState state, final int defaultValue, final boolean isLeft) {
		return switch (state) {
			case RAY -> defaultValue;
			case DOT -> getIndexOf(element) + (isLeft ? 0 : 1);
			case END -> getIndexOf(element) + (isLeft ? 1 : 0);
		};
	}

	private SortedSet<E> getInterval(final E fromElement, final E toElement, final IntervalPositionState fromState, final IntervalPositionState toState) {
		if (fromState != IntervalPositionState.RAY && toState != IntervalPositionState.RAY) {
			if (compare(Objects.requireNonNull(fromElement), Objects.requireNonNull(toElement)) > 0) {
				throw new IllegalArgumentException();
			}
		}
		final int from = getIndexOfState(fromElement, fromState, 0, true);
		final int to = getIndexOfState(toElement, toState, size(), false);
		return new ArraySet<>(values.subList(from, to), comparator);
	}

	@Override
	public Iterator<E> iterator() {
		return values.iterator();
	}

	@Override
	public int size() {
		return values.size();
	}

	@Override
	public Comparator<? super E> comparator() {
		return comparator;
	}

	@Override
	public SortedSet<E> subSet(final E fromElement, final E toElement) {
		return getInterval(fromElement, toElement, IntervalPositionState.DOT, IntervalPositionState.END);
	}

	@Override
	public SortedSet<E> headSet(final E toElement) {
		return getInterval(null, toElement, IntervalPositionState.RAY, IntervalPositionState.END);
	}

	@Override
	public SortedSet<E> tailSet(final E fromElement) {
		return getInterval(fromElement, null, IntervalPositionState.DOT, IntervalPositionState.RAY);
	}

	@Override
	public E first() {
		if (size() != 0) {
			return values.get(0);
		} else {
			throw new NoSuchElementException();
		}
	}

	@Override
	public E last() {
		if (size() != 0) {
			return values.get(size() - 1);
		} else {
			throw new NoSuchElementException();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean contains(final Object e) {
		return Collections.binarySearch(values, (E) Objects.requireNonNull(e), comparator) >= 0;
	}
}
