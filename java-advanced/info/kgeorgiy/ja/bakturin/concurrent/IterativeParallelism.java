package info.kgeorgiy.ja.bakturin.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ScalarIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import static java.lang.Math.min;

/**
 * {@code Class} of the job being run Georgiy Korneev's tests.
 *
 * @author Saveliy Bakturin
 * @version 2.0
 */
public class IterativeParallelism implements ScalarIP {
	private final ParallelMapper mapper;

	public IterativeParallelism() {
		mapper = null;
	}

	public IterativeParallelism(final ParallelMapper mapper) {
		this.mapper = mapper;
	}

	/**
	 * Abstract function for solving tasks in {@code maximumThreads} parallel threads.
	 *
	 * @param mapper         optional {@code ParallelMapper}
	 * @param maximumThreads the maximum number of threads to use.
	 * @param values         {@code List} of raw values.
	 * @param threadSet      {@code Function} that proceed from {@code T} to {@code R}.
	 * @param bin            {@code Function} value processing of {@code R}.
	 * @param <R>            type of processed values.
	 * @param <T>            type of submitted for processing values.
	 * @return {@code R} processed value.
	 * @throws InterruptedException if executing thread was interrupted.
	 */
	private static <R, T> R general(final ParallelMapper mapper, final int maximumThreads, final List<? extends T> values, final Function<Stream<? extends T>, R> threadSet, final Function<Stream<R>, R> bin) throws InterruptedException {
		final int real = min(maximumThreads, values.size());
		final int step = values.size() / real;
		final List<List<? extends T>> sublists = new ArrayList<>(Collections.nCopies(real, null));
		for (int l = 0, mod = values.size() % real, i = 0; l < values.size(); mod--, i++) {
			sublists.set(i, values.subList(l, min(l + step + (mod > 0 ? 1 : 0), values.size())));
			l += step + (mod > 0 ? 1 : 0);
		}
		final List<R> results;
		if (Objects.isNull(mapper)) {
			final Thread[] threads = new Thread[real];
			results = new ArrayList<>(Collections.nCopies(real, null));
			for (int i = 0; i < real; i++) {
				final int index = i;
				threads[i] = new Thread(() -> results.set(index, threadSet.apply(sublists.get(index).stream())));
				threads[i].start();
			}
			for (final Thread th : threads) {
				th.join();
			}
		} else {
			final Function<List<? extends T>, R> function =
					(final List<? extends T> val) -> threadSet.apply(val.stream());
			results = mapper.map(function, sublists);
		}
		return bin.apply(results.stream());
	}

	@Override
	public <T> T maximum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator) throws InterruptedException {
		return general(mapper, threads, values, s -> s.max(comparator).orElse(null), s -> s.reduce(BinaryOperator.maxBy(comparator)).orElse(null));
	}

	@Override
	public <T> T minimum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator) throws InterruptedException {
		return maximum(threads, values, comparator.reversed());
	}

	@Override
	public <T> boolean all(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
		return count(threads, values, predicate) == values.size();
	}

	@Override
	public <T> boolean any(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
		return count(threads, values, predicate) > 0;
	}

	@Override
	public <T> int count(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
		return general(mapper, threads, values, s -> (int) s.filter(predicate).count(), s -> s.reduce(Integer::sum).orElse(0));
	}
}
