package info.kgeorgiy.ja.bakturin.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ScalarIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import static java.lang.Math.min;

/**
 * {@code Class} of the job being run Georgiy Korneev's tests.
 *
 * @author Saveliy Bakturin
 * @version 1.0
 */
public class IterativeParallelism implements ScalarIP {
	/**
	 * Abstract function for solving tasks in {@code maximumThreads} parallel threads.
	 *
	 * @param maximumThreads the maximum number of threads to use.
	 * @param values         {@code List} of raw values.
	 * @param threadSet      {@code Function} that proceed from {@code T} to {@code R}.
	 * @param bin            {@code BinaryOperator} value processing of {@code R}.
	 * @param <R>            type of processed values.
	 * @param <T>            type of submitted for processing values.
	 * @return {@code R} processed value.
	 * @throws InterruptedException if executing thread was interrupted.
	 */
	private static <R, T> R general(final int maximumThreads, final List<? extends T> values, final Function<Stream<? extends T>, R> threadSet, final BinaryOperator<R> bin) throws InterruptedException {
		final int real = min(maximumThreads, values.size());
		final int step = values.size() / real;
		final Thread[] threads = new Thread[real];
		final List<R> answer = new ArrayList<>(Collections.nCopies(real, null));
		for (int l = 0, mod = values.size() % real, i = 0; l < values.size(); mod--, i++) {
			final int index = i;
			final int constL = l;
			final int constMod = mod;
			threads[i] = new Thread(() -> answer.set(index, threadSet.apply(values.subList(constL, min(constL + step + (constMod > 0 ? 1 : 0), values.size())).stream())));
			threads[i].start();
			l += step + (mod > 0 ? 1 : 0);
		}
		for (final Thread th : threads) {
			th.join();
		}
		return answer.stream().reduce(bin).orElse(null);
	}

	@Override
	public <T> T maximum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator) throws InterruptedException {
		return general(threads, values, s -> s.max(comparator).orElse(null), BinaryOperator.maxBy(comparator));
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
		return general(threads, values, s -> (int) s.filter(predicate).count(), Integer::sum);
	}
}
