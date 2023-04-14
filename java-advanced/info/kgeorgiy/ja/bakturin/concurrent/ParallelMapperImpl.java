package info.kgeorgiy.ja.bakturin.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.List;
import java.util.function.Function;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class ParallelMapperImpl implements ParallelMapper {
	private final Dispenser dispenser;
	private final ThreadHandler threadHandler;

	public ParallelMapperImpl(final int threads) {
		this.dispenser = new Dispenser();
		this.threadHandler = new ThreadHandler(threads, this.dispenser);
	}

	@Override
	public <T, R> List<R> map(final Function<? super T, ? extends R> f, final List<? extends T> args) throws InterruptedException {
		final ExpectedCounter counter = new ExpectedCounter(args.size());
		final WaitingList<R> waiter = new WaitingList<>(args.size(), counter);
		for (int i = 0; i < args.size(); i++) {
			final int index = i;
			this.dispenser.add(new TaskImpl<>(() -> waiter.set(index, f.apply(args.get(index))), counter, waiter));
		}
		return waiter.get();
	}

	@Override
	public void close() {
		this.threadHandler.close();
	}
}
