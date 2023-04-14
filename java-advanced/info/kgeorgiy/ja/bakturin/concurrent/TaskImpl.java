package info.kgeorgiy.ja.bakturin.concurrent;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class TaskImpl<T> implements Task {
	private final Runnable function;
	private final ExpectedCounter counter;
	private final WaitingList<T> list;

	public TaskImpl(final Runnable r, final ExpectedCounter c, final WaitingList<T> l) {
		this.function = r;
		this.counter = c;
		this.list = l;
	}

	@Override
	public void run() {
		synchronized (this) {
			function.run();
			if (counter.incrementAndCheck()) {
				synchronized (list) {
					list.notify();
				}
			}
		}
	}
}
