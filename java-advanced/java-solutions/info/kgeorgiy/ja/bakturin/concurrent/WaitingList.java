package info.kgeorgiy.ja.bakturin.concurrent;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class WaitingList<T> {
	private final List<T> results;
	private final ExpectedCounter counter;

	public WaitingList(final int size, final ExpectedCounter cnt) {
		this.results = new ArrayList<>(Collections.nCopies(size, null));
		this.counter = cnt;
	}

	public void set(final int i, final T r) {
		synchronized (this) {
			this.results.set(i, r);
		}
	}

	public List<T> get() throws InterruptedException {
		synchronized (this) {
			while (!this.counter.check()) {
				this.wait();
			}
			return this.results;
		}
	}
}
