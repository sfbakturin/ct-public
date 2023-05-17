package info.kgeorgiy.ja.bakturin.concurrent;

import java.util.ArrayDeque;
import java.util.Queue;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Dispenser {
	private final Queue<Task> buffer;

	public Dispenser() {
		this.buffer = new ArrayDeque<>();
	}

	public void add(final Task r) {
		synchronized (this) {
			this.buffer.add(r);
			this.notify();
		}
	}

	public boolean isEmpty() {
		synchronized (this) {
			return this.buffer.isEmpty();
		}
	}

	public Task get() {
		synchronized (this) {
			return this.buffer.poll();
		}
	}
}
