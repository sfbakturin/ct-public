package info.kgeorgiy.ja.bakturin.concurrent;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class ThreadHandler {
	private final Thread[] ths;
	private final Dispenser d;

	public ThreadHandler(final int threads, final Dispenser dispenser) {
		this.ths = new Thread[threads];
		this.d = dispenser;
		for (int i = 0; i < threads; i++) {
			this.ths[i] = new Thread(this::daemon);
			this.ths[i].start();
		}
	}

	public void close() {
		for (final Thread t : this.ths) {
			t.interrupt();
			while (true) {
				try {
					t.join();
					break;
				} catch (final InterruptedException ignored) {
				}
			}
		}
	}

	private void daemon() {
		try {
			while (!Thread.interrupted()) {
				final Task r;
				synchronized (this.d) {
					while (this.d.isEmpty()) {
						this.d.wait();
					}
					r = this.d.get();
				}
				r.run();
				synchronized (this.d) {
					this.d.notify();
				}
			}
		} catch (final InterruptedException ignored) {
		} finally {
			Thread.currentThread().interrupt();
		}
	}
}
