package info.kgeorgiy.ja.bakturin.concurrent;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class ExpectedCounter {
	private final int expected;
	private int actual;

	public ExpectedCounter(final int e) {
		this.expected = e;
		this.actual = 0;
	}

	public boolean incrementAndCheck() {
		synchronized (this) {
			this.actual++;
			return this.actual == this.expected;
		}
	}

	public boolean check() {
		synchronized (this) {
			return this.expected == this.actual;
		}
	}
}
