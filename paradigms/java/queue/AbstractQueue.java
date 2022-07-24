package queue;

import java.util.Objects;
import java.util.function.Predicate;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public abstract class AbstractQueue implements Queue {
	private int size = 0;

	@Override
	public int size() {
		return this.size;
	}

	@Override
	public void enqueue(final Object element) {
		this.checkNull(element);
		this.size++;
		this.enqueueImpl(element);
	}

	@Override
	public boolean isEmpty() {
		return this.size == 0;
	}

	@Override
	public Object dequeue() {
		final Object v = this.element();
		this.size--;
		this.dequeueImpl();
		return v;
	}

	@Override
	public void clear() {
		this.size = 0;
		this.clearImpl();
	}

	@Override
	public int countIf(final Predicate<Object> condition) {
		this.checkNull(condition);
		int count = 0, counter = 0;
		final int queueSize = this.size();
		while (counter != queueSize) {
			final Object current = this.dequeue();
			if (condition.test(current)) {
				count++;
			}
			this.enqueue(current);
			counter++;
		}
		return count;
	}

	@Override
	public Object element() {
		return this.elementImpl();
	}

	public abstract void enqueueImpl(final Object element);

	public abstract void dequeueImpl();

	public abstract void clearImpl();

	public abstract Object elementImpl();

	private void checkNull(final Object element) {
		Objects.requireNonNull(element);
	}
}
