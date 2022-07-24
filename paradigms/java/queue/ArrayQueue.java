package queue;

import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class ArrayQueue extends AbstractQueue {
	private final static int START_SIZE = 2;
	private int head, tail;
	private Object[] elements = new Object[START_SIZE];

	public void enqueueImpl(final Object element) {
		if (this.head == this.tail && this.elements[this.head] != null) {
			final Object[] elementsNew = new Object[this.elements.length * 2];
			this.ensureCapacity(elementsNew);
			this.tail = this.elements.length + 1;
			this.head = 0;
			elementsNew[this.elements.length] = element;
			this.elements = elementsNew;
		} else {
			this.elements[this.tail] = element;
			this.tail = (this.tail + 1) % this.elements.length;
		}
	}

	public Object elementImpl() {
		return this.elements[this.head];
	}

	public void dequeueImpl() {
		this.elements[this.head] = null;
		this.head = (this.head + 1) % this.elements.length;
	}

	public void clearImpl() {
		this.elements = new Object[START_SIZE];
		this.head = 0;
		this.tail = 0;
	}

	/*
		Name: count()
		Pred: tail >= 0 && head >= 0 && elements != null
		Post: R <- v : forall i : elements[i] == element -> count++;
	*/
	public int count(final Object element) {
		if (isEmpty()) {
			return 0;
		}
		Objects.requireNonNull(element);
		int count = 0;
		if (this.head < this.tail) {
			for (int i = this.head; i < this.tail; i++) {
				count = this.incCount(element, i, count);
			}
		} else {
			for (int i = this.head; i < this.elements.length; i++) {
				count = this.incCount(element, i, count);
			}
			for (int i = 0; i < this.tail; i++) {
				count = this.incCount(element, i, count);
			}
		}
		return count;
	}

	private int incCount(final Object element, final int i, int count) {
		if (this.elements[i] != null && this.elements[i].equals(element)) {
			count++;
		}
		return count;
	}

	private void ensureCapacity(final Object[] elementsNew) {
		int index = 0;
		if (this.head < this.tail) {
			for (int i = this.head; i < this.tail; i++) {
				elementsNew[index++] = this.elements[i];
			}
		} else {
			for (int i = this.head; i < this.elements.length; i++) {
				elementsNew[index++] = this.elements[i];
			}
			for (int i = 0; i < this.tail; i++) {
				elementsNew[index++] = this.elements[i];
			}
		}
	}
}
