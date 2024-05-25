package queue;

import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

	/*
		Model: ...a[n - 3], a[n - 2], a[n - 1], a[0], a[1], [2], ...
		head -> first element, tail -> last element
	*/

public class ArrayQueueADT {
	private final static int START_SIZE = 2;
	private int head, tail, size;
	private Object[] elements = new Object[START_SIZE];

	/*
		Name: enqueue(element);
		Pred: element != null;
		Post: n' = n + 1 && a[n'] == element
	*/
	public static void enqueue(final ArrayQueueADT queue, final Object element) {
		checkNullADT(queue, element);
		if (queue.head == queue.tail && queue.elements[queue.head] != null) {
			final Object[] elementsNew = new Object[queue.elements.length * 2];
			ensureCapacity(queue, elementsNew);
			checkNullADT(queue, element);
			queue.tail = queue.elements.length + 1;
			queue.head = 0;
			elementsNew[queue.elements.length] = element;
			queue.elements = elementsNew;
		} else {
			queue.elements[queue.tail] = element;
			queue.tail = (queue.tail + 1) % queue.elements.length;
		}
		queue.size++;
	}

	/*
		Name: size()
		Pred: queue != null
		Post: R <- queue.size
	*/
	public static int size(final ArrayQueueADT queue) {
		checkNull(queue);
		return queue.size;
	}

	/*
		Name: isEmpty()
		Pred: queue != null && queue != null;
		Post: R <- queue.size <=> 0
	*/
	public static boolean isEmpty(final ArrayQueueADT queue) {
		checkNull(queue);
		return queue.size == 0;
	}

	/*
		Name: element();
		Pred: queue.head in [0..n - 1] && queue != null;
		Post: n' = queue.head && R = a[n'];
	*/
	public static Object element(final ArrayQueueADT queue) {
		checkNull(queue);
		return queue.elements[queue.head];
	}

	/*
		Name: dequeue();
		Pred: queue.head in [0..n - 1];
		Post: n' := queue.head && R <- a[n'];
	*/
	public static Object dequeue(final ArrayQueueADT queue) {
		checkNull(queue);
		final Object v = queue.elements[queue.head];
		queue.elements[queue.head] = null;
		queue.head = (queue.head + 1) % queue.elements.length;
		queue.size--;
		return v;
	}

	/*
		Name: clear()
		Pred: true
		Post: R <- (forall i := [0..n - 1] : a[i] == null) && (queue.head := queue.tail := queue.tail := 0)
	*/
	public static void clear(final ArrayQueueADT queue) {
		checkNull(queue);
		queue.elements = new Object[START_SIZE];
		queue.size = 0;
		queue.head = 0;
		queue.tail = 0;
	}

	/*
		Name: count()
		Pred: tail >= 0 && head >= 0 && elements != null
		Post: R <- v : forall i : elements[i] == element -> count++;
	*/
	public static int count(final ArrayQueueADT queue, final Object element) {
		checkNullADT(queue, element);
		if (queue.size == 0) {
			return 0;
		}
		int count = 0;
		if (queue.head < queue.tail) {
			for (int i = queue.head; i < queue.tail; i++) {
				count = incCount(queue, element, i, count);
			}
		} else {
			for (int i = queue.head; i < queue.elements.length; i++) {
				count = incCount(queue, element, i, count);
			}
			for (int i = 0; i < queue.tail; i++) {
				count = incCount(queue, element, i, count);
			}
		}
		return count;
	}

	private static void checkNullADT(final ArrayQueueADT queue, final Object element) {
		checkNull(element);
		checkNull(queue);
	}

	private static void checkNull(final Object element) {
		Objects.requireNonNull(element);
	}

	private static int incCount(final ArrayQueueADT queue, final Object element, final int i, int count) {
		checkNullADT(queue, element);
		if (queue.elements[i] != null && queue.elements[i].equals(element)) {
			count++;
		}
		return count;
	}

	private static void ensureCapacity(final ArrayQueueADT queue, final Object[] elementsNew) {
		int index = 0;
		if (queue.head < queue.tail) {
			for (int i = queue.head; i < queue.tail; i++) {
				elementsNew[index++] = queue.elements[i];
			}
		} else {
			for (int i = queue.head; i < queue.elements.length; i++) {
				elementsNew[index++] = queue.elements[i];
			}
			for (int i = 0; i < queue.tail; i++) {
				elementsNew[index++] = queue.elements[i];
			}
		}
	}
}
