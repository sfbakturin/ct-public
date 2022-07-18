package queue;

import java.util.Objects;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

/*
 * Model: ...a[n - 3], a[n - 2], a[n - 1], a[0], a[1], [2], ...
 * head -> first element, tail -> last element
 */

public class ArrayQueueModule {
    private static final int START_SIZE = 2;
    private static int head, tail, size;
    private static Object[] elements = new Object[START_SIZE];

    /*
     * Name: enqueue(element);
     * Pred: element != null;
     * Post: n' = n + 1 && a[n'] == element
     */
    public static void enqueue(final Object element) {
        checkNull(element);
        if (head == tail && elements[head] != null) {
            final Object[] elementsNew = new Object[elements.length * 2];
            ensureCapacity(elementsNew);
            tail = elements.length + 1;
            head = 0;
            elementsNew[elements.length] = element;
            elements = elementsNew;
        } else {
            elements[tail] = element;
            tail = (tail + 1) % elements.length;
        }
        size++;
    }

    /*
     * Name: size()
     * Pred: queue != null
     * Post: R <- queue.size
     */
    public static int size() {
        return size;
    }

    /**
     * Name: isEmpty()
     * Pred: queue != null && queue != null;
     * Post: R <- queue.size <=> 0
     */
    public static boolean isEmpty() {
        return size == 0;
    }

    /*
     * Name: element();
     * Pred: queue.head in [0..n - 1];
     * Post: n' = queue.head && R = a[n'];
     */
    public static Object element() {
        return elements[head];
    }

    /*
     * Name: dequeue();
     * Pred: queue.head in [0..n - 1];
     * Post: n' := queue.head && R <- a[n'];
     */
    public static Object dequeue() {
        final Object v = elements[head];
        elements[head] = null;
        head = (head + 1) % elements.length;
        size--;
        return v;
    }

    /*
     * Name: clear()
     * Pred: true
     * Post: R <- (forall i := [0..n - 1] : a[i] == null) && (queue.head := queue.tail := queue.tail := 0)
     */
    public static void clear() {
        elements = new Object[START_SIZE];
        size = 0;
        head = 0;
        tail = 0;
    }

    /*
     * Name: count()
     * Pred: tail >= 0 && head >= 0 && elements != null
     * Post: R <- v : forall i : elements[i] == element -> count++;
     */
    public static int count(final Object element) {
        checkNull(element);
        if (size == 0) {
            return 0;
        }
        int count = 0;
        if (head < tail) {
            for (int i = head; i < tail; i++) {
                count = incCount(element, i, count);
            }
        } else {
            for (int i = head; i < elements.length; i++) {
                count = incCount(element, i, count);
            }
            for (int i = 0; i < tail; i++) {
                count = incCount(element, i, count);
            }
        }
        return count;
    }

    private static void checkNull(final Object element) {
        Objects.requireNonNull(element);
    }

    private static int incCount(final Object element, final int i, int count) {
        if (elements[i] != null && elements[i].equals(element)) {
            count++;
        }
        return count;
    }

    private static void ensureCapacity(final Object[] elementsNew) {
        int index = 0;
        if (head < tail) {
            for (int i = head; i < tail; i++) {
                elementsNew[index++] = elements[i];
            }
        } else {
            for (int i = head; i < elements.length; i++) {
                elementsNew[index++] = elements[i];
            }
            for (int i = 0; i < tail; i++) {
                elementsNew[index++] = elements[i];
            }
        }
    }
}
