package queue;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class LinkedQueue extends AbstractQueue {
	private LinkedQueueNode head, tail;

	public void enqueueImpl(final Object element) {
		final LinkedQueueNode item = new LinkedQueueNode(element);
		if (head == null) {
			this.head = item;
			this.tail = head;
		} else {
			this.tail.node = item;
			this.tail = tail.node;
		}
	}

	public void dequeueImpl() {
		this.head = this.head.node;
	}

	public Object elementImpl() {
		return this.head.value;
	}

	public void clearImpl() {
		this.head = null;
		this.tail = null;
	}

	private static class LinkedQueueNode {
		private final Object value;
		private LinkedQueueNode node;

		public LinkedQueueNode(final Object value) {
			this.value = value;
			this.node = null;
		}
	}
}
