import java.util.concurrent.atomic.*

/**
 * @author Bakturin Saveliy
 */
class MSQueue<E> : Queue<E> {
    private val head: AtomicReference<Node<E>>
    private val tail: AtomicReference<Node<E>>

    init {
        val dummy = Node<E>(null)
        head = AtomicReference(dummy)
        tail = AtomicReference(dummy)
    }

    override fun enqueue(element: E) {
        val newNode = Node<E>(element)
        while (true) {
            val curTail = tail.get()
            val curNext = curTail.next
            if (curTail == tail.get()) {
                if (curNext.get() == null) {
                    if (curTail.next.compareAndSet(null, newNode)) {
                        tail.compareAndSet(curTail, newNode)
                        return
                    }
                } else {
                    tail.compareAndSet(curTail, curNext.get())
                }
            }
        }
    }

    override fun dequeue(): E? {
        while (true) {
            val curHead = head.get()
            val curTail = tail.get()
            val curNext = curHead.next
            if (curHead == head.get()) {
                if (curHead == curTail) {
                    if (curNext.get() == null) {
                        return null
                    }
                    tail.compareAndSet(curTail, curNext.get())
                } else {
                    val ret = curNext.get()!!.element
                    if (head.compareAndSet(curHead, curNext.get())) {
                        head.get().element = null
                        return ret
                    }
                }
            }
        }
    }

    // FOR TEST PURPOSE, DO NOT CHANGE IT.
    override fun validate() {
        check(tail.get().next.get() == null) {
            "At the end of the execution, `tail.next` must be `null`"
        }
        check(head.get().element == null) {
            "At the end of the execution, the dummy node shouldn't store an element"
        }
    }

    private class Node<E>(var element: E?) {
        val next = AtomicReference<Node<E>?>(null)
    }
}
