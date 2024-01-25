import java.util.concurrent.atomic.*

/**
 * @author Bakturin Saveliy
 *
 * TODO: Copy the code from `FAABasedQueueSimplified` TODO: and implement the infinite array on a
 * linked list TODO: of fixed-size `Segment`s.
 */
class FAABasedQueue<E> : Queue<E> {
    private val enqIdx: AtomicLong
    private val deqIdx: AtomicLong
    private val head: AtomicReference<Segment>
    private val tail: AtomicReference<Segment>

    init {
        val dummy = Segment(0)
        enqIdx = AtomicLong(0)
        deqIdx = AtomicLong(0)
        head = AtomicReference(dummy)
        tail = AtomicReference(dummy)
    }

    private fun findSegment(startSegment: Segment, endId: Long): Segment {
        var transport = startSegment
        while (transport.id < endId) {
            if (transport.next.get() == null &&
                            transport.next.compareAndSet(null, Segment(transport.id + 1))
            ) {
                continue
            }
            transport = transport.next.get()!!
        }
        return transport
    }

    private fun moveTailForward(startSegment: Segment, flagIfHead: Boolean = false) {
        if (flagIfHead) {
            val curHead = head.get()
            if (curHead.id < startSegment.id) {
                head.compareAndSet(curHead, startSegment)
            }
        } else {
            val curTail = tail.get()
            if (curTail.id < startSegment.id) {
                tail.compareAndSet(curTail, startSegment)
            }
        }
    }

    override fun enqueue(element: E) {
        // TODO("Implement me!")
        while (true) {
            val curTail = tail.get()
            val curIndex = enqIdx.getAndIncrement()
            val curSegment = findSegment(curTail, curIndex / SEGMENT_SIZE)
            moveTailForward(curSegment)
            if (curSegment.cells.compareAndSet(curIndex.toInt() % SEGMENT_SIZE, null, element)) {
                return
            }
        }
    }

    override fun dequeue(): E? {
        // TODO("Implement me!")
        while (true) {
            if (!shouldTryToDeque()) {
                return null
            } else {
                val curHead = head.get()
                val curIndex = deqIdx.getAndIncrement()
                val curSegment = findSegment(curHead, curIndex / SEGMENT_SIZE)
                moveTailForward(curSegment, true)
                if (!curSegment.cells.compareAndSet(curIndex.toInt() % SEGMENT_SIZE, null, POISONED)
                ) {
                    val ret = curSegment.cells.get(curIndex.toInt() % SEGMENT_SIZE) as E
                    curSegment.cells.set(curIndex.toInt() % SEGMENT_SIZE, null)
                    return ret
                }
            }
        }
    }

    private fun shouldTryToDeque(): Boolean {
        while (true) {
            var curDeqIdx = deqIdx.get()
            var curEnqIdx = enqIdx.get()
            if (curDeqIdx == deqIdx.get()) {
                return curDeqIdx < curEnqIdx
            }
        }
    }
}

private class Segment(val id: Long) {
    val enqIdx = AtomicLong(0)
    val deqIdx = AtomicLong(0)
    val next = AtomicReference<Segment?>(null)
    val cells = AtomicReferenceArray<Any?>(SEGMENT_SIZE)
}

// DO NOT CHANGE THIS CONSTANT
private const val SEGMENT_SIZE = 2

// TODO: poison cells with this value.
private val POISONED = Any()
