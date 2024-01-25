import java.util.concurrent.atomic.*
import kotlin.math.*

/**
 * @author Bakturin Saveliy
 */
class FAABasedQueueSimplified<E> : Queue<E> {
    private val infiniteArray = AtomicReferenceArray<Any?>(1024) // conceptually infinite array
    private val enqIdx = AtomicLong(0)
    private val deqIdx = AtomicLong(0)

    override fun enqueue(element: E) {
        // TODO: Increment the counter atomically via Fetch-and-Add.
        // TODO: Use `getAndIncrement()` function for that.
        // TODO: Atomically install the element into the cell
        // TODO: if the cell is not poisoned.newValue
        while (true) {
            val i = enqIdx.getAndIncrement()
            if (infiniteArray.compareAndSet(i.toInt(), null, element)) {
                return
            }
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun dequeue(): E? {
        // Is this queue empty?
        // TODO: Increment the counter atomically via Fetch-and-Add.
        // TODO: Use `getAndIncrement()` function for that.
        // TODO: Try to retrieve an element if the cell contains an
        // TODO: element, poisoning the cell if it is empty.
        while (true) {
            if (!shouldTryToDeque()) {
                return null
            } else {
                val i = deqIdx.getAndIncrement()
                if (!infiniteArray.compareAndSet(i.toInt(), null, POISONED)) {
                    val ret = infiniteArray.get(i.toInt()) as E
                    infiniteArray.set(i.toInt(), null)
                    return ret
                }
            }
        }
    }

    private fun shouldTryToDeque(): Boolean {
        while (true) {
            var curDeqIdx = deqIdx
            var curEnqIdx = enqIdx
            if (curDeqIdx == deqIdx) {
                return curDeqIdx.toInt() < curEnqIdx.toInt()
            }
        }
    }

    override fun validate() {
        for (i in 0 until min(deqIdx.get().toInt(), enqIdx.get().toInt())) {
            check(infiniteArray[i] == null || infiniteArray[i] == POISONED) {
                "`infiniteArray[$i]` must be `null` or `POISONED` with `deqIdx = ${deqIdx.get()}` at the end of the execution"
            }
        }
        for (i in max(deqIdx.get().toInt(), enqIdx.get().toInt()) until infiniteArray.length()) {
            check(infiniteArray[i] == null || infiniteArray[i] == POISONED) {
                "`infiniteArray[$i]` must be `null` or `POISONED` with `enqIdx = ${enqIdx.get()}` at the end of the execution"
            }
        }
    }
}

// TODO: poison cells with this value.
private val POISONED = Any()
