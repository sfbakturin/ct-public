import java.util.concurrent.*
import java.util.concurrent.atomic.*

/**
 *
 * @author Bakturin Saveliy
 */
class FlatCombiningQueue<E> : Queue<E> {
    private val queue = ArrayDeque<E>() // sequential queue
    private val combinerLock = AtomicBoolean(false) // unlocked initially
    private val tasksForCombiner = AtomicReferenceArray<Any?>(TASKS_FOR_COMBINER_SIZE)

    override fun enqueue(element: E) {
        // TODO: Make this code thread-safe using the flat-combining technique.
        // TODO: 1.  Try to become a combiner by
        // TODO:     changing `combinerLock` from `false` (unlocked) to `true` (locked).
        // TODO: 2a. On success, apply this operation and help others by traversing
        // TODO:     `tasksForCombiner`, performing the announced operations, and
        // TODO:      updating the corresponding cells to `Result`.
        // TODO: 2b. If the lock is already acquired, announce this operation in
        // TODO:     `tasksForCombiner` by replacing a random cell state from
        // TODO:      `null` with the element. Wait until either the cell state
        // TODO:      updates to `Result` (do not forget to clean it in this case),
        // TODO:      or `combinerLock` becomes available to acquire.
        var idx = randomCellIndex()
        var isWaiting = false
        while (true) {
            if (combinerLock.compareAndSet(false, true)) {
                if (isWaiting) {
                    val obj = tasksForCombiner.get(idx)
                    if (obj is Result<*>) {
                        tasksForCombiner.set(idx, null)
                        combinerLock.set(false)
                        return
                    } else {
                        tasksForCombiner.set(idx, null)
                    }
                }
                queue.addLast(element)
                for (i in 0..TASKS_FOR_COMBINER_SIZE - 1) {
                    val obj = tasksForCombiner.get(i)
                    val elemAsElement: E? = obj as? E?
                    if (obj is Dequeue) {
                        tasksForCombiner.set(i, Result<E?>(queue.removeFirstOrNull()))
                    } else if (elemAsElement != null && elemAsElement !is Result<*>) {
                        queue.addLast(elemAsElement as E)
                        tasksForCombiner.set(i, Result<E?>(elemAsElement))
                    }
                }
                combinerLock.set(false)
                return
            } else {
                if (tasksForCombiner.compareAndSet(idx, null, element)) {
                    isWaiting = true
                } else if (!isWaiting) {
                    idx = randomCellIndex()
                }
            }
        }
    }

    override fun dequeue(): E? {
        // TODO: Make this code thread-safe using the flat-combining technique.
        // TODO: 1.  Try to become a combiner by
        // TODO:     changing `combinerLock` from `false` (unlocked) to `true` (locked).
        // TODO: 2a. On success, apply this operation and help others by traversing
        // TODO:     `tasksForCombiner`, performing the announced operations, and
        // TODO:      updating the corresponding cells to `Result`.
        // TODO: 2b. If the lock is already acquired, announce this operation in
        // TODO:     `tasksForCombiner` by replacing a random cell state from
        // TODO:      `null` with `Dequeue`. Wait until either the cell state
        // TODO:      updates to `Result` (do not forget to clean it in this case),
        // TODO:      or `combinerLock` becomes available to acquire.
        var idx = randomCellIndex()
        var isWaiting = false
        while (true) {
            if (combinerLock.compareAndSet(false, true)) {
                if (isWaiting) {
                    val obj = tasksForCombiner.get(idx)
                    if (obj is Result<*>) {
                        val casted = obj as Result<E?>
                        tasksForCombiner.set(idx, null)
                        combinerLock.set(false)
                        return casted.value
                    } else {
                        tasksForCombiner.set(idx, null)
                    }
                }
                val ret = queue.removeFirstOrNull()
                for (i in 0..TASKS_FOR_COMBINER_SIZE - 1) {
                    val obj = tasksForCombiner.get(i)
                    val elem: E? = obj as? E?
                    if (obj is Dequeue) {
                        tasksForCombiner.set(i, Result<E?>(queue.removeFirstOrNull()))
                    } else if (elem != null && elem !is Result<*>) {
                        queue.addLast(elem as E)
                        tasksForCombiner.set(i, Result<E?>(elem))
                    }
                }
                combinerLock.set(false)
                return ret
            } else {
                if (tasksForCombiner.compareAndSet(idx, null, Dequeue)) {
                    isWaiting = true
                } else if (!isWaiting) {
                    idx = randomCellIndex()
                }
            }
        }
    }

    private fun randomCellIndex(): Int =
            ThreadLocalRandom.current().nextInt(tasksForCombiner.length())
}

private const val TASKS_FOR_COMBINER_SIZE = 3 // Do not change this constant!

// TODO: Put this token in `tasksForCombiner` for dequeue().
// TODO: enqueue()-s should put the inserting element.
private object Dequeue

// TODO: Put the result wrapped with `Result` when the operation in `tasksForCombiner` is processed.
private class Result<V>(val value: V)
