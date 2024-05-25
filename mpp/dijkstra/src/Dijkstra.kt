package dijkstra

import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicInteger
import kotlin.Comparator
import kotlin.concurrent.thread
import kotlinx.atomicfu.AtomicBoolean

private val NODE_DISTANCE_COMPARATOR =
        Comparator<Node> { o1, o2 -> Integer.compare(o1!!.distance, o2!!.distance) }

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    // The distance to the start node is `0`
    start.distance = 0
    // Create a priority (by distance) queue and add the start node into it
    val q = MultiPriorityQueue(workers, NODE_DISTANCE_COMPARATOR)
    q.add(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    val activeNodes = AtomicInteger(1)
    repeat(workers) {
        thread {
            while (activeNodes.get() > 0) {
                val u = q.delete()
                if (u == null) continue
                for (v in u.outgoingEdges) {
                    while (true) {
                        val old = v.to.distance
                        val d = u.distance + v.weight
                        if (old > d) {
                            if (v.to.casDistance(old, d)) {
                                q.insert(v.to)
                                activeNodes.incrementAndGet()
                                break
                            }
                        } else break
                    }
                }
                activeNodes.decrementAndGet()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}

class SinglePriorityQueue(comparator: Comparator<Node>) {
    private final val queue = PriorityQueue(comparator)
    private final val lock = java.util.concurrent.atomic.AtomicBoolean(false)

    public fun tryLock(): Boolean {
        return lock.compareAndSet(false, true)
    }

    public fun unlock() {
        lock.set(false)
    }

    public fun extractTop(): Node? {
        return queue.poll()
    }

    public fun top(): Node? {
        return queue.peek()
    }

    public fun add(task: Node) {
        queue.add(task)
    }
}

class MultiPriorityQueue(private val workers: Int, private val comparator: Comparator<Node>) {
    private final val C = 3
    private final val T = workers
    private final val queues = Array(C * T) { SinglePriorityQueue(comparator) }

    public fun add(task: Node) {
        insert(task)
    }

    public fun insert(task: Node) {
        while (true) {
            val q = queues.get(randomCellIndex())
            if (!q.tryLock()) continue
            q.add(task)
            q.unlock()
            break
        }
    }

    public fun delete(): Node? {
        while (true) {
            val (i1, i2) = distinctRandom()
            val q1 = queues.get(i1)
            val q2 = queues.get(i2)
            if (q1.top() == null && q2.top() == null) return null
            var q = q2
            if (q1.top() != null && (q2.top() == null || comparator.compare(q1.top(), q2.top()) < 0)) q = q1
            if (!q.tryLock()) continue
            val task = q.extractTop()
            q.unlock()
            return task
        }
    }

    private fun randomCellIndex(): Int = ThreadLocalRandom.current().nextInt(queues.size)

    private fun distinctRandom(): Pair<Int, Int> {
        val i1 = randomCellIndex()
        var i2 = randomCellIndex()
        while (i1 == i2) {
            i2 = randomCellIndex()
        }
        return Pair(i1, i2)
    }
}
