import java.util.concurrent.atomic.*;
import java.util.Objects;

public class Solution implements Lock<Solution.Node> {
    private final Environment env;
    private final AtomicReference<Node> tail = new AtomicReference<>(null);

    public Solution(final Environment env) {
        this.env = env;
    }

    @Override
    public Node lock() {
        final Node my = new Node(); // сделали узел
        my.locked.set(true);
        final Node pred = this.tail.getAndSet(my);
        if (Objects.nonNull(pred)) {
            pred.next.set(my);
            while (my.locked.get()) {
                this.env.park();
            }
        }
        return my; // вернули узел
    }

    @Override
    public void unlock(final Node my) {
        if (Objects.isNull(my.next.get())) {
            if (this.tail.compareAndSet(my, null)) {
                return;
            } else {
                while (Objects.isNull(my.next.get())) {
                    // this.env.park();
                }
            }
        }
        final Node node = my.next.get();
        node.locked.set(false);
        this.env.unpark(node.thread);
    }

    static class Node {
        public final Thread thread = Thread.currentThread(); // запоминаем поток, которые создал узел
        public final AtomicReference<Boolean> locked = new AtomicReference<>(false);
        public final AtomicReference<Node> next = new AtomicReference<>(null);
    }
}
