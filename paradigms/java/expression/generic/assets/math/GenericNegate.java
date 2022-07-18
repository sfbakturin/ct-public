package expression.generic.assets.math;

import expression.generic.assets.parser.GenericExpression;
import expression.generic.assets.operation.GenericOperationType;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class GenericNegate<T> implements GenericExpression<T> {
    private final GenericExpression<T> a;
    private final GenericOperationType<T> mode;

    public GenericNegate(final GenericExpression<T> a, final GenericOperationType<T> mode) {
        this.a = a;
        this.mode = mode;
    }

    @Override
    public String toString() {
        return "-" + "(" + this.a + ")";
    }

    @Override
    public T evaluate(final T x, final T y, final T z) {
        return this.mode.negate(this.a.evaluate(x, y, z));
    }
}
