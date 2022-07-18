package expression.generic.assets.math;

import expression.generic.assets.operation.GenericOperationType;
import expression.generic.assets.parser.GenericExpression;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class GenericMax<T> extends GenericBinaryOperator<T> {
    private final GenericOperationType<T> mode;

    public GenericMax(final GenericExpression<T> a, final GenericExpression<T> b, final GenericOperationType<T> mode) {
        super(a, b, "max");
        this.mode = mode;
    }

    @Override
    public T calculation(final T l, final T r) {
        return this.mode.max(l, r);
    }
}
