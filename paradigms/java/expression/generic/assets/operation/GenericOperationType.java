package expression.generic.assets.operation;

import expression.generic.assets.parser.GenericExpression;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public abstract class GenericOperationType<T> {
    public abstract T add(final T a, final T b);
    public abstract T subtract(final T a, final T b);
    public abstract T multiply(final T a, final T b);
    public abstract T divide(final T a, final T b);
    public T negate(final T a) {
        return this.multiply(a, this.generate("-1"));
    }
    public abstract GenericExpression<T> createConst(final String s);
    public abstract T generate(final String s);
    public abstract T count(final T a);
    public abstract T min(final T a, final T b);
    public abstract T max(final T a, final T b);
}
