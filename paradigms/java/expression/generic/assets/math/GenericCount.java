package expression.generic.assets.math;

import expression.generic.assets.operation.GenericOperationType;
import expression.generic.assets.parser.GenericExpression;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class GenericCount<T> implements GenericExpression<T> {
	private final GenericExpression<T> a;
	private final GenericOperationType<T> mode;

	public GenericCount(final GenericExpression<T> a, final GenericOperationType<T> mode) {
		this.a = a;
		this.mode = mode;
	}

	@Override
	public String toString() {
		return "count" + "(" + this.a + ")";
	}

	@Override
	public T evaluate(final T x, final T y, final T z) {
		return this.mode.count(this.a.evaluate(x, y, z));
	}
}
