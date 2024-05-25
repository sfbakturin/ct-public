package expression.generic.assets.math;

import expression.generic.assets.operation.GenericOperationType;
import expression.generic.assets.parser.GenericExpression;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class GenericMin<T> extends GenericBinaryOperator<T> {
	private final GenericOperationType<T> mode;

	public GenericMin(final GenericExpression<T> a, final GenericExpression<T> b, final GenericOperationType<T> mode) {
		super(a, b, "min");
		this.mode = mode;
	}

	@Override
	public T calculation(final T l, final T r) {
		return this.mode.min(l, r);
	}
}
