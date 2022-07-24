package expression.generic.assets.math;

import expression.generic.assets.parser.GenericExpression;
import expression.generic.assets.operation.GenericOperationType;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class GenericAdd<T> extends GenericBinaryOperator<T> {
	private final GenericOperationType<T> mode;

	public GenericAdd(final GenericExpression<T> a, final GenericExpression<T> b, final GenericOperationType<T> mode) {
		super(a, b, "+");
		this.mode = mode;
	}

	@Override
	public T calculation(final T l, final T r) {
		return this.mode.add(l, r);
	}
}
