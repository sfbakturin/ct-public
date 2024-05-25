package expression.generic.assets.math;

import expression.generic.assets.parser.GenericExpression;

import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public abstract class GenericBinaryOperator<T> implements GenericExpression<T> {
	private final GenericExpression<T> a;
	private final GenericExpression<T> b;
	private final String sign;

	protected GenericBinaryOperator(final GenericExpression<T> a, final GenericExpression<T> b, final String sign) {
		this.a = a;
		this.b = b;
		this.sign = sign;
	}

	@Override
	public String toString() {
		return ('(' + this.a.toString() + ' ' + this.sign + ' ' + this.b.toString() + ')');
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof GenericBinaryOperator that) {
			return that.getClass() == this.getClass() && Objects.equals(that.a, this.a) && Objects.equals(that.b, this.b);
		}
		return false;
	}

	protected abstract T calculation(final T l, final T r);

	@Override
	public T evaluate(final T x, final T y, final T z) {
		return this.calculation(this.a.evaluate(x, y, z), this.b.evaluate(x, y, z));
	}

	@Override
	public int hashCode() {
		return Objects.hash(a, b, getClass());
	}
}
