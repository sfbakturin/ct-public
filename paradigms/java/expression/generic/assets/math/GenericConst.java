package expression.generic.assets.math;

import expression.generic.assets.parser.GenericExpression;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class GenericConst<T> implements GenericExpression<T> {
	private final T constant;

	public GenericConst(final T constant) {
		this.constant = constant;
	}

	@Override
	public String toString() {
		return String.valueOf(this.constant);
	}

	@Override
	public T evaluate(final T x, final T y, final T z) {
		return this.constant;
	}

	public String evaluate() {
		return String.valueOf(this.constant);
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		final GenericConst casted = (GenericConst) o;
		return constant == casted.constant;
	}

	@Override
	public int hashCode() {
		return this.constant.hashCode();
	}
}
