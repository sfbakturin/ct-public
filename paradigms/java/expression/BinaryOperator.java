package expression;

import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public abstract class BinaryOperator implements CommonExpression {
	private final CommonExpression a;
	private final CommonExpression b;
	private final String sign;

	protected BinaryOperator(final CommonExpression a, final CommonExpression b, final String sign) {
		this.a = a;
		this.b = b;
		this.sign = sign;
	}

	@Override
	public String toString() {
		return ('(' + this.a.toString() + ' ' + this.sign + ' ' + this.b.toString() + ')');
	}

	protected abstract int calculation(final int l, final int r);

	@Override
	public int evaluate(final int x) {
		return this.calculation(this.a.evaluate(x), this.b.evaluate(x));
	}

	@Override
	public int evaluate(final int x, final int y, final int z) {
		return this.calculation(this.a.evaluate(x, y, z), this.b.evaluate(x, y, z));
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof BinaryOperator that) {
			return that.getClass() == this.getClass() && Objects.equals(that.a, this.a) && Objects.equals(that.b, this.b);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return Objects.hash(a, b, getClass());
	}
}
