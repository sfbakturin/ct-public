package expression;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class T0 implements CommonExpression {
	final CommonExpression a;

	public T0(final CommonExpression a) {
		this.a = a;
	}

	@Override
	public String toString() {
		return "t0(" + this.a.toString() + ')';
	}

	@Override
	public int evaluate(int x) {
		return Integer.numberOfTrailingZeros(this.evaluate(x, 0, 0));
	}

	@Override
	public int evaluate(int x, int y, int z) {
		return Integer.numberOfTrailingZeros(this.a.evaluate(x, y, z));
	}
}
