package expression;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class L0 implements CommonExpression {
	final CommonExpression a;

	public L0(final CommonExpression a) {
		this.a = a;
	}

	@Override
	public String toString() {
		return "l0(" + this.a.toString() + ")";
	}

	@Override
	public int evaluate(int x) {
		return Integer.numberOfLeadingZeros(this.evaluate(x, 0, 0));
	}

	@Override
	public int evaluate(int x, int y, int z) {
		return Integer.numberOfLeadingZeros(this.a.evaluate(x, y, z));
	}
}
