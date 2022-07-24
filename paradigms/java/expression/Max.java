package expression;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Max extends BinaryOperator {
	public Max(final CommonExpression a, final CommonExpression b) {
		super(a, b, "max");
	}

	@Override
	protected int calculation(int l, int r) {
		return (l > r) ? l : r;
	}
}
