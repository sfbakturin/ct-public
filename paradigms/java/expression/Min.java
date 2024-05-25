package expression;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Min extends BinaryOperator {
	public Min(final CommonExpression a, final CommonExpression b) {
		super(a, b, "min");
	}

	@Override
	protected int calculation(int l, int r) {
		return (l > r) ? r : l;
	}
}
