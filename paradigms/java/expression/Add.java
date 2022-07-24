package expression;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Add extends BinaryOperator {
	public Add(final CommonExpression a, final CommonExpression b) {
		super(a, b, "+");
	}

	@Override
	public int calculation(int l, int r) {
		return l + r;
	}
}
