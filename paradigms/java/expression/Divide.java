package expression;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Divide extends BinaryOperator {
	public Divide(final CommonExpression a, final CommonExpression b) {
		super(a, b, "/");
	}

	@Override
	public int calculation(int l, int r) {
		return l / r;
	}
}
