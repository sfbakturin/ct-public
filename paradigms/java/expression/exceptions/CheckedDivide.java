package expression.exceptions;

import expression.BinaryOperator;
import expression.CommonExpression;
import expression.exceptions.custom.EvaluateDivisionByZeroException;
import expression.exceptions.custom.EvaluateException;
import expression.exceptions.custom.EvaluateOverflowException;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CheckedDivide extends BinaryOperator {
	private static final int MIN = Integer.MIN_VALUE;

	public CheckedDivide(final CommonExpression a, final CommonExpression b) {
		super(a, b, "/");
	}

	@Override
	public int calculation(int l, int r) throws EvaluateException {
		if (r != 0 && !(l == MIN && r == -1)) {
			return l / r;
		} else {
			if (r != 0) {
				throw new EvaluateOverflowException();
			} else {
				throw new EvaluateDivisionByZeroException();
			}
		}
	}
}
