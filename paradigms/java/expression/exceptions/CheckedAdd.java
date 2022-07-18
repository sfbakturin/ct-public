package expression.exceptions;

import expression.BinaryOperator;
import expression.CommonExpression;
import expression.exceptions.custom.EvaluateException;
import expression.exceptions.custom.EvaluateOverflowException;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class CheckedAdd extends BinaryOperator {
    private static final int MIN = Integer.MIN_VALUE;
    private static final int MAX = Integer.MAX_VALUE;

    public CheckedAdd(final CommonExpression a, final CommonExpression b) {
        super(a, b, "+");
    }

    @Override
    public int calculation(int l, int r) throws EvaluateException {
        if ((r >= 0 && (MAX - r >= l)) || (r < 0 && (MIN - r <= l)) || (l >= 0 && (MAX - l >= r)) || (l < 0 && (MIN - l <= r))) {
            return l + r;
        } else {
            throw new EvaluateOverflowException();
        }
    }
}
