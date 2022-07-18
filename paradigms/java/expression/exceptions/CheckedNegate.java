package expression.exceptions;

import expression.CommonExpression;
import expression.exceptions.custom.EvaluateException;
import expression.exceptions.custom.EvaluateOverflowException;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class CheckedNegate implements CommonExpression {
    private static final int MIN = Integer.MIN_VALUE;
    final CommonExpression a;

    public CheckedNegate(final CommonExpression a) {
        this.a = a;
    }

    @Override
    public String toString() {
        return "-" + "(" + this.a + ")";
    }

    @Override
    public int evaluate(int x) {
        return this.evaluate(x, 0, 0);
    }

    @Override
    public int evaluate(int x, int y, int z) throws EvaluateException {
        final int temp = this.a.evaluate(x, y, z);
        if (temp != MIN) {
            return (-1) * temp;
        } else {
            throw new EvaluateOverflowException();
        }
    }
}
