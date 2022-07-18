package expression;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class Subtract extends BinaryOperator {
    public Subtract(final CommonExpression a, final CommonExpression b) {
        super(a, b, "-");
    }

    @Override
    public int calculation(int l, int r) {
        return l - r;
    }
}
