package expression;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class Negate implements CommonExpression {
    final CommonExpression a;

    public Negate(final CommonExpression a) {
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
    public int evaluate(int x, int y, int z) {
        return (-1) * this.a.evaluate(x, y, z);
    }
}
