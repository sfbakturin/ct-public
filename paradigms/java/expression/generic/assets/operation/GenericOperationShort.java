package expression.generic.assets.operation;

import expression.exceptions.custom.EvaluateDivisionByZeroException;;
import expression.generic.assets.math.GenericConst;
import expression.generic.assets.parser.GenericExpression;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class GenericOperationShort extends GenericOperationType<Short> {
    @Override
    public Short add(final Short l, final Short r) {
        return this.s(l + r);
    }

    @Override
    public Short subtract(final Short l, final Short r) {
        return this.s(l - r);
    }

    @Override
    public Short multiply(final Short l, final Short r) {
        return this.s(l * r);
    }

    @Override
    public Short divide(final Short l, final Short r) {
        if (r != 0) {
            return this.s(l / r);
        } else {
            throw new EvaluateDivisionByZeroException();
        }
    }

    @Override
    public GenericExpression<Short> createConst(final String s) {
        try {
            return new GenericConst<>(Short.parseShort(s));
        } catch (final NumberFormatException err) {
            return new GenericConst<>(this.s(Integer.parseInt(s)));
        }
    }

    @Override
    public Short generate(final String s) {
        try {
            return Short.parseShort(s);
        } catch (final NumberFormatException err) {
            return this.s(Integer.parseInt(s));
        }
    }

    @Override
    public Short count(final Short a) {
        return this.s(Integer.bitCount(a & 0xffff));
    }

    @Override
    public Short min(final Short a, final Short b) {
        return (a > b) ? b : a;
    }

    @Override
    public Short max(final Short a, final Short b) {
        return (a > b) ? a : b;
    }

    private Short s(final int i) {
        return (short) i;
    }
}
