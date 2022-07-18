package expression.generic.assets.math;

import expression.exceptions.custom.ParserException;
import expression.generic.assets.parser.GenericExpression;

import java.util.Objects;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class GenericVariable<T> implements GenericExpression<T> {
    private final String s;

    public GenericVariable(final String s) {
        this.s = s;
    }

    @Override
    public String toString() {
        return this.s;
    }

    @Override
    public T evaluate(final T x, final T y, final T z) {
        if (this.s.equals("x")) {
            return x;
        } else {
            if (this.s.equals("y")) {
                return y;
            } else {
                if (this.s.equals("z")) {
                    return z;
                } else {
                    throw new ParserException("no such variable can be found");
                }
            }
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final GenericVariable casted = (GenericVariable) o;
        return Objects.equals(s, casted.s);
    }

    @Override
    public int hashCode() {
        return this.s.hashCode();
    }
}
