package expression.generic.assets.parser;

import expression.generic.assets.operation.GenericOperationType;
import expression.util.CharSource;
import expression.util.StringSource;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class GenericExpressionParser<T> implements GenericParser<T> {
    private final GenericOperationType<T> mode;

    public GenericExpressionParser(final GenericOperationType<T> mode) {
        this.mode = mode;
    }

    @Override
    public GenericExpression<T> parse(final String expression) {
        return this.parse(new StringSource(expression));
    }

    private GenericExpression<T> parse(final CharSource source) {
        return new GenericCheckedParsersizing<>(source, this.mode).parse();
    }
}
