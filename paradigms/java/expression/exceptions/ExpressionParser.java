package expression.exceptions;

import expression.CommonExpression;
import expression.util.CharSource;
import expression.util.StringSource;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class ExpressionParser implements TripleParser {
    @Override
    public CommonExpression parse(final String expression) {
        return parse(new StringSource(expression));
    }

    private static CommonExpression parse(final CharSource source) {
        return new CheckedParsersizing(source).parse();
    }
}
