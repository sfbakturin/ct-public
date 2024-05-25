package expression.parser;

import expression.CommonExpression;
import expression.util.CharSource;
import expression.util.StringSource;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class ExpressionParser implements TripleParser {
	@Override
	public CommonExpression parse(final String expression) {
		return parse(new StringSource(expression));
	}

	private static CommonExpression parse(final CharSource source) {
		return new Parsersizing(source).parse();
	}
}
