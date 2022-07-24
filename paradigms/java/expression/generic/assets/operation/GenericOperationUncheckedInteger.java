package expression.generic.assets.operation;

import expression.exceptions.custom.EvaluateDivisionByZeroException;
import expression.exceptions.custom.ParserException;
import expression.generic.assets.math.GenericConst;
import expression.generic.assets.parser.GenericExpression;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class GenericOperationUncheckedInteger extends GenericOperationType<Integer> {
	@Override
	public Integer add(final Integer l, final Integer r) {
		return l + r;
	}

	@Override
	public Integer subtract(final Integer l, final Integer r) {
		return l - r;
	}

	@Override
	public Integer multiply(final Integer l, final Integer r) {
		return l * r;
	}

	@Override
	public Integer divide(final Integer l, final Integer r) {
		if (r != 0) {
			return l / r;
		} else {
			throw new EvaluateDivisionByZeroException();
		}
	}

	@Override
	public GenericExpression<Integer> createConst(final String s) {
		try {
			return new GenericConst<>(Integer.parseInt(s));
		} catch (final NumberFormatException err) {
			throw new ParserException("Wrong number, it's overflowed");
		}
	}

	@Override
	public Integer generate(final String s) {
		return Integer.parseInt(s);
	}

	@Override
	public Integer count(final Integer a) {
		return Integer.bitCount(a);
	}

	@Override
	public Integer min(final Integer a, final Integer b) {
		return (a > b) ? b : a;
	}

	@Override
	public Integer max(final Integer a, final Integer b) {
		return (a > b) ? a : b;
	}
}
