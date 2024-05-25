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

public class GenericOperationLong extends GenericOperationType<Long> {
	@Override
	public Long add(final Long l, final Long r) {
		return l + r;
	}

	@Override
	public Long subtract(final Long l, final Long r) {
		return l - r;
	}

	@Override
	public Long multiply(final Long l, final Long r) {
		return l * r;
	}

	@Override
	public Long divide(final Long l, final Long r) {
		if (r != 0) {
			return l / r;
		} else {
			throw new EvaluateDivisionByZeroException();
		}
	}

	@Override
	public GenericExpression<Long> createConst(final String s) {
		try {
			return new GenericConst<>(Long.parseLong(s));
		} catch (final NumberFormatException err) {
			throw new ParserException("Wrong number, it's overflowed");
		}
	}

	@Override
	public Long generate(final String s) {
		return Long.parseLong(s);
	}

	@Override
	public Long count(final Long a) {
		return (long) Long.bitCount(a);
	}

	@Override
	public Long min(final Long a, final Long b) {
		return (a > b) ? b : a;
	}

	@Override
	public Long max(final Long a, final Long b) {
		return (a > b) ? a : b;
	}
}
