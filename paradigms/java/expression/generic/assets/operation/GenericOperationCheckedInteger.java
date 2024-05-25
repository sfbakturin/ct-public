package expression.generic.assets.operation;

import expression.exceptions.custom.EvaluateDivisionByZeroException;
import expression.exceptions.custom.EvaluateOverflowException;
import expression.exceptions.custom.ParserException;
import expression.generic.assets.parser.GenericExpression;
import expression.generic.assets.math.GenericConst;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class GenericOperationCheckedInteger extends GenericOperationType<Integer> {
	private static final int MAX = Integer.MAX_VALUE;
	private static final int MIN = Integer.MIN_VALUE;

	@Override
	public Integer add(final Integer l, final Integer r) {
		if ((r >= 0 && (MAX - r >= l)) || (r < 0 && (MIN - r <= l)) || (l >= 0 && (MAX - l >= r)) || (l < 0 && (MIN - l <= r))) {
			return l + r;
		} else {
			throw new EvaluateOverflowException();
		}
	}

	@Override
	public Integer subtract(final Integer l, final Integer r) {
		if ((r >= 0 && (MIN + r <= l)) || (r < 0 && (MAX + r >= l))) {
			return l - r;
		} else {
			throw new EvaluateOverflowException();
		}
	}

	@Override
	public Integer multiply(final Integer l, final Integer r) {
		if (r == 0 || l == 0 || ((r * l / l == r) && !((r == MIN && l == -1) || ((l == MIN && r == -1))))) {
			return l * r;
		} else {
			throw new EvaluateOverflowException();
		}
	}

	@Override
	public Integer divide(final Integer l, final Integer r) {
		if (r != 0 && !(l == MIN && r == -1)) {
			return l / r;
		} else {
			if (r != 0) {
				throw new EvaluateDivisionByZeroException();
			} else {
				throw new EvaluateOverflowException();
			}
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
