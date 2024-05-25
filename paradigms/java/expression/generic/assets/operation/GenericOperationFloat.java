package expression.generic.assets.operation;

import expression.exceptions.custom.ParserException;
import expression.generic.assets.math.GenericConst;
import expression.generic.assets.parser.GenericExpression;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class GenericOperationFloat extends GenericOperationType<Float> {
	@Override
	public Float add(final Float l, final Float r) {
		return l + r;
	}

	@Override
	public Float subtract(final Float l, final Float r) {
		return l - r;
	}

	@Override
	public Float multiply(final Float l, final Float r) {
		return l * r;
	}

	@Override
	public Float divide(final Float l, final Float r) {
		return l / r;
	}

	@Override
	public GenericExpression<Float> createConst(final String s) {
		try {
			return new GenericConst<>(Float.parseFloat(s));
		} catch (final NumberFormatException err) {
			throw new ParserException("Wrong number, it's overflowed");
		}
	}

	@Override
	public Float generate(final String s) {
		return Float.parseFloat(s);
	}

	@Override
	public Float count(final Float a) {
		return (float) Integer.bitCount(Float.floatToIntBits(a));
	}

	@Override
	public Float min(final Float a, final Float b) {
		if (a.isNaN()) {
			return Float.NaN;
		}

		if (b.isNaN()) {
			return Float.NaN;
		}

		return Float.compare(a, b) < 0 ? a : b;
	}

	@Override
	public Float max(final Float a, final Float b) {
		if (a.isNaN()) {
			return Float.NaN;
		}

		if (b.isNaN()) {
			return Float.NaN;
		}

		return Float.compare(a, b) > 0 ? a : b;
	}
}
