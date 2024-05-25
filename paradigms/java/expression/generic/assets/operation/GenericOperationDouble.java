package expression.generic.assets.operation;

import expression.generic.assets.math.GenericConst;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class GenericOperationDouble extends GenericOperationType<Double> {
	@Override
	public Double add(final Double a, final Double b) {
		return a + b;
	}

	@Override
	public Double subtract(final Double a, final Double b) {
		return a - b;
	}

	@Override
	public Double multiply(final Double a, final Double b) {
		return a * b;
	}

	@Override
	public Double divide(final Double a, final Double b) {
		return a / b;
	}

	@Override
	public GenericConst<Double> createConst(final String s) {
		return new GenericConst<>(Double.parseDouble(s));
	}

	@Override
	public Double generate(final String s) {
		return Double.parseDouble(s);
	}

	@Override
	public Double count(final Double a) {
		return (double) Long.bitCount(Double.doubleToLongBits(a));
	}

	@Override
	public Double min(final Double a, final Double b) {
		if (a.isNaN()) {
			return Double.NaN;
		}

		if (b.isNaN()) {
			return Double.NaN;
		}

		return Double.compare(a, b) < 0 ? a : b;
	}

	@Override
	public Double max(final Double a, final Double b) {
		if (a.isNaN()) {
			return Double.NaN;
		}

		if (b.isNaN()) {
			return Double.NaN;
		}

		return Double.compare(a, b) > 0 ? a : b;
	}
}
