package expression.generic.assets.operation;

import expression.exceptions.custom.EvaluateDivisionByZeroException;
import expression.generic.assets.math.GenericConst;

import java.math.BigInteger;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class GenericOperationBigInteger extends GenericOperationType<BigInteger> {
	@Override
	public BigInteger add(final BigInteger a, final BigInteger b) {
		return a.add(b);
	}

	@Override
	public BigInteger subtract(final BigInteger a, final BigInteger b) {
		return a.subtract(b);
	}

	@Override
	public BigInteger multiply(final BigInteger a, final BigInteger b) {
		return a.multiply(b);
	}

	@Override
	public BigInteger divide(final BigInteger a, final BigInteger b) {
		if (!b.equals(BigInteger.ZERO)) {
			return a.divide(b);
		} else {
			throw new EvaluateDivisionByZeroException();
		}
	}

	@Override
	public GenericConst<BigInteger> createConst(final String s) {
		return new GenericConst<>(new BigInteger(s));
	}

	@Override
	public BigInteger generate(final String s) {
		return new BigInteger(s);
	}

	@Override
	public BigInteger count(final BigInteger a) {
		return BigInteger.valueOf(a.bitCount());
	}

	@Override
	public BigInteger min(final BigInteger a, final BigInteger b) {
		return a.compareTo(b) < 0 ? a : b;
	}

	@Override
	public BigInteger max(final BigInteger a, final BigInteger b) {
		return a.compareTo(b) > 0 ? a : b;
	}
}
