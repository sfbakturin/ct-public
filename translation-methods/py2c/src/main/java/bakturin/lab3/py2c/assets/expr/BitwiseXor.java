package bakturin.lab3.py2c.assets.expr;

import bakturin.lab3.py2c.assets.CExpression;
import bakturin.lab3.py2c.assets.CExpressionType;
import bakturin.lab3.py2c.assets.CType;
import bakturin.lab3.py2c.exceptions.Py2CUnsupportedBinaryExpressionOperationException;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class BitwiseXor extends BinaryOperator {
	public BitwiseXor(final CExpression left, final CExpression right) {
		super(left, right);
	}

	@Override
	protected String binFormat() {
		final CType type = getType();
		if (type == CType.STRING || type == CType.FLOAT) {
			throw new Py2CUnsupportedBinaryExpressionOperationException(CExpressionType.BITWISE_XOR, type);
		} else {
			return "%s ^ %s";
		}
	}
}
