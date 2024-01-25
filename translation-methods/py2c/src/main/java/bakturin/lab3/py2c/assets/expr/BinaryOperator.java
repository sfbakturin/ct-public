package bakturin.lab3.py2c.assets.expr;

import bakturin.lab3.py2c.assets.CExpression;
import bakturin.lab3.py2c.assets.CType;
import bakturin.lab3.py2c.exceptions.Py2CIncompatibleTypesException;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public abstract class BinaryOperator implements CExpression {
	private final CExpression left;
	private final CExpression right;

	protected BinaryOperator(final CExpression left, final CExpression right) {
		this.left = left;
		this.right = right;
	}

	@Override
	public CType getType() {
		if (((left.getType() == CType.FLOAT || left.getType() == CType.INT) && right.getType() == CType.STRING) || ((right.getType() == CType.FLOAT || right.getType() == CType.INT) && left.getType() == CType.STRING)) {
			throw new Py2CIncompatibleTypesException(left.getType(), right.getType());
		}
		if (left.getType() == CType.FLOAT || right.getType() == CType.FLOAT) {
			return CType.FLOAT;
		} else {
			return left.getType();
		}
	}

	@Override
	public String asExpression() {
		return "(" + binFormat().formatted(left.asExpression(), right.asExpression()) + ")";
	}

	protected abstract String binFormat();
}
