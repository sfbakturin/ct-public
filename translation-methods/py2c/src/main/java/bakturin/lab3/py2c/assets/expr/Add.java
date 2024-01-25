package bakturin.lab3.py2c.assets.expr;

import bakturin.lab3.py2c.assets.CExpression;
import bakturin.lab3.py2c.assets.CType;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Add extends BinaryOperator {
	public Add(final CExpression left, final CExpression right) {
		super(left, right);
	}

	@Override
	protected String binFormat() {
		if (getType() == CType.STRING) {
			return "std_string_concat(%s, %s)";
		} else {
			return "%s + %s";
		}
	}
}
