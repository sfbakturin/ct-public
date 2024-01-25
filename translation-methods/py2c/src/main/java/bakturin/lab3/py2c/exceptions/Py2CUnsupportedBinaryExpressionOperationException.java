package bakturin.lab3.py2c.exceptions;

import bakturin.lab3.py2c.assets.CExpressionType;
import bakturin.lab3.py2c.assets.CType;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CUnsupportedBinaryExpressionOperationException extends Py2CUnsupportedOperationException {
	public Py2CUnsupportedBinaryExpressionOperationException(final CExpressionType expression, final CType type) {
		super("operation: \"" + expression.toString() + "\", with type: \"" + type.toString() + "\"");
	}
}
