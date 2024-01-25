package bakturin.lab3.py2c.exceptions;

import bakturin.lab3.py2c.assets.CType;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CIncompatibleTypesException extends Py2CUnsupportedOperationException {
	public Py2CIncompatibleTypesException(final CType left, final CType right) {
		super("incompatible types: \"" + left.toString() + "\", \"" + right.toString() + "\"");
	}
}
