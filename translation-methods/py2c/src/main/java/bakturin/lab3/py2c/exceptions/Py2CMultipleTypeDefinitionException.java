package bakturin.lab3.py2c.exceptions;

import bakturin.lab3.py2c.assets.CType;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CMultipleTypeDefinitionException extends Py2CTranslationException {
	public Py2CMultipleTypeDefinitionException(final CType left, final CType right) {
		super("reset type from" + " " + "\"" + left + "\"" + " " + "to" + " " + "\"" + right + "\"");
	}
}
