package bakturin.lab3.py2c.exceptions;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CUnsupportedBinaryOperatorException extends Py2CTranslationException {
	public Py2CUnsupportedBinaryOperatorException(final String op) {
		super("unsupported" + " " + "\"" + op + "\"" + " " + "operator for binary expressions");
	}
}
