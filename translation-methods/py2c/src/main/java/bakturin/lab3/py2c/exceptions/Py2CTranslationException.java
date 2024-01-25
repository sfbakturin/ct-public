package bakturin.lab3.py2c.exceptions;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CTranslationException extends Py2CException {
	public Py2CTranslationException() {
		this("unknown error");
	}

	public Py2CTranslationException(final String message) {
		super("Translation error: " + message + ".");
	}
}
