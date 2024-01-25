package bakturin.lab3.py2c.exceptions;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CUnsupportedOperationException extends Py2CException {
	public Py2CUnsupportedOperationException() {
		this("unknown error");
	}

	public Py2CUnsupportedOperationException(final String message) {
		super("Unsupported operation: " + message + ".");
	}
}
