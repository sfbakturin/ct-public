package bakturin.lab3.py2c.exceptions;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CCompileException extends Py2CException {
	public Py2CCompileException() {
		this("unknown error");
	}

	public Py2CCompileException(final String message) {
		super("Compile error: " + message + ".");
	}
}
