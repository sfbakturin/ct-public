package bakturin.lab3.py2c.exceptions;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CDefineNotVariableException extends Py2CCompileException {
	public Py2CDefineNotVariableException() {
		super("defining non variable");
	}
}
