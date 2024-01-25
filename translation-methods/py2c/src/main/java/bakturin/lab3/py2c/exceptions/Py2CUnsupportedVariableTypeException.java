package bakturin.lab3.py2c.exceptions;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CUnsupportedVariableTypeException extends Py2CTranslationException {
	public Py2CUnsupportedVariableTypeException() {
		super("unsupported variable type definition");
	}
}
