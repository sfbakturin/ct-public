package expression.exceptions.custom;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class EvaluateOverflowException extends EvaluateException {
	public EvaluateOverflowException() {
		super("overflow");
	}

	public EvaluateOverflowException(final String message) {
		super(message);
	}
}
