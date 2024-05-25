package expression.exceptions.custom;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class EvaluateException extends RuntimeException {
	public EvaluateException(final String message) {
		super(message);
	}

	public EvaluateException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
