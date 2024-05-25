package expression.exceptions.custom;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class EvaluateDivisionByZeroException extends EvaluateException {
	public EvaluateDivisionByZeroException() {
		super("division by zero");
	}
}
