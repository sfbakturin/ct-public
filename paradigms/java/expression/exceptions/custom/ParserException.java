package expression.exceptions.custom;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class ParserException extends RuntimeException {
	public ParserException(final String message) {
		super(message);
	}
}
