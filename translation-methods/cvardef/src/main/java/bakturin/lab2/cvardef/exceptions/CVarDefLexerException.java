package bakturin.lab2.cvardef.exceptions;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CVarDefLexerException extends CVarDefException {
	public CVarDefLexerException() {
		super("Parsing string error.");
	}

	public CVarDefLexerException(final int pos, final String message) {
		super("Error reading at %d: %s.".formatted(pos, message));
	}
}
