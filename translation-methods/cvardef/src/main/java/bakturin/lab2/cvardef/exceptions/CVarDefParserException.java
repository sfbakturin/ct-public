package bakturin.lab2.cvardef.exceptions;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CVarDefParserException extends CVarDefException {
	public CVarDefParserException() {
		super("Parsing string error.");
	}

	public CVarDefParserException(final int pos, final String expected) {
		super("At %d expected %s.".formatted(pos, expected));
	}
}
