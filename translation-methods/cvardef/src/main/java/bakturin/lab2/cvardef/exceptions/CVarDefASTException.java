package bakturin.lab2.cvardef.exceptions;

import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CVarDefASTException extends CVarDefException {
	public CVarDefASTException() {
		super("AST parsing error.");
	}

	public CVarDefASTException(final String item, final int possibleNumber) {
		this("possible number of" + " " + "'" + item + "'" + " " + "is" + " " + possibleNumber);
	}

	public CVarDefASTException(final String type, final String... types) {
		this("when its" + " " + "'" + type + "'" + " " + "there should be none of" + " "
				+ String.join(",", types));
	}

	public CVarDefASTException(final String message) {
		super("AST error:" + " " + message + ".");
	}
}
