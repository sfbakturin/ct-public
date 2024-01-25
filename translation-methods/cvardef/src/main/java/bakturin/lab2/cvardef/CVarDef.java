package bakturin.lab2.cvardef;

import java.util.List;
import java.util.Objects;

import bakturin.lab2.cvardef.assets.CVarDefParser;
import bakturin.lab2.cvardef.assets.CVarDefTree;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CVarDef {
	public static CVarDefTree getParsingTree(final boolean exceptions, final String expression) {
		final CVarDefParser p = new CVarDefParser(exceptions);
		return p.parse(expression);
	}

	public static List<CVarDefVariable> parse(final String expression) {
		final CVarDefTree t = getParsingTree(true, expression);
		return t.getVariables();
	}

	public static List<CVarDefVariable> parseNull(final String expression) {
		final CVarDefTree t = getParsingTree(false, expression);

		if (Objects.isNull(t)) {
			return null;
		}

		return t.getVariables();
	}
}
