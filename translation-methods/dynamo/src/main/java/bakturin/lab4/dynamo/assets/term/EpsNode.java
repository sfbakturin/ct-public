package bakturin.lab4.dynamo.assets.term;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class EpsNode implements TerminalNode {
	private final static String FMT_CREATE = """
			if (parsed.length() == 1 && curChar == -1) {
			curToken = %sToken.EPSILON;
			return;
			}""";

	@Override
	public boolean isRegular() {
		return false;
	}

	@Override
	public boolean isString() {
		return false;
	}

	@Override
	public boolean isEpsilon() {
		return true;
	}

	@Override
	public boolean isStart() {
		return false;
	}

	@Override
	public String asString() {
		return "<end>";
	}

	@Override
	public String name() {
		return "EPSILON";
	}

	@Override
	public String create(final String grammar, final String name) {
		return FMT_CREATE.formatted(grammar);
	}

	@Override
	public boolean equals(final Object other) {
		return other instanceof EpsNode;
	}

	@Override
	public int hashCode() {
		return FMT_CREATE.hashCode();
	}
}
