package bakturin.lab4.dynamo.assets.term;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class StringNode implements TerminalNode {
	private final static String FMT_CREATE = """
			if (!prefix && parsed.equals("%s")) {
			curToken = %sToken.STRING_%s;
			found = true;
			prefix = true;
			} else {
			if (!prefix && parsed.length() < "%s".length()) {
			prefix = parsed.substring(0, parsed.length()).equals("%s".substring(0, parsed.length()));
			}
			}
			""";

	private final String expected;
	private final String name;

	public StringNode(final String expected, final String name) {
		this.expected = expected;
		this.name = name;
	}

	@Override
	public boolean isRegular() {
		return false;
	}

	@Override
	public boolean isString() {
		return true;
	}

	@Override
	public boolean isEpsilon() {
		return false;
	}

	@Override
	public boolean isStart() {
		return false;
	}

	@Override
	public String asString() {
		return this.expected;
	}

	@Override
	public String name() {
		return this.name;
	}

	@Override
	public String create(final String grammar, final String name) {
		return FMT_CREATE.formatted(expected, grammar, name, expected, expected);
	}
}
