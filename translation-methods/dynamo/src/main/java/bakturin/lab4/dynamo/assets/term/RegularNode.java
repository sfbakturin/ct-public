package bakturin.lab4.dynamo.assets.term;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class RegularNode implements TerminalNode {
	private final static String FMT_CREATE = """
			{
			final Pattern pat = Pattern.compile("%s");
			if (!prefix && pat.matcher(parsed).matches()) {
			curToken = %sToken.REGEX_%s;
			found = true;
			prefix = true;
			}
			}""";

	private final String expected;
	private final String name;

	public RegularNode(final String expected, final String name) {
		this.expected = expected;
		this.name = name;
	}

	@Override
	public boolean isRegular() {
		return true;
	}

	@Override
	public boolean isString() {
		return false;
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
		return "/" + this.expected + "/";
	}

	@Override
	public String name() {
		return this.name;
	}

	@Override
	public String create(final String grammar, final String name) {
		return FMT_CREATE.formatted(expected, grammar, name);
	}
}
