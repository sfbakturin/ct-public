package bakturin.lab4.dynamo.assets.term;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class StartNode implements TerminalNode {
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
		return false;
	}

	@Override
	public boolean isStart() {
		return true;
	}

	@Override
	public String asString() {
		return "<start>";
	}

	@Override
	public String name() {
		return "START";
	}

	@Override
	public String create(final String grammar, final String name) {
		return null;
	}

	@Override
	public boolean equals(final Object other) {
		return other instanceof StartNode;
	}

	@Override
	public int hashCode() {
		return asString().hashCode();
	}
}
