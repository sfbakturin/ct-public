package bakturin.lab4.dynamo.assets.rule;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class Statement {
	public final boolean isTerminal;
	public final String objectName;
	public String args;
	public String code;

	public Statement(final boolean isTerminal, final String objectName) {
		this.isTerminal = isTerminal;
		this.objectName = objectName;
		this.args = null;
		this.code = null;
	}
}
