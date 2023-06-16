/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class Variable implements Expression {
	private final String name;

	public Variable(final String name) {
		this.name = name;
	}

	@Override
	public String disassembly() {
		return name;
	}
}
