/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class Inversio implements Expression {
	private final Expression val;

	public Inversio(final Expression val) {
		this.val = val;
	}

	@Override
	public String disassembly() {
		final String dis = val.disassembly();
		return "(" + "!" + dis + ")";
	}
}
