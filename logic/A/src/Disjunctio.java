/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class Disjunctio implements Expression {
	private final Expression left;
	private final Expression right;

	public Disjunctio(final Expression left, final Expression right) {
		this.left = left;
		this.right = right;
	}

	@Override
	public String disassembly() {
		final String leftDis = left.disassembly();
		final String rightDis = right.disassembly();
		return "(" + "|" + "," + leftDis + "," + rightDis + ")";
	}
}
