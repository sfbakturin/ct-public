package expression;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Const implements CommonExpression {
	private final int constant;

	public Const(final int constant) {
		this.constant = constant;
	}

	@Override
	public String toString() {
		return String.valueOf(this.constant);
	}

	@Override
	public int evaluate(final int x, final int y, final int z) {
		return this.evaluate(0);
	}

	@Override
	public int evaluate(final int x) {
		return this.constant;
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		final Const casted = (Const) o;
		return constant == casted.constant;
	}

	@Override
	public int hashCode() {
		return this.evaluate(0);
	}
}
