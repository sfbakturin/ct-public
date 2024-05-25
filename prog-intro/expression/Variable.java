package expression;

import java.util.Map;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Variable implements CommonExpression {
	private final String s;
	private final static Map<String, Integer> ALPHABET = Map.of(
			"x", 0,
			"y", 1,
			"z", 2
	);

	public Variable(final String s) {
		this.s = s;
	}

	@Override
	public String toString() {
		return this.s;
	}

	@Override
	public int evaluate(final int x, final int y, final int z) {
		final int[] arr = new int[]{x, y, z};
		if (this.check(this.s) && (ALPHABET.get(s) < arr.length)) {
			return arr[ALPHABET.get(s)];
		}
		throw new AssertionError("No such variable found OR not all variables are init-ed");
	}

	@Override
	public int evaluate(final int x) {
		return this.evaluate(x, 0, 0);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		final Variable casted = (Variable) o;
		return Objects.equals(s, casted.s);
	}

	@Override
	public int hashCode() {
		return this.s.hashCode();
	}

	private boolean check(final String s) {
		return ALPHABET.containsKey(s);
	}
}
