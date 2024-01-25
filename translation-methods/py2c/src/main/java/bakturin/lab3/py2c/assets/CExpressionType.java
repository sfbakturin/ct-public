package bakturin.lab3.py2c.assets;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public enum CExpressionType {
	ADD("add"), SUBTRACT("subtract"), DIVIDE("divide"), MULTIPLY("multiply"), POWER("power"), BITWISE_AND("bitwise-and"), BITWISE_OR("bitwise-or"), BITWISE_XOR("bitwise-xor"), EQ("equals"), LT("less-than"), GT("greater-than"), NLT("not-less-than"), NGT("not-greater-than"), NE("not-equals");

	private final String expressionName;

	CExpressionType(final String s) {
		this.expressionName = s;
	}

	@Override
	public String toString() {
		return this.expressionName;
	}
}
