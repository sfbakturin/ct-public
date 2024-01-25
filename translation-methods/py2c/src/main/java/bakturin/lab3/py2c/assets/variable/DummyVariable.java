package bakturin.lab3.py2c.assets.variable;

import bakturin.lab3.py2c.assets.CType;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class DummyVariable extends AbstractVariable<Object> {
	public DummyVariable(final String variableName) {
		super(variableName, null, CType.VOID);
	}

	@Override
	public String asExpression() {
		return getVariableName();
	}

	@Override
	public String asDeclaration() {
		throw new AssertionError("123");
	}
}
