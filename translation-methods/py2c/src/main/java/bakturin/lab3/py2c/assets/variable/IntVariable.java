package bakturin.lab3.py2c.assets.variable;

import bakturin.lab3.py2c.assets.CType;
import bakturin.lab3.py2c.exceptions.Py2CDefineNotVariableException;

import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class IntVariable extends AbstractVariable<Integer> {
	public IntVariable(final int value) {
		super(value, CType.INT);
	}

	public IntVariable(final String variableName) {
		this(variableName, 0);
	}

	public IntVariable(final String variableName, final int value) {
		super(variableName, value, CType.INT);
	}

	@Override
	public String asExpression() {
		final String variableName = getVariableName();
		if (Objects.isNull(variableName)) {
			return String.valueOf(getValue());
		} else {
			return variableName;
		}
	}

	@Override
	public String asDeclaration() {
		final String variableName = getVariableName();
		if (Objects.nonNull(variableName)) {
			return "int" + " " + variableName + ";";
		} else {
			throw new Py2CDefineNotVariableException();
		}
	}
}
