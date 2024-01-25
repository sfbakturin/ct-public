package bakturin.lab3.py2c.assets.variable;

import bakturin.lab3.py2c.assets.CType;
import bakturin.lab3.py2c.exceptions.Py2CDefineNotVariableException;

import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class FloatVariable extends AbstractVariable<Float> {
	public FloatVariable(final float value) {
		super(value, CType.FLOAT);
	}

	public FloatVariable(final String variableName) {
		this(variableName, 0.0F);
	}

	public FloatVariable(final String variableName, final float value) {
		super(variableName, value, CType.FLOAT);
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
			return "float" + " " + variableName + ";";
		} else {
			throw new Py2CDefineNotVariableException();
		}
	}
}
