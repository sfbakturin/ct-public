package bakturin.lab3.py2c.assets.variable;

import bakturin.lab3.py2c.assets.CType;
import bakturin.lab3.py2c.exceptions.Py2CDefineNotVariableException;

import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class StringVariable extends AbstractVariable<String> {

	public StringVariable(final String value) {
		super(value, CType.STRING);
	}

	public StringVariable(final String variableName, final String value) {
		super(variableName, value, CType.STRING);
	}

	@Override
	public String asExpression() {
		final String variableName = getVariableName();
		if (Objects.isNull(variableName)) {
			return "\"" + getValue() + "\"";
		} else {
			return variableName;
		}
	}

	@Override
	public String asDeclaration() {
		final String variableName = getVariableName();
		if (Objects.nonNull(variableName)) {
			return "char*" + " " + variableName + " " + "=" + " " + "NULL" + ";";
		} else {
			throw new Py2CDefineNotVariableException();
		}
	}
}
