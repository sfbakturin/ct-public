package bakturin.lab3.py2c.assets.stmt;

import bakturin.lab3.py2c.assets.CExpression;
import bakturin.lab3.py2c.assets.CStatement;
import bakturin.lab3.py2c.assets.CType;
import bakturin.lab3.py2c.assets.CVariable;
import bakturin.lab3.py2c.exceptions.Py2CUnsupportedOperationException;

import java.util.Map;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class AssignmentStatement implements CStatement {
	private final String variableName;
	private final CExpression expression;

	public AssignmentStatement(final String variableName, final CExpression expression) {
		this.variableName = variableName;
		this.expression = expression;
	}

	@Override
	public String asStatement() {
		return variableName + " " + "=" + " " + expression.asExpression() + ";" + System.lineSeparator();
	}

	@Override
	public String asStatement(final Map<String, CVariable> args) {
		throw new Py2CUnsupportedOperationException("compiling with arguments in AssignmentStatement");
	}

	@Override
	public CType getType() {
		throw new Py2CUnsupportedOperationException("type of AssignmentStatement");
	}

	@Override
	public String asExpression() {
		throw new Py2CUnsupportedOperationException("expression of AssignmentStatement");
	}

	@Override
	public void fixTypes(final Map<String, CVariable> args) {
		throw new Py2CUnsupportedOperationException("fix types in ElifStatement");
	}

	@Override
	public void fixTypes(final Map<String, CVariable> args, final Map<String, CVariable> defArgs) {
		throw new Py2CUnsupportedOperationException("fix types in ElifStatement");
	}
}
