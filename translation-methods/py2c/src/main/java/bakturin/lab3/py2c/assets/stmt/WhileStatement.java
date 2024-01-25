package bakturin.lab3.py2c.assets.stmt;

import bakturin.lab3.py2c.assets.CExpression;
import bakturin.lab3.py2c.assets.CStatement;
import bakturin.lab3.py2c.assets.CType;
import bakturin.lab3.py2c.assets.CVariable;
import bakturin.lab3.py2c.exceptions.Py2CUnsupportedOperationException;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class WhileStatement implements CStatement {
	private final CExpression expression;
	private final List<CStatement> statements;

	public WhileStatement(final CExpression expression, final List<CStatement> statements) {
		this.expression = expression;
		this.statements = new ArrayList<>(statements);
	}

	@Override
	public String asStatement() {
		return "while" + expression.asExpression() + "{" + System.lineSeparator() + statements.stream().map(CStatement::asStatement).collect(Collectors.joining()) + "}" + System.lineSeparator();
	}

	@Override
	public String asStatement(final Map<String, CVariable> args) {
		throw new Py2CUnsupportedOperationException("compiling with arguments in WhileStatement");
	}

	@Override
	public CType getType() {
		throw new Py2CUnsupportedOperationException("type of WhileStatement");
	}

	@Override
	public String asExpression() {
		throw new Py2CUnsupportedOperationException("expression of WhileStatement");
	}

	@Override
	public void fixTypes(final Map<String, CVariable> args) {
		throw new Py2CUnsupportedOperationException("fixing types in WhileStatement");
	}

	@Override
	public void fixTypes(final Map<String, CVariable> args, final Map<String, CVariable> defArgs) {
		throw new Py2CUnsupportedOperationException("fix types in ElifStatement");
	}
}
