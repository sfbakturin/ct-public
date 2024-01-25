package bakturin.lab3.py2c.assets.stmt;

import bakturin.lab3.py2c.assets.CExpression;
import bakturin.lab3.py2c.assets.CStatement;
import bakturin.lab3.py2c.assets.CType;
import bakturin.lab3.py2c.assets.CVariable;
import bakturin.lab3.py2c.exceptions.Py2CUnsupportedOperationException;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class ForRangeStatement implements CStatement {
	private final static String TAB = "\t";
	private final static String LS = System.lineSeparator();
	private final String variableName;
	private final CExpression from;
	private final CExpression to;
	private final List<CStatement> statements;

	public ForRangeStatement(final String variableName, final CExpression from, final CExpression to, final List<CStatement> statements) {
		this.variableName = variableName;
		this.from = from;
		this.to = to;
		this.statements = statements;
	}

	@Override
	public String asStatement() {
		return "for" + " " + "(" + variableName + " " + "=" + " " + from.asExpression() + ";" + " " + variableName + " " + "<" + " " + to.asExpression() + ";" + " " + variableName + " " + "=" + " " + variableName + " " + "+" + " " + "1" + ")" + " " + "{" + LS + statements.stream().map(CStatement::asStatement).map(s -> TAB + TAB + s).collect(Collectors.joining()) + TAB + "}" + LS;
	}

	@Override
	public String asStatement(final Map<String, CVariable> args) {
		throw new Py2CUnsupportedOperationException("compiling with arguments in ForRangeStatement");
	}

	@Override
	public CType getType() {
		throw new Py2CUnsupportedOperationException("type of ForRangeStatement");
	}

	@Override
	public String asExpression() {
		throw new Py2CUnsupportedOperationException("expression of ForRangeStatement");
	}

	@Override
	public void fixTypes(final Map<String, CVariable> args) {
		throw new Py2CUnsupportedOperationException("fixing types in ForRangeStatement");
	}

	@Override
	public void fixTypes(final Map<String, CVariable> args, final Map<String, CVariable> defArgs) {
		throw new Py2CUnsupportedOperationException("fix types in ElifStatement");
	}
}
