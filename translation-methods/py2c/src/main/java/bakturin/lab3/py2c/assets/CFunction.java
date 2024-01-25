package bakturin.lab3.py2c.assets;

import bakturin.lab3.py2c.assets.stmt.CallStatement;
import bakturin.lab3.py2c.assets.variable.FloatVariable;
import bakturin.lab3.py2c.assets.variable.IntVariable;
import bakturin.lab3.py2c.assets.variable.StringVariable;
import bakturin.lab3.py2c.exceptions.Py2CMultipleTypeDefinitionException;
import bakturin.lab3.py2c.exceptions.Py2CUnsupportedOperationException;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CFunction implements CStatement {
	private final static String LS = System.lineSeparator();
	private final static String TAB = "\t";
	private final CType returnCType;
	private final String functionName;
	private final List<CStatement> statements;
	private final List<CVariable> argumentsName;

	public CFunction(final CType returnCType, final String functionName, final List<CStatement> statements) {
		this(returnCType, Collections.emptyList(), functionName, statements);
	}

	public CFunction(final CType returnCType, final List<CVariable> argumentsName, final String functionName, final List<CStatement> statements) {
		this.returnCType = returnCType;
		this.functionName = functionName;
		this.statements = new ArrayList<>(statements);
		this.argumentsName = argumentsName;
	}

	public void setTypeOf(final int index, final CType type) {
		final CVariable dummy = this.argumentsName.get(index);
		if (Objects.requireNonNull(dummy.getType()) == CType.VOID) {
			final CVariable newVar;
			switch (type) {
				case INT -> newVar = new IntVariable(dummy.getName());
				case FLOAT -> newVar = new FloatVariable(dummy.getName());
				case STRING -> newVar = new StringVariable(dummy.getName(), "");
				default -> throw new Py2CMultipleTypeDefinitionException(CType.VOID, type);
			}
			this.argumentsName.set(index, newVar);
		} else {
			throw new Py2CMultipleTypeDefinitionException(dummy.getType(), type);
		}
	}

	@Override
	public String asStatement() {
		return returnCType + " " + functionName + "(" + argumentsName.stream().map(s -> s.getType().toString() + " " + s.getName()).collect(Collectors.joining(",")) + ")" + " " + "{" + LS + statements.stream().map(CStatement::asStatement).map(s -> TAB + s).collect(Collectors.joining()) + "}";
	}

	@Override
	public String asStatement(final Map<String, CVariable> args) {
		final StringBuilder sb = new StringBuilder();
		sb.append(returnCType).append(" ").append(functionName).append("(").append(argumentsName.stream().map(s -> s.getType().toString() + " " + s.getName()).collect(Collectors.joining(","))).append(")").append(" ").append("{").append(LS);
		args.forEach((s, v) -> sb.append(System.lineSeparator()).append(v.asDeclaration()).append(System.lineSeparator()));
		sb.append(statements.stream().map(CStatement::asStatement).collect(Collectors.joining())).append("}");
		return sb.toString();
	}

	@Override
	public void fixTypes(final Map<String, CVariable> args) {
		for (final CStatement stmt : this.statements) {
			if (stmt instanceof CallStatement) {
				stmt.fixTypes(args);
			}
		}
	}

	public List<CVariable> getArgumentsName() {
		return this.argumentsName;
	}

	@Override
	public void fixTypes(final Map<String, CVariable> args, final Map<String, CVariable> defArgs) {
		for (final CStatement stmt : this.statements) {
			if (stmt instanceof CallStatement) {
				stmt.fixTypes(args, defArgs);
			}
		}
	}

	@Override
	public CType getType() {
		throw new Py2CUnsupportedOperationException("type of Function");
	}

	@Override
	public String asExpression() {
		throw new Py2CUnsupportedOperationException("expression of Function");
	}
}
