package bakturin.lab3.py2c.assets.stmt;

import bakturin.lab3.py2c.assets.*;
import bakturin.lab3.py2c.assets.variable.FloatVariable;
import bakturin.lab3.py2c.assets.variable.IntVariable;
import bakturin.lab3.py2c.assets.variable.StringVariable;
import bakturin.lab3.py2c.exceptions.Py2CUnsupportedOperationException;

import java.util.*;
import java.util.function.Function;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CallStatement implements CStatement {
	private final CFunction func;
	private final Function<List<CExpression>, String> function;
	private final List<CExpression> arguments;

	public CallStatement(final Function<List<CExpression>, String> function, final List<CExpression> arguments) {
		this(function, arguments, null);
	}

	public CallStatement(final Function<List<CExpression>, String> function, final List<CExpression> arguments, final CFunction func) {
		this.function = function;
		this.arguments = new ArrayList<>(arguments);
		this.func = func;
	}

	@Override
	public String asStatement() {
		return function.apply(arguments) + ";" + System.lineSeparator();
	}

	@Override
	public String asStatement(final Map<String, CVariable> args) {
		throw new Py2CUnsupportedOperationException("compiling with arguments in CallStatement");
	}

	@Override
	public CType getType() {
		throw new Py2CUnsupportedOperationException("type of CallStatement");
	}

	@Override
	public String asExpression() {
		throw new Py2CUnsupportedOperationException("expression of CallStatement");
	}

	@Override
	public void fixTypes(final Map<String, CVariable> args) {
		if (Objects.nonNull(this.func)) {
			for (int i = 0; i < this.arguments.size(); i++) {
				this.func.setTypeOf(i, this.arguments.get(i).getType());
			}
			final Map<String, CVariable> defArgs = new HashMap<>();
			for (final CVariable vs : this.func.getArgumentsName()) {
				defArgs.put(vs.getName(), vs);
			}
			this.func.fixTypes(args, defArgs);
		}
	}

	@Override
	public void fixTypes(final Map<String, CVariable> args, final Map<String, CVariable> defArgs) {
		for (int i = 0; i < this.arguments.size(); i++) {
			if (this.arguments.get(i) instanceof CVariable dummy && this.arguments.get(i).getType() == CType.VOID) {
				final CVariable newVar;
				switch (defArgs.get(dummy.getName()).getType()) {
					case INT -> newVar = new IntVariable(dummy.getName());
					case FLOAT -> newVar = new FloatVariable(dummy.getName());
					case STRING -> newVar = new StringVariable(dummy.getName(), "");
					default -> throw new AssertionError("123");
				}
				this.arguments.set(i, newVar);
			}
		}
	}
}
