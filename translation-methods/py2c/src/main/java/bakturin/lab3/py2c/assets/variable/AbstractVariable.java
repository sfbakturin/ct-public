package bakturin.lab3.py2c.assets.variable;

import bakturin.lab3.py2c.assets.CType;
import bakturin.lab3.py2c.assets.CVariable;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public abstract class AbstractVariable<T> implements CVariable {
	private final String variableName;
	private final T value;
	private final CType type;

	public AbstractVariable(final String variableName, final T value, final CType type) {
		this.variableName = variableName;
		this.value = value;
		this.type = type;
	}

	public AbstractVariable(final T value, final CType type) {
		this(null, value, type);
	}

	@Override
	public CType getType() {
		return this.type;
	}

	@Override
	public String getName() {
		return this.variableName;
	}

	protected String getVariableName() {
		return this.variableName;
	}

	protected T getValue() {
		return this.value;
	}
}
