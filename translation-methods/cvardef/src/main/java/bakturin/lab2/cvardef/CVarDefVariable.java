package bakturin.lab2.cvardef;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CVarDefVariable {
	public final String name;
	public final String type;
	public final String defaultValue;
	private final boolean isInitialized;
	private final int pointer;

	private CVarDefVariable(final String name, final String type, final int pointer, final boolean isInitialized,
			final String defaultValue) {
		this.name = name;
		this.type = type;
		this.pointer = pointer;
		this.isInitialized = isInitialized;
		this.defaultValue = defaultValue;
	}

	public CVarDefVariable(final String name, final String type) {
		this(name, type, 0, false, "");
	}

	public CVarDefVariable(final String name, final String type, final String defaultValue) {
		this(name, type, 0, true, defaultValue);
	}

	public CVarDefVariable(final String name, final String type, final int pointer) {
		this(name, type, pointer, false, "");
	}

	public CVarDefVariable(final String name, final String type, final int pointer, final String defaultValue) {
		this(name, type, pointer, true, defaultValue);
	}

	@Override
	public String toString() {
		return type + " " + "*".repeat(pointer) + name + (isInitialized ? " " + "=" + " " + defaultValue : "") + ";";
	}

	@Override
	public boolean equals(final Object other) {
		if (other instanceof CVarDefVariable casted) {
			return name.equals(casted.name) && type.equals(casted.type) && pointer == casted.pointer
					&& (isInitialized == casted.isInitialized
							? (isInitialized ? defaultValue.equals(casted.defaultValue) : true)
							: false);
		} else {
			return false;
		}
	}
}
