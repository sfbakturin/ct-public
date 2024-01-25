package bakturin.lab3.py2c.assets;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public enum CType {
	INT("int", "%i"), FLOAT("float", "%f"), STRING("char*", "%s"), VOID("void", null);

	private final String display;
	private final String format;

	CType(final String display, final String format) {
		this.display = display;
		this.format = format;
	}

	@Override
	public String toString() {
		return this.display;
	}

	public String getFormat() {
		return this.format;
	}
}
