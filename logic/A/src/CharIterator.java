/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class CharIterator {
	private static final char EOF = '\0';
	private final String source;
	private int pos;

	public CharIterator(final String source) {
		this.source = clean(source);
		pos = 0;
	}

	private static String clean(final String source) {
		final StringBuilder sb = new StringBuilder();
		for (final char c : source.toCharArray()) {
			if (!Character.isWhitespace(c)) {
				sb.append(c);
			}
		}
		return sb.toString();
	}

	public char next() {
		return hasNext() ? source.charAt(pos++) : EOF;
	}

	public char prev() {
		pos--;
		return source.charAt(pos);
	}

	public boolean hasNext() {
		return pos < source.length();
	}

	public void skip() {
		next();
	}

	public boolean isEOF(final char c) {
		return c == EOF;
	}
}
