package expression.util;

import expression.exceptions.custom.ParserException;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class BaseParser {
	private static final char END = '\0';
	private final CharSource source;
	private char ch;
	private final StringBuilder error = new StringBuilder();

	protected BaseParser(final CharSource source) {
		this.source = source;
		this.take();
	}

	protected char take() {
		final char result = ch;
		this.ch = this.source.hasNext() ? this.source.next() : END;
		return result;
	}

	protected boolean test(final char expected) {
		return this.ch == expected;
	}

	protected boolean take(final char expected) {
		if (this.test(expected)) {
			this.take();
			return true;
		}
		return false;
	}

	protected boolean expect(final String expected) throws ParserException {
		boolean flag = true;
		for (final char c : expected.toCharArray()) {
			error.append(c);
			if (!this.take(c)) {
				flag = false;
				break;
			}
		}
		return flag;
	}

	protected String get() {
		return error.toString();
	}

	protected char lookNext() {
		return this.source.lookNext();
	}

	protected boolean eof() {
		return take(END);
	}

	protected char returnChar() {
		return this.ch;
	}

	protected boolean checkChar(final char c) {
		return this.source.check(c);
	}

	protected boolean between(final char from) {
		return from <= this.ch && this.ch <= '9';
	}
}
