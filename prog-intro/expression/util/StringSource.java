package expression.util;

import expression.exceptions.custom.ParserException;

import java.util.Set;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class StringSource implements CharSource {
	private static final Set<Character> ALPHABET_CHARACTER_SET, VARIABLES_SET;
	private final String data;
	private int pos;

	static {
		ALPHABET_CHARACTER_SET = Set.of(
				'x', 'y', 'z', '+', '-', '*', '/', 't', 'l', '(', ')', 'm', 'i', 'n', 'a',
				'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
		);
		VARIABLES_SET = Set.of(
				'x', 'y', 'z'
		);
	}

	public StringSource(final String source) throws ParserException {
		int balance = 0;
		final StringBuilder sb = new StringBuilder();
		boolean flag = false;
		final char[] arr = source.toCharArray();
		for (final char c : arr) {
			if (ALPHABET_CHARACTER_SET.contains(c)) {
				sb.append(c);
				if (flag) {
					flag = false;
				}
				switch (c) {
					case '(' -> balance++;
					case ')' -> balance--;
				}
				if (balance < 0) {
					throw new ParserException("No balance with ()");
				}
			} else {
				if (!Character.isWhitespace(c)) {
					throw new ParserException("No such symbol was found in my alphabet: " + c);
				} else {
					if (!flag) {
						sb.append(' ');
					}
					flag = !flag;
				}
			}
		}
		if (balance != 0) {
			throw new ParserException("No balance with ()");
		}
		final String s = sb.toString();
		for (int i = 0; i < s.length() - 1; i++) {
			final boolean digit = Character.isDigit(s.charAt(i + 1));
			if ((Character.isDigit(s.charAt(i)) && s.charAt(i + 1) == 'm') || (s.charAt(i) == 'n' && digit) || (s.charAt(i) == 'm' && digit)) {
				throw new ParserException("digit-min-digit or digit-max-digit: " + i + ", " + (i + 1));
			}
		}
		this.pos = 0;
		this.data = s;
	}

	@Override
	public boolean hasNext() {
		return this.pos < this.data.length();
	}

	@Override
	public char next() {
		return this.data.charAt(this.pos++);
	}

	@Override
	public char lookNext() {
		if (this.pos - 1 >= 0) {
			return this.data.charAt(this.pos - 1);
		} else {
			return 0;
		}
	}

	@Override
	public boolean check(final char c) {
		return VARIABLES_SET.contains(c);
	}
}
