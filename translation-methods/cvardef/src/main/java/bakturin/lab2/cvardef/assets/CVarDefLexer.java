package bakturin.lab2.cvardef.assets;

import java.io.IOException;
import java.io.InputStream;
import java.util.Optional;

import bakturin.lab2.cvardef.exceptions.CVarDefLexerException;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CVarDefLexer {
	private final InputStream input;
	private int cur;
	private int pos;
	private CVarDefToken tok;
	private Optional<String> asciiToken;

	public CVarDefLexer(final InputStream input) {
		this.input = input;
		pos = 0;
		nextChar();
	}

	private boolean isBlank(final int c) {
		return Character.isWhitespace(c);
	}

	private void nextChar() {
		pos++;
		try {
			cur = input.read();
		} catch (final IOException e) {
			throw new CVarDefLexerException(cur, e.getMessage());
		}
	}

	private static boolean isValidNameChar(final int ch) {
		return Character.isLetter(ch) || ch == '_';
	}

	private static boolean isValidValueChar(final int ch) {
		return ch == '\"' || ch == '\'' || Character.isDigit(ch) || Character.isLetter(ch);
	}

	public void tryValue() {
		while (isBlank(cur)) {
			nextChar();
		}

		if (isValidValueChar(cur)) {
			final StringBuilder sb = new StringBuilder();
			while (isValidValueChar(cur)) {
				sb.append((char) cur);
				nextChar();
			}
			tok = CVarDefToken.ASCII;
			asciiToken = Optional.of(sb.toString());
		} else {
			throw new CVarDefLexerException(pos, "unknown character for valued" + " " + "\'" + (char) cur + "\'");
		}
	}

	public void nextToken() {
		while (isBlank(cur)) {
			nextChar();
		}

		if (cur == '*') {
			nextChar();
			tok = CVarDefToken.ASTERISK;
		} else if (cur == ',') {
			nextChar();
			tok = CVarDefToken.COMMA;
		} else if (cur == ';') {
			nextChar();
			tok = CVarDefToken.SEMICOLON;
		} else if (cur == '=') {
			nextChar();
			tok = CVarDefToken.EQUALS;
		} else if (isValidNameChar(cur)) {
			final StringBuilder sb = new StringBuilder();
			while (isValidNameChar(cur) || Character.isDigit(cur)) {
				sb.append((char) cur);
				nextChar();
			}
			tok = CVarDefToken.ASCII;
			asciiToken = Optional.of(sb.toString());
		} else {
			throw new CVarDefLexerException(pos, "unknown character" + " " + "\'" + (char) cur + "\'");
		}
	}

	public CVarDefToken getToken() {
		return tok;
	}

	public String getAscii() {
		return asciiToken.get();
	}

	public int getPos() {
		return pos;
	}
}
