package bakturin.lab2.cvardef.assets;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Objects;
import java.util.Set;

import bakturin.lab2.cvardef.exceptions.CVarDefException;
import bakturin.lab2.cvardef.exceptions.CVarDefParserException;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CVarDefParser {
	private final static Set<String> KEYWORDS = Set.of("const", "double", "float", "int", "short", "unsigned",
			"volatile", "char", "static", "long", "signed");
	private final static String W_STRUCT = "struct";
	private final boolean exceptions;
	private CVarDefLexer lex;

	public CVarDefParser(final boolean exceptions) {
		this.exceptions = exceptions;
	}

	private CVarDefTree S() {
		// TYPE
		final CVarDefTree sub = TYPE();
		// VARARG
		final CVarDefTree cont = VARARG();
		// ';'
		if (lex.getToken() != CVarDefToken.SEMICOLON) {
			throw new CVarDefParserException(lex.getPos(), "\";\"");
		}
		return new CVarDefTree(exceptions, CVarDefTree.Node.START, sub, cont);
	}

	private CVarDefTree TYPE() {
		// ASCII
		lex.nextToken();
		if (lex.getToken() != CVarDefToken.ASCII) {
			throw new CVarDefParserException(lex.getPos(), "ascii word");
		}
		final String ascii = lex.getAscii();
		if (ascii.equals(W_STRUCT)) {
			lex.nextToken();
			final String structType = lex.getAscii();
			lex.nextToken();
			return new CVarDefTree(exceptions, CVarDefTree.Node.TYPE, "struct" + " " + structType);
		} else if (KEYWORDS.contains(ascii)) {
			final CVarDefTree keys = TYPE_KEYWORDS();
			if (Objects.isNull(keys)) {
				return new CVarDefTree(exceptions, CVarDefTree.Node.TYPE, ascii);
			} else {
				return new CVarDefTree(exceptions, CVarDefTree.Node.TYPE, ascii, keys);
			}
		} else {
			lex.nextToken();
			return new CVarDefTree(exceptions, CVarDefTree.Node.TYPE, ascii);
		}
	}

	private CVarDefTree TYPE_KEYWORDS() {
		// ASCII
		lex.nextToken();
		final String ascii = lex.getAscii();
		if (lex.getToken() == CVarDefToken.ASCII && KEYWORDS.contains(ascii)) {
			final CVarDefTree keys = TYPE_KEYWORDS();
			if (Objects.isNull(keys)) {
				return new CVarDefTree(exceptions, CVarDefTree.Node.TYPE, ascii);
			} else {
				return new CVarDefTree(exceptions, CVarDefTree.Node.TYPE, ascii, keys);
			}
		} else {
			return null;
		}
	}

	private CVarDefTree VARARG() {
		switch (lex.getToken()) {
			case ASTERISK: {
				// '*'
				lex.nextToken();
				final CVarDefTree sub = VARARG();
				return new CVarDefTree(exceptions, CVarDefTree.Node.POINTER, sub);
			}
			case ASCII: {
				// ASCII
				final String name = lex.getAscii();
				lex.nextToken();
				// ',' or end of VARARG
				if (lex.getToken() == CVarDefToken.COMMA) {
					lex.nextToken();
					final CVarDefTree sub = VARARG();
					return new CVarDefTree(exceptions, CVarDefTree.Node.NAME, name, sub);
				} else if (lex.getToken() == CVarDefToken.SEMICOLON) {
					return new CVarDefTree(exceptions, CVarDefTree.Node.NAME, name);
				} else if (lex.getToken() == CVarDefToken.EQUALS) {
					lex.tryValue();
					final String val = lex.getAscii();
					lex.nextToken();
					if (lex.getToken() == CVarDefToken.COMMA) {
						lex.nextToken();
						final CVarDefTree sub = VARARG();
						return new CVarDefTree(exceptions, CVarDefTree.Node.NAME, name, val, sub);
					} else if (lex.getToken() == CVarDefToken.SEMICOLON) {
						return new CVarDefTree(exceptions, CVarDefTree.Node.NAME, name, val);
					} else {
						throw new CVarDefParserException(lex.getPos(), "',' or ';'");
					}
				}
			}
			default:
				throw new CVarDefParserException(lex.getPos(), "word or '*'");
		}
	}

	public CVarDefTree parse(final String input) {
		return parse(new ByteArrayInputStream(input.getBytes()));
	}

	public CVarDefTree parse(final InputStream input) {
		try {
			lex = new CVarDefLexer(input);
			return S();
		} catch (final CVarDefException e) {
			if (exceptions) {
				throw e;
			} else {
				return null;
			}
		}
	}
}
