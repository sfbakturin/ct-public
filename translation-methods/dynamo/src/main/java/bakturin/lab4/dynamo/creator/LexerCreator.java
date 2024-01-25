package bakturin.lab4.dynamo.creator;

import bakturin.lab4.dynamo.assets.term.TerminalNode;

import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class LexerCreator implements Creator {
	private final Map<String, TerminalNode> terms;

	private final static String LEXER_IMPORTS = """
			import java.io.IOException;
			import java.io.InputStream;
			import java.text.ParseException;
			import java.util.regex.Pattern;""";
	private final static String LEXER_FIELDS = """
			private final InputStream is;
			private int curChar;
			private int curPos;
			private %sToken curToken;""";
	private final static String LEXER_CTOR = """
			public %sLexer(final InputStream is) throws ParseException {
			this.is = is;
			this.curPos = 0;
			this.nextChar();
			}""";
	private final static String LEXER_IS_BLANK = """
			private static boolean isBlank(final int c) {
			return c == ' ' || c == '\\r' || c == '\\n' || c == '\\t';
			}""";
	private final static String LEXER_NEXT_CHAR = """
			private void nextChar() throws ParseException {
			curPos++;
			try {
			curChar = is.read();
			} catch (final IOException e) {
			throw new ParseException(e.getMessage(), curPos);
			}
			}""";
	private final static String LEXER_GET_TOKEN = """
			public %sToken getToken() {
			return this.curToken;
			}""";
	private final static String LEXER_GET_POS = """
			public int getPos() {
			return this.curPos;
			}""";
	private final static String LEXER_NEXT_TOKEN = """
			public void nextToken() throws ParseException {
			while (isBlank(curChar)) {
			nextChar();
			}

			final StringBuilder raw = new StringBuilder();
			boolean found = false, prefix = false;
			while (true) {
			prefix = false;
			raw.append((char) curChar);
			final String parsed = raw.toString();
			%s
			if (!found && !prefix) {
			throw new ParseException("Illegal" + " " + "text" + " " + "\\"" + parsed + "\\"", curPos);
			}
			if (found && !prefix) {
			raw.deleteCharAt(raw.length() - 1);
			break;
			}
			nextChar();
			}
			curToken.setParsed(raw.toString());
			}""";

	private final static String FMT_CLASS = "public" + " " + "final" + " " + "class" + " " + "%s" + "Lexer";

	public LexerCreator(final Map<String, TerminalNode> terms) {
		this.terms = terms;
	}

	@Override
	public Map<String, String> create(final String grammar) {
		final StringBuilder clazz = new StringBuilder();
		clazz.append(LEXER_IMPORTS);
		clazz.append(FMT_CLASS.formatted(grammar));
		clazz.append("{");
		clazz.append(LEXER_FIELDS.formatted(grammar));
		clazz.append(LEXER_CTOR.formatted(grammar));
		clazz.append(LEXER_IS_BLANK);
		clazz.append(LEXER_NEXT_CHAR);
		clazz.append(LEXER_GET_TOKEN.formatted(grammar));
		clazz.append(LEXER_GET_POS);
		clazz.append(LEXER_NEXT_TOKEN.formatted(terms.entrySet()
				.stream().filter(term -> !term.getValue().isStart()).map(term -> {
					final String name = term.getKey();
					final TerminalNode node = term.getValue();
					return node.create(grammar, name);
				}).collect(Collectors.joining())));
		clazz.append("}");
		return Map.of("%sLexer".formatted(grammar), clazz.toString());
	}
}
