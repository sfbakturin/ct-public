package bakturin.lab4.dynamo.creator;

import bakturin.lab4.dynamo.Generator;
import bakturin.lab4.dynamo.assets.term.TerminalNode;

import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class TokenCreator implements Creator {
	private final Map<String, TerminalNode> terms;

	private final static String TOKEN_FIELDS = """
			private final String expected;
			private final boolean isRegex;
			public String parsed;""";
	private final static String TOKEN_CTOR = """
			%sToken(final String expected, final boolean isRegex) {
			this.expected = expected;
			this.isRegex = isRegex;
			this.parsed = null;
			}""";
	private final static String TOKEN_SET_GET_PARSED = """
			public void setParsed(final String parsed) {
			this.parsed = parsed;
			}

			public String getParsed() {
			return this.parsed;
			}""";

	private final static String FMT_ENUM = "public" + " " + "enum" + " " + "%s" + "Token";
	private final static String FMT_STRING = "STRING" + "_" + "%s" + "(" + "\"" + "%s" + "\"" + "," + "false" + ")";
	private final static String FMT_REGEX = "REGEX" + "_" + "%s" + "(" + "\"" + "%s" + "\"" + "," + "true" + ")";
	private final static String FMT_EPSILON = "EPSILON" + "(" + "null" + "," + "false" + ")";
	private final static String FMT_START = "START" + "(" + "null" + "," + "false" + ")";

	public TokenCreator(final Map<String, TerminalNode> terms) {
		this.terms = terms;
	}

	@Override
	public Map<String, String> create(final String grammar) {
		final StringBuilder clazz = new StringBuilder();
		clazz.append(FMT_ENUM.formatted(grammar));
		clazz.append("{");
		this.terms.put(Generator.NAME_NODE_EPSILON, Generator.NODE_EPSILON);
		this.terms.put(Generator.NAME_NODE_START, Generator.NODE_START);
		clazz.append(this.terms.entrySet().stream().map(term -> {
			final String name = term.getKey();
			final TerminalNode node = term.getValue();
			final String value = node.asString();
			if (node.isRegular()) {
				return FMT_REGEX.formatted(name, value);
			} else if (node.isString()) {
				return FMT_STRING.formatted(name, value);
			} else if (node.isEpsilon()) {
				return FMT_EPSILON;
			} else if (node.isStart()) {
				return FMT_START;
			} else {
				throw new AssertionError();
			}
		}).collect(Collectors.joining(","))).append(";");
		clazz.append(TOKEN_FIELDS);
		clazz.append(TOKEN_CTOR.formatted(grammar));
		clazz.append(TOKEN_SET_GET_PARSED);
		clazz.append("}");
		return Map.of("%sToken".formatted(grammar), clazz.toString());
	}
}
