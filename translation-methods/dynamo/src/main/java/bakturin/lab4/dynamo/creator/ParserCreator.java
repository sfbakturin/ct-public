package bakturin.lab4.dynamo.creator;

import bakturin.lab4.dynamo.assets.rule.Rule;
import bakturin.lab4.dynamo.assets.rule.Statement;
import bakturin.lab4.dynamo.assets.term.TerminalNode;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class ParserCreator implements Creator {
	private final String start;
	private final Map<String, Rule> rules;
	private final Map<String, TerminalNode> terminals;
	private final Map<String, Set<TerminalNode>> first;
	private final Map<String, Set<TerminalNode>> follow;

	private final static String PARSER_IMPORTS = "import java.io.InputStream;\n" +
			"import java.text.ParseException;";
	private final static String PARSER_FIELDS = "private %sLexer lex;";
	private final static String PARSER_PARSE = """
			public %sContext parse(final InputStream is) throws ParseException {
			lex = new %sLexer(is);
			lex.nextToken();
			return %s();
			}""";
	/*private final static String PARSER_FIB = """
			private static long fib_(final int n, final int prev, final int acc) {
			if (n <= 0) return acc;
			else return fib_(n - 1, acc, prev + acc);
			}

			private static long fib(final int n) {
			return fib_(n, 1, 0);
			}""";*/

	private final static String FMT_CLASS = "public" + " " + "final" + " " + "class" + " " + "%s" + "Parser";

	private final static String CODE_RULE = "%([0-9]+)";
	private final static String CODE_TERM = "\\$([0-9]+)";

	public ParserCreator(
			final String start,
			final Map<String, Rule> rules,
			final Map<String, TerminalNode> terminals,
			final Map<String, Set<TerminalNode>> first,
			final Map<String, Set<TerminalNode>> follow
	) {
		this.start = start;
		this.rules = rules;
		this.terminals = terminals;
		this.first = first;
		this.follow = follow;
	}

	private static String capitalize(final String s) {
		return s.substring(0, 1).toUpperCase() + s.substring(1);
	}

	private static String processCode(final String code) {
		return code.replaceAll(CODE_RULE, "ctx$1").replaceAll(CODE_TERM, "term$1");
	}

	@Override
	public Map<String, String> create(final String grammar) {
		final StringBuilder clazz = new StringBuilder();
		clazz.append(PARSER_IMPORTS);
		clazz.append(FMT_CLASS.formatted(grammar));
		clazz.append("{");
		clazz.append(PARSER_FIELDS.formatted(grammar));
//		clazz.append(PARSER_FIB);
		clazz.append(PARSER_PARSE.formatted(grammar, grammar, start));
		for (final Map.Entry<String, Rule> rule : this.rules.entrySet()) {
			final String ruleName = rule.getKey();
			final Rule ruleVal = rule.getValue();
			final StringBuilder ruler = new StringBuilder();
			clazz.append("public %sContext %s(".formatted(grammar, ruleName));
			if (!ruleVal.arguments.isEmpty()) {
				clazz.append(ruleVal.arguments.stream().map(f -> f.type() + " " + f.name())
						.collect(Collectors.joining(",")));
			}
			clazz.append(") throws ParseException {");
			clazz.append("final %sContext ctx0 = new %s%sContext();".formatted(grammar, grammar, capitalize(ruleName)));
			ruler.append("switch").append("(").append("lex.getToken()").append(")").append("{");
			List<Statement> epsilon = null;
			for (final List<Statement> statements : ruleVal.statements) {
				final Statement firstElement = statements.get(0);
				ruler.append("case").append(" ");
				if (firstElement.isTerminal) {
					final TerminalNode node = this.terminals.get(firstElement.objectName);
					if (node.isStart()) {
						continue;
					}
					if (node.isRegular()) {
						ruler.append("REGEX");
						ruler.append("_").append(firstElement.objectName).append(":").append("{");
					} else if (node.isString()) {
						ruler.append("STRING");
						ruler.append("_").append(firstElement.objectName).append(":").append("{");
					} else if (node.isEpsilon()) {
						ruler.append("EPSILON");
						epsilon = statements;
						ruler.append(":").append("{");
					} else {
						throw new AssertionError();
					}
				} else {
					final Set<TerminalNode> firsts = this.first.get(firstElement.objectName);
					ruler.append(firsts.stream().map(t -> {
						if (t.isRegular()) {
							return "REGEX" + "_" + t.name();
						} else if (t.isString()) {
							return "STRING" + "_" + t.name();
						} else {
							throw new AssertionError();
						}
					}).collect(Collectors.joining(","))).append(":").append("{");
				}
				fillCase(grammar, statements, ruler, true);
				ruler.append("break;").append("}");
			}
			final Set<TerminalNode> filtered = this.follow.get(ruleName)
					.stream().filter(t -> !t.isEpsilon() && !t.isStart()).collect(Collectors.toSet());
			if (!filtered.isEmpty()) {
				ruler.append("case").append(" ");
				ruler.append(filtered.stream().map(t -> (t.isRegular() ? "REGEX" : "STRING") + "_" + t.name())
						.collect(Collectors.joining(","))).append(":").append("{");
				if (Objects.nonNull(epsilon)) {
					fillCase(grammar, epsilon, ruler, false);
				}
				ruler.append("break;").append("}");
			}
			ruler.append("default: throw new AssertionError();").append("}");
			clazz.append(ruler).append("return ctx0;").append("}");
		}
		clazz.append("}");
		return Map.of("%sParser".formatted(grammar), clazz.toString());
	}

	private void fillCase(
			final String grammar,
			final List<Statement> statements,
			final StringBuilder ruler,
			final boolean nextToken
	) {
		int ruleId = 1, termId = 0;
		for (final Statement part : statements) {
			if (part.isTerminal) {
				final TerminalNode term = this.terminals.get(part.objectName);
				final String prefix;
				if (term.isRegular()) {
					prefix = "REGEX" + "_" + part.objectName;
				} else if (term.isString()) {
					prefix = "STRING" + "_" + part.objectName;
				} else if (term.isEpsilon()) {
					prefix = "EPSILON";
				} else if (term.isStart()) {
					prefix = "START";
				} else {
					throw new AssertionError();
				}
				if (nextToken) {
					ruler.append("if")
							.append("(").append("lex.getToken()")
							.append("!=").append("%sToken.%s".formatted(grammar, prefix))
							.append(")").append("{");
					ruler.append("throw new ParseException(\"Not expected string: \" + \"%s\", lex.getPos());"
							.formatted(term.asString())).append("}");
				}
				ruler.append("final String term%s=lex.getToken().getParsed();".formatted(termId));
				if (Objects.nonNull(part.code)) {
					ruler.append(processCode(part.code));
				}
				termId++;
				if (nextToken) ruler.append("lex.nextToken();");
			} else {
				ruler.append("final %sContext ctx%s=".formatted(grammar, ruleId)).append(part.objectName).append("(");
				if (Objects.nonNull(part.args)) {
					ruler.append(processCode(part.args));
				}
				ruler.append(");");
				ruler.append("ctx0.add(ctx%s);".formatted(ruleId));
				if (Objects.nonNull(part.code)) {
					ruler.append(processCode(part.code));
				}
				ruleId++;
			}
		}
	}
}
