package bakturin.lab4.dynamo;

import bakturin.lab4.antlr.GrammarLexer;
import bakturin.lab4.antlr.GrammarParser;
import bakturin.lab4.dynamo.assets.rule.Rule;
import bakturin.lab4.dynamo.assets.rule.Statement;
import bakturin.lab4.dynamo.assets.term.EpsNode;
import bakturin.lab4.dynamo.assets.term.StartNode;
import bakturin.lab4.dynamo.assets.term.TerminalNode;
import bakturin.lab4.dynamo.creator.ContextCreator;
import bakturin.lab4.dynamo.creator.Creator;
import bakturin.lab4.dynamo.creator.LexerCreator;
import bakturin.lab4.dynamo.creator.ParserCreator;
import bakturin.lab4.dynamo.creator.TokenCreator;
import bakturin.lab4.dynamo.creator.TreeCreator;
import bakturin.lab4.dynamo.grammar.Result;
import bakturin.lab4.dynamo.grammar.Translator;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class Generator {
	public final static String NAME_NODE_EPSILON = "EPS";
	public final static String NAME_NODE_START = "$";
	public final static TerminalNode NODE_EPSILON = new EpsNode();
	public final static TerminalNode NODE_START = new StartNode();

	private Generator() {
		throw new AssertionError("No bakturin.lab4.dynamo.Dynamo instance for you!");
	}

	private static Map<String, Set<TerminalNode>> constructFIRST(final Result result) {
		final Map<String, Set<TerminalNode>> first = new HashMap<>();
		boolean changed = true;

		result.rules().forEach((r, rs) -> first.put(r, new HashSet<>()));
		while (changed) {
			changed = false;
			for (final Map.Entry<String, Rule> rule : result.rules().entrySet()) {
				final String ruleName = rule.getKey();
				final Rule ruleVal = rule.getValue();

				for (final List<Statement> stmt : ruleVal.statements) {
					final Statement firstElement = stmt.get(0);
					final int size = first.get(ruleName).size();

					if (firstElement.isTerminal)
						first.get(ruleName).add(result.terminals().get(firstElement.objectName));
					else first.get(ruleName).addAll(first.get(firstElement.objectName));

					if (size != first.get(ruleName).size()) changed = true;
				}
			}
		}
		return first;
	}

	private static Map<String, Set<TerminalNode>> constructFOLLOW(
			final Result result,
			final Map<String, Set<TerminalNode>> first
	) {
		final Map<String, Set<TerminalNode>> follow = new HashMap<>();
		boolean changed = true;

		result.rules().forEach((r, rs) -> follow.put(r, new HashSet<>()));
		follow.get(result.start()).add(new StartNode());
		while (changed) {
			changed = false;

			for (final Map.Entry<String, Rule> rule : result.rules().entrySet()) {
				final String ruleName = rule.getKey();
				final Rule ruleVal = rule.getValue();

				for (final List<Statement> statements : ruleVal.statements) {
					for (int i = 0; i < statements.size(); i++) {
						final Statement b = statements.get(i);

						if (b.isTerminal) continue;

						final int size = follow.get(b.objectName).size();

						if (i == statements.size() - 1) {
							follow.get(b.objectName).addAll(follow.get(ruleName));
							continue;
						}

						final Statement lambda = statements.get(i + 1);

						if (lambda.isTerminal) {
							follow.get(b.objectName).add(result.terminals().get(lambda.objectName));
						} else {
							final Set<TerminalNode> lambdaFirst = new HashSet<>(first.get(lambda.objectName));

							if (lambdaFirst.remove(NODE_EPSILON))
								follow.get(b.objectName).addAll(follow.get(ruleName));

							follow.get(b.objectName).addAll(lambdaFirst);
						}

						if (size != follow.get(b.objectName).size()) changed = true;
					}
				}
			}
		}
		return follow;
	}

	private static void checkLL1(
			final Result result,
			final Map<String, Set<TerminalNode>> first,
			final Map<String, Set<TerminalNode>> follow
	) {
		for (final Map.Entry<String, Rule> rule : result.rules().entrySet()) {
			final Set<String> used = new HashSet<>();
			final Rule ruleVal = rule.getValue();
			for (final List<Statement> statements : ruleVal.statements) {
				final Statement firstElem = statements.get(0);
				if (firstElem.isTerminal) {
					if (!used.add(firstElem.objectName)) {
						throw new AssertionError("This is not LL(1)!");
					}
				} else {
					for (final String s : statements.stream().map(s -> s.objectName).collect(Collectors.toSet())) {
						if (!used.add(s)) {
							throw new AssertionError("This is not LL(1)!");
						}
					}
				}
			}
		}

		final Set<Rule> epsilonsStart = new HashSet<>();
		for (final Map.Entry<String, Rule> rule : result.rules().entrySet()) {
			final Rule ruleVal = rule.getValue();
			for (final List<Statement> statements : ruleVal.statements) {
				final Statement firstElem = statements.get(0);
				if (firstElem.isTerminal && result.terminals().get(firstElem.objectName).isEpsilon()) {
					epsilonsStart.add(ruleVal);
				}
			}
		}

		for (final Rule ruleVal : epsilonsStart) {
			final Set<Statement> firstElems = new HashSet<>();

			for (final List<Statement> rule : ruleVal.statements) {
				final Statement firstElem = rule.get(0);
				firstElems.add(firstElem);
			}

			for (final Statement f : firstElems) {
				for (final Statement s : firstElems) {
					if (f.equals(s)) continue;
					final Set<TerminalNode> followF = f.isTerminal ? Collections.emptySet() : follow.get(f.objectName);
					final Set<TerminalNode> firstS = s.isTerminal ? Collections.emptySet() : first.get(s.objectName);
					final Set<TerminalNode> cloned = new HashSet<>(followF);
					cloned.retainAll(firstS);
					if (!cloned.isEmpty()) throw new AssertionError("This is not LL(1)!");
				}
			}
		}
	}

	public static Result parseGrammar(final String grammar) {
		final GrammarLexer lex = new GrammarLexer(CharStreams.fromString(grammar));
		final CommonTokenStream tok = new CommonTokenStream(lex);
		final GrammarParser p = new GrammarParser(tok);
		final GrammarParser.StartContext t = p.start();
		final Translator walker = new Translator();
		return (Result) walker.visitStart(t);
	}

	public static Map<String, String> createTokens(final Result result) {
		final Creator tokens = new TokenCreator(result.terminals());
		return tokens.create(result.grammar());
	}

	public static Map<String, String> createLexer(final Result result) {
		final Creator lexer = new LexerCreator(result.terminals());
		return lexer.create(result.grammar());
	}

	public static Map<String, String> createTree(final Result result) {
		final Creator tree = new TreeCreator();
		return tree.create(result.grammar());
	}

	public static Map<String, String> createContext(final Result result) {
		final Creator ctx = new ContextCreator(result.rules());
		return ctx.create(result.grammar());
	}

	public static Map<String, String> createParser(final Result result) {
		final Map<String, Set<TerminalNode>> first = constructFIRST(result);
		final Map<String, Set<TerminalNode>> follow = constructFOLLOW(result, first);
		checkLL1(result, first, follow);
		final Creator parser = new ParserCreator(
				result.start(),
				result.rules(),
				result.terminals(),
				first, follow
		);
		return parser.create(result.grammar());
	}

	public static Map<String, String> generate(final Result result) {
		final Map<String, String> classes = new HashMap<>();
		classes.putAll(Generator.createTokens(result));
		classes.putAll(Generator.createLexer(result));
		classes.putAll(Generator.createTree(result));
		classes.putAll(Generator.createContext(result));
		classes.putAll(Generator.createParser(result));
		return classes;
	}
}
