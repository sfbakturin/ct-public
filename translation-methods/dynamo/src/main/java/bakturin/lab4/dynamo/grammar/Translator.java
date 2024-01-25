package bakturin.lab4.dynamo.grammar;

import bakturin.lab4.antlr.GrammarBaseVisitor;
import bakturin.lab4.antlr.GrammarParser;
import bakturin.lab4.dynamo.assets.rule.Field;
import bakturin.lab4.dynamo.assets.rule.Rule;
import bakturin.lab4.dynamo.assets.rule.Statement;
import bakturin.lab4.dynamo.assets.term.RegularNode;
import bakturin.lab4.dynamo.assets.term.StringNode;
import bakturin.lab4.dynamo.assets.term.TerminalNode;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class Translator extends GrammarBaseVisitor<Object> {
	@Override
	@SuppressWarnings("unchecked cast")
	public Object visitStart(final GrammarParser.StartContext ctx) {
		final Map<String, TerminalNode> terms = new HashMap<>();
		final Map<String, Rule> rules = new HashMap<>();
		final String grammar = ctx.name().TERMINAL().getText();
		final String start = ctx.define().RULE().getText();

		for (final GrammarParser.TermContext term : ctx.term()) {
			final Map<String, TerminalNode> parsed = (Map<String, TerminalNode>) visitTerm(term);
			terms.putAll(parsed);
		}

		for (final GrammarParser.RuleContext rule : ctx.rule_()) {
			final Map<String, Rule> parsed = (Map<String, Rule>) visitRule(rule);
			rules.putAll(parsed);
		}

		return new Result(grammar, start, terms, rules);
	}

	@Override
	public Object visitArg(final GrammarParser.ArgContext ctx) {
		return new Field(ctx.argName().getText(), ctx.argType().getText());
	}

	@Override
	public Object visitRule(final GrammarParser.RuleContext ctx) {
		final GrammarParser.RuleRContext ruleR = ctx.ruleR();
		final GrammarParser.RuleLContext ruleL = ctx.ruleL();
		final Rule rule;

		if (Objects.nonNull(ruleL.ret())) {
			final Field returnArg = (Field) visitRet(ruleL.ret());
			rule = new Rule(returnArg);
		} else {
			rule = new Rule(null);
		}

		if (Objects.nonNull(ruleL.args())) {
			for (final GrammarParser.ArgContext argCtx : ruleL.args().arg()) {
				final Field arg = (Field) visitArg(argCtx);
				rule.addArg(arg);
			}
		}

		final String ruleName = ctx.RULE().getText();

		for (final GrammarParser.StmtsContext stmts : ruleR.stmts()) {
			final List<Statement> stmtObjects = new ArrayList<>();
			for (final GrammarParser.StmtContext stmt : stmts.stmt()) {
				final Statement parsed = (Statement) visitStmt(stmt);
				stmtObjects.add(parsed);
			}
			rule.addStmt(stmtObjects);
		}

		return Map.of(ruleName, rule);
	}

	@Override
	public Object visitStmt(final GrammarParser.StmtContext ctx) {
		final Function<String, String> getSubstring = s -> s.substring(1, s.length() - 1);
		final Statement stmt;

		if (Objects.isNull(ctx.RULE())) {
			stmt = new Statement(true, ctx.TERMINAL().getText());
		} else {
			stmt = new Statement(false, ctx.RULE().getText());
		}

		if (Objects.nonNull(ctx.ARGS())) {
			stmt.args = getSubstring.apply(ctx.ARGS().getText());
		}
		if (Objects.nonNull(ctx.CODE())) {
			stmt.code = "{" + getSubstring.apply(ctx.CODE().getText()) + "}";
		}

		return stmt;
	}

	@Override
	public Object visitTerm(final GrammarParser.TermContext ctx) {
		final Function<String, String> getSubstring = s -> s.substring(1, s.length() - 1);
		final String termName = ctx.TERMINAL().getText();

		if (Objects.isNull(ctx.REGEX())) {
			final String s = ctx.STRING().getText();
			return Map.of(termName, new StringNode(getSubstring.apply(s), termName));
		} else {
			final String r = ctx.REGEX().getText();
			return Map.of(termName, new RegularNode(getSubstring.apply(r), termName));
		}
	}
}
