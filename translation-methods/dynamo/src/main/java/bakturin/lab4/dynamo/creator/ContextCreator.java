package bakturin.lab4.dynamo.creator;

import bakturin.lab4.dynamo.assets.rule.Field;
import bakturin.lab4.dynamo.assets.rule.Rule;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class ContextCreator implements Creator {
	private final Map<String, Rule> rules;

	private final static String FMT_INTERFACE_NAME = "%sContext";
	private final static String FMT_CONTEXT_NAME = "%s%sContext";
	private final static String FMT_INTERFACE = "public" + " " + "interface" + " " + FMT_INTERFACE_NAME;
	private final static String FMT_CONTEXT =
			"public" + " " + "final" + " " + "class" + " " + FMT_CONTEXT_NAME + " " + "extends" + " " + "%sTree";
	private final static String FMT_INTERFACE_GENERAL = "%sContext child(int i);" + "void add(%sContext child);";
	private final static String FMT_INTERFACE_METHOD = "%s%sContext %s();";
	private final static String FMT_CONTEXT_METHOD_IMPL = """
			@Override
			public %s%sContext %s() {
			return this;
			}""";
	private final static String FMT_CONTEXT_METHOD_NULL = """
			@Override
			public %s%sContext %s() {
			throw new UnsupportedOperationException();
			}""";
	private final static String FMT_CONTEXT_FIELD = "public" + " " + "%s" + " " + "%s" + ";";

	private static String capitalize(final String s) {
		return s.substring(0, 1).toUpperCase() + s.substring(1);
	}

	private static String mkInterfaceMethod(final String grammar, final String rule) {
		return FMT_INTERFACE_METHOD.formatted(grammar, capitalize(rule), rule);
	}

	private static String mkContextMethodImpl(final String grammar, final String rule) {
		return FMT_CONTEXT_METHOD_IMPL.formatted(grammar, capitalize(rule), rule);
	}

	private static String mkContextMethodNull(final String grammar, final String rule) {
		return FMT_CONTEXT_METHOD_NULL.formatted(grammar, capitalize(rule), rule);
	}

	public ContextCreator(final Map<String, Rule> rules) {
		this.rules = rules;
	}

	@Override
	public Map<String, String> create(final String grammar) {
		final Map<String, String> classes = new HashMap<>();
		final StringBuilder classCtx = new StringBuilder();
		classCtx.append(FMT_INTERFACE.formatted(grammar)).append("{");
		classCtx.append(FMT_INTERFACE_GENERAL.formatted(grammar, grammar));
		for (final Map.Entry<String, Rule> rule : this.rules.entrySet()) {
			final String ruleName = rule.getKey();
			classCtx.append(mkInterfaceMethod(grammar, ruleName));
		}
		classCtx.append("}");
		classes.put(FMT_INTERFACE_NAME.formatted(grammar), classCtx.toString());
		for (final Map.Entry<String, Rule> rule : this.rules.entrySet()) {
			final String ruleName = rule.getKey();
			final Rule ruleVal = rule.getValue();
			final StringBuilder ctx = new StringBuilder();
			ctx.append(FMT_CONTEXT.formatted(grammar, capitalize(ruleName), grammar)).append("{");
			for (final Map.Entry<String, Rule> method : this.rules.entrySet()) {
				final String methodName = method.getKey();
				if (methodName.equals(ruleName)) {
					ctx.append(mkContextMethodImpl(grammar, methodName));
				} else {
					ctx.append(mkContextMethodNull(grammar, methodName));
				}
			}
			if (Objects.nonNull(ruleVal.returnArg)) {
				final Field retArg = ruleVal.returnArg;
				ctx.append(FMT_CONTEXT_FIELD.formatted(retArg.type(), retArg.name()));
			}
			ctx.append("}");
			classes.put(FMT_CONTEXT_NAME.formatted(grammar, capitalize(ruleName)), ctx.toString());
		}
		return classes;
	}
}
