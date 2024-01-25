package bakturin.lab4.dynamo.creator;

import java.util.Map;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class TreeCreator implements Creator {
	private final static String FMT_TREE = """
			import java.util.ArrayList;
			import java.util.List;

			public abstract class %sTree implements %sContext {
			private final List<%sContext> children;

			public %sTree() {
			children = new ArrayList<>();
			}

			@Override
			public %sContext child(final int i) {
			return children.get(i);
			}

			@Override
			public void add(final %sContext child) {
			children.add(child);
			}
			}
			""";
	private final static String FMT_NAME = "%sTree";

	private static String mkTreeFormatted(final String grammar) {
		return FMT_TREE.formatted(grammar, grammar, grammar, grammar, grammar, grammar);
	}

	@Override
	public Map<String, String> create(final String grammar) {
		return Map.of(FMT_NAME.formatted(grammar), mkTreeFormatted(grammar));
	}
}
