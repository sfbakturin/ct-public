import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.Rule;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import bakturin.lab2.cvardef.CVarDefVariable;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CVarDefTester {
	private final static String LS = System.lineSeparator();
	private final static String TAB = "    ";
	private final static String FORMAT_START = "===> Start of" + " " + "\"" + "%s" + "\"." + LS;
	private final static String FORMAT_SUCCESS = TAB + " " + "%s" + " " + "successfully finished." + LS;
	private final static String FORMAT_FAIL = TAB + " " + "%s" + " " + "failed, because of" + " " + "%s." + LS;

	private String testMethodName;

	public CVarDefTester() {
	}

	@Rule
	public final TestRule watcher = new TestWatcher() {
		@Override
		protected void starting(final Description description) {
			testMethodName = description.getMethodName();
			System.out.printf(FORMAT_START, testMethodName);
		}

		@Override
		protected void succeeded(final Description description) {
			System.out.printf(FORMAT_SUCCESS, testMethodName);
		}

		@Override
		protected void failed(final Throwable e, final Description description) {
			System.err.printf(FORMAT_FAIL, testMethodName, e.getMessage());
		}
	};

	protected CVarDefVariable makeVariable(final String type, final String name, final int pointer) {
		return new CVarDefVariable(name, type, pointer);
	}

	protected CVarDefVariable makeVariable(final String type, final String name) {
		return makeVariable(type, name, 0);
	}

	protected List<CVarDefVariable> makeVariables(final String type, final Map<String, Integer> namesPointers) {
		return namesPointers.entrySet().stream().map(s -> new CVarDefVariable(s.getKey(), type, s.getValue())).toList();
	}

	protected String makeString(final String type, final Map<String, Integer> options) {
		return type + " " + options.entrySet().stream().map(s -> "*".repeat(s.getValue()) + s.getKey())
				.collect(Collectors.joining(",")) + ";";
	}

	protected String makeSortedString(final List<String> mods) {
		return mods.stream().sorted().collect(Collectors.joining(" "));
	}
}
