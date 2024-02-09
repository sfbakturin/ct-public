import bakturin.lab3.py2c.Py2C;
import bakturin.lab3.py2c.exceptions.Py2CException;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CTester {
	private final static String LS = System.lineSeparator();
	private final static String TAB = "    ";
	private final static String FORMAT_START = "===> Start of" + " " + "\"" + "%s" + "\"." + LS;
	private final static String FORMAT_SUCCESS = TAB + " " + "%s" + " " + "successfully finished." + LS;
	private final static String FORMAT_FAIL = TAB + " " + "%s" + " " + "failed, because of" + " " + "%s." + LS;

	private final static Path DIR_SAMPLES = Paths.get("samples");
	private final static String EXECUTABLE = "__test";
	private final static String FILENAME = EXECUTABLE + "." + "c";
	private final static Path FILE_SOURCE = Paths.get(FILENAME);
	private final static String FORMAT_CMD_COMPILE = "gcc" + " " + "-O3" + " " + "%s" + " " + "-o" + " " + "%s";

	@AfterClass
	public static void afterClass() {
		try {
			Files.delete(Paths.get(EXECUTABLE));
			Files.delete(Paths.get(FILENAME));
		} catch (final IOException ignored) {
		}
	}

	private String testMethodName;

	public Py2CTester() {
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

	private static Py2C.Py2CResult expect(final String code, final boolean isSuccess) {
		final Py2C.Py2CResult result = Py2C.translate(code);
		Assert.assertEquals(isSuccess, result.isSuccess());
		return result;
	}

	protected static String readCode(final String test) {
		final Path src = DIR_SAMPLES.resolve(Paths.get(test));
		try {
			return String.join(System.lineSeparator(), Files.readAllLines(src));
		} catch (final Exception e) {
			throw new AssertionError(e);
		}
	}

	protected static void runCode(final String expected) {
		final ProcessBuilder cmd = new ProcessBuilder("./" + EXECUTABLE);
		final Process p;
		try {
			p = cmd.start();
		} catch (final IOException e) {
			throw new AssertionError(e);
		}
		final BufferedReader r = new BufferedReader(new InputStreamReader(p.getInputStream()));
		final StringBuilder actual = new StringBuilder();
		while (true) {
			final String line;
			try {
				line = r.readLine();
			} catch (final IOException e) {
				throw new AssertionError(e);
			}
			if (line == null) {
				break;
			}
			actual.append(line).append(System.lineSeparator());
		}
		Assert.assertEquals(expected, actual.toString());
	}

	protected static void compileCode(final String src) {
		try {
			Files.writeString(FILE_SOURCE, src);
		} catch (final Exception e) {
			throw new AssertionError(e);
		}

		final String cmd = FORMAT_CMD_COMPILE.formatted(FILENAME, EXECUTABLE);
		try {
			final Process p = Runtime.getRuntime().exec(cmd);
			p.waitFor();
			Assert.assertEquals(0, p.exitValue());
		} catch (final Exception e) {
			throw new AssertionError(e);
		}
	}

	protected static String expectSuccess(final String code) {
		return expect(code, true).code;
	}

	protected static void expectFailed(final String code, final Class<? extends Py2CException> expected) {
		final Py2C.Py2CResult result = expect(code, false);
		Assert.assertEquals(expected, result.errorClass());
	}
}
