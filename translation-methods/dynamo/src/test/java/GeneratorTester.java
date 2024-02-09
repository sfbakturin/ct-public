import bakturin.lab4.dynamo.Generator;
import bakturin.lab4.dynamo.grammar.Result;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Map;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class GeneratorTester {
	private final static String LS = System.lineSeparator();
	private final static String TAB = "    ";
	private final static String FORMAT_START = "===> Start of" + " " + "\"" + "%s" + "\"." + LS;
	private final static String FORMAT_SUCCESS = TAB + " " + "%s" + " " + "successfully finished." + LS;
	private final static String FORMAT_FAIL = TAB + " " + "%s" + " " + "failed, because of" + " " + "%s." + LS;


	private final static Path DIR_SAMPLES = Paths.get("samples");
	private final static Path DIR_OUTPUT = Paths.get("__test");
	private final static Path FILE_OUTPUT = DIR_OUTPUT.resolve("__test.out");

	private final static FileVisitor<Path> DIR_DELETER = new SimpleFileVisitor<>() {

		@Override
		public FileVisitResult visitFile(
				final Path file,
				final BasicFileAttributes attrs
		) throws IOException {
			Files.delete(file);
			return FileVisitResult.CONTINUE;
		}

		@Override
		public FileVisitResult postVisitDirectory(
				final Path dir,
				final IOException exc
		) throws IOException {
			if (Objects.isNull(exc)) {
				Files.delete(dir);
				return FileVisitResult.CONTINUE;
			} else {
				throw exc;
			}
		}
	};

	private final static String MAIN_VOID = """
			import java.io.ByteArrayInputStream;
			import java.text.ParseException;

			public class Main {
			public static void main(String[] args) throws Exception {
			final %sParser p = new %sParser();
			p.parse(new ByteArrayInputStream("%s".getBytes()));
			}
			}
			""";
	private final static String MAIN_OUTPUT = """
			import java.io.ByteArrayInputStream;
			import java.text.ParseException;
			import java.nio.file.Files;
			import java.nio.file.Paths;

			public class Main {
			public static void main(String[] args) throws Exception {
			final %sParser p = new %sParser();
			Files.writeString(Paths.get("%s"), String.valueOf(p.parse(new ByteArrayInputStream("%s".getBytes())).%s.%s));
			}
			}
			""";

	private String testMethodName;

	@BeforeClass
	public static void beforeClass() {
		try {
			Files.createDirectories(DIR_OUTPUT);
		} catch (final FileAlreadyExistsException ignored) {
		} catch (final IOException e) {
			throw new AssertionError(e);
		}
	}

	@AfterClass
	@SuppressWarnings("all")
	public static void afterClass() {
		try {
			Files.walkFileTree(DIR_OUTPUT, DIR_DELETER);
		} catch (final NoSuchFileException ignored) {
		} catch (final IOException e) {
			throw new AssertionError(e);
		}
	}

	public GeneratorTester() {
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

	private static String readGrammar(final String grammar) {
		final Path src = DIR_SAMPLES.resolve(grammar);
		try {
			return String.join(System.lineSeparator(), Files.readAllLines(src));
		} catch (final Exception e) {
			throw new AssertionError(e);
		}
	}

	private static void writeClasses(final Map<String, String> classes) {
		for (final Map.Entry<String, String> clazz : classes.entrySet()) {
			final String className = clazz.getKey();
			final String classImpl = clazz.getValue();
			final Path filename = DIR_OUTPUT.resolve(className + ".java");
			try {
				Files.writeString(filename, classImpl);
			} catch (final IOException e) {
				throw new AssertionError(e);
			}
		}
	}

	private static void compileFile(final Path mainPath, final String main) {
		final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		if (Objects.isNull(compiler)) {
			throw new AssertionError("No Java compiler was found.");
		}
		try {
			Files.writeString(mainPath, main);
		} catch (final IOException e) {
			throw new AssertionError(e);
		}
		final String file = mainPath.toString();
		final String classpath = DIR_OUTPUT.toString();
		final String[] args = new String[]{file, "-cp", classpath};
		try {
			compiler.run(null, null, null, args);
		} catch (final NullPointerException e) {
			throw new AssertionError(e);
		}
	}

	protected static void generateClasses(final String grammar) {
		final String src = readGrammar(grammar + ".grammar");
		final Result result = Generator.parseGrammar(src);
		final Map<String, String> classes = Generator.generate(result);
		writeClasses(classes);
	}

	@SuppressWarnings("all")
	protected static void except(
			final String grammar,
			final String field,
			final String str,
			final String expected,
			final String start
	) {
		final Path mainPath = DIR_OUTPUT.resolve("Main.java");
		compileFile(mainPath, MAIN_OUTPUT.formatted(
				grammar,
				grammar, FILE_OUTPUT.toString(),
				str, start + "()", field)
		);
		final ProcessBuilder cmd = new ProcessBuilder("java", "-cp", DIR_OUTPUT.toString(), mainPath.toString());
		final Process p;
		try {
			p = cmd.start();
			p.waitFor();
		} catch (final IOException | InterruptedException e) {
			throw new AssertionError(e);
		}
		final String actual;
		try {
			actual = Files.readString(FILE_OUTPUT);
		} catch (final IOException e) {
			throw new AssertionError(e);
		}
		Assert.assertEquals(expected, actual);
	}

	@SuppressWarnings("all")
	protected static void except(final String grammar, final String str) {
		final Path mainPath = DIR_OUTPUT.resolve("Main.java");
		compileFile(mainPath, MAIN_VOID.formatted(grammar, grammar, str));
		final ProcessBuilder cmd = new ProcessBuilder("java", mainPath.toString());
		final Process p;
		try {
			p = cmd.start();
			p.waitFor();
		} catch (final IOException | InterruptedException e) {
			throw new AssertionError(e);
		}
	}

	private static long fib_(final int n, final int prev, final int acc) {
		if (n <= 0) return acc;
		else return fib_(n - 1, acc, prev + acc);
	}

	protected static double fib(final int n) {
		return (double) fib_(n, 1, 0);
	}
}
