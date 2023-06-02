package info.kgeorgiy.ja.bakturin.i18n;

import info.kgeorgiy.ja.bakturin.i18n.format.Formatter;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DateFormat;
import java.text.Format;
import java.text.NumberFormat;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.ResourceBundle;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class TextStatisticsTester {
	private final static String LS = System.lineSeparator();
	private final static String TAB = "    ";
	private final static String FORMAT_START = "===> Start of" + " " + "\"" + "%s" + "\"." + LS;
	private final static String FORMAT_SUCCESS = TAB + " " + "%s" + " " + "successfully finished." + LS;
	private final static String FORMAT_FAIL = TAB + " " + "%s" + " " + "failed, because of" + " " + "%s." + LS;

	private static final String TEXT_STATISTICS_NAME = "info.kgeorgiy.ja.bakturin.i18n.TextStatistics";
	private static final String BASENAME = "%s.%s".formatted(Formatter.class.getPackageName(), "stats");

	private String testMethodName;

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

	private static Class<?> findMain() {
		try {
			return Class.forName(TEXT_STATISTICS_NAME);
		} catch (final ClassNotFoundException e) {
			throw new RuntimeException(e);
		}
	}

	private static Method findMethod() {
		try {
			return findMain().getMethod("main", String[].class);
		} catch (final NoSuchMethodException e) {
			throw new RuntimeException(e);
		}
	}

	private static void runMethod(final Object args) {
		try {
			findMethod().invoke(null, args);
		} catch (final IllegalAccessException | InvocationTargetException e) {
			throw new RuntimeException(e);
		}
	}

	private static Object transform(
			final String arg1,
			final String arg2,
			final String arg3,
			final String arg4
	) {
		return new String[]{arg1, arg2, arg3, arg4};
	}

	private static void runMain(
			final String localeInput,
			final String localeOutput,
			final String fileInput,
			final String fileOutput
	) {
		runMethod(transform(localeInput, localeOutput, fileInput, fileOutput));
	}

	protected static void runMain(
			final Locale localeInput,
			final Locale localeOutput,
			final Path fileInput,
			final Path fileOutput
	) {
		runMain(
				localeInput.toLanguageTag(),
				localeOutput.toLanguageTag(),
				fileInput.toString(),
				fileOutput.toString()
		);
	}

	private static String[] getFullPureName() {
		final String packageName = TextStatisticsTester.class.getPackage().getName();
		return packageName.split("\\.");
	}

	private static String getAbsolutePath() {
		try {
			return Path.of(TextStatisticsTester.class.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
		} catch (final URISyntaxException e) {
			throw new RuntimeException(e);
		}
	}

	protected static Path getPath(final String... name) {
		Path base = Paths.get(getAbsolutePath());
		for (final String p : getFullPureName()) {
			base = base.resolve(p);
		}
		for (final String p : name) {
			base = base.resolve(p);
		}
		return base;
	}

	private static String readFile(final Path p) {
		try {
			return Files.readString(p, StandardCharsets.UTF_8);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	protected static void checkSolution(final String solution, final Path pO) {
		Assert.assertEquals(solution, readFile(pO));
	}

	protected static Format instanceNumber(final Locale locale) {
		return NumberFormat.getNumberInstance(locale);
	}

	protected static Format instanceMoney(final Locale locale) {
		return NumberFormat.getCurrencyInstance(locale);
	}

	protected static Format instanceDate(final Locale locale) {
		return DateFormat.getDateInstance(DateFormat.DEFAULT, locale);
	}

	private static String format(final Locale locale, final String key) {
		return ResourceBundle.getBundle(BASENAME, locale).getString("statistics_%s".formatted(key));
	}

	private static String format(final Locale locale, final String key, final String subkey) {
		return format(locale, "%s_%s".formatted(key, subkey));
	}

	protected static String writeSolution(
			final String filename,
			final Locale locale,
			final StringSolution sentences,
			final StringSolution words,
			final NumberSolution numbers,
			final NumberSolution money,
			final NumberSolution dates
	) {
		try {
			final StringBuilder answer = new StringBuilder();
			answer.append(format(locale, CATEGORY_HEADER)).append(" \"").append(filename).append("\"").append(LS);
			answer.append(format(locale, CATEGORY_SUMMARY)).append(".").append(LS);
			answer.append(TAB).append(format(locale, CATEGORY_SENTENCES, SUBCATEGORY_OCCURRENCES)).append(": ").append(sentences.occurrences()).append(".").append(LS);
			answer.append(TAB).append(format(locale, CATEGORY_WORDS, SUBCATEGORY_OCCURRENCES)).append(": ").append(words.occurrences()).append(".").append(LS);
			answer.append(TAB).append(format(locale, CATEGORY_NUMBERS, SUBCATEGORY_OCCURRENCES)).append(": ").append(numbers.getOccurrences()).append(".").append(LS);
			answer.append(TAB).append(format(locale, CATEGORY_MONEY, SUBCATEGORY_OCCURRENCES)).append(": ").append(money.getOccurrences()).append(".").append(LS);
			answer.append(TAB).append(format(locale, CATEGORY_DATES, SUBCATEGORY_OCCURRENCES)).append(": ").append(dates.getOccurrences()).append(".").append(LS);
			final List<Object> objs = List.of(sentences, words, numbers, money, dates);
			int i = 0;
			for (final PairCategorySubcategories item : CATEGORY_SUBCATEGORY) {
				answer.append(format(locale, item.left)).append(".").append(LS);
				for (int j = 0; j < item.right.size(); j++) {
					final String sub = item.right.get(j);
					answer.append(TAB).append(format(locale, item.left, sub)).append(": ");
					if (objs.get(i) instanceof StringSolution) {
						final StringSolution obj = (StringSolution) objs.get(i);
						final String val = STRING_METHODS.get(j).invoke(obj).toString();
						answer.append(Objects.isNull(val) ? format(locale, "error") : val).append(".").append(LS);
					} else if (objs.get(i) instanceof NumberSolution) {
						final NumberSolution obj = (NumberSolution) objs.get(i);
						final Object val = NUMBER_METHODS.get(j).invoke(obj);
						answer.append(Objects.isNull(val) ? format(locale, "error") : val.toString()).append(".").append(LS);
					} else {
						throw new RuntimeException("unknown");
					}
				}
				i++;
			}
			return answer.toString();
		} catch (final InvocationTargetException | IllegalAccessException e) {
			throw new RuntimeException(e);
		}
	}

	private static final String CATEGORY_HEADER = "header";
	private static final String CATEGORY_SUMMARY = "summary";
	private static final String CATEGORY_SENTENCES = "sentences";
	private static final String CATEGORY_WORDS = "words";
	private static final String CATEGORY_NUMBERS = "numbers";
	private static final String CATEGORY_MONEY = "money";
	private static final String CATEGORY_DATES = "dates";

	private static final String SUBCATEGORY_OCCURRENCES = "occurrences";
	private static final String SUBCATEGORY_UNIQUE = "unique";
	private static final String SUBCATEGORY_MIN = "min";
	private static final String SUBCATEGORY_MAX = "max";
	private static final String SUBCATEGORY_MIN_LENGTH = "min_length";
	private static final String SUBCATEGORY_MAX_LENGTH = "max_length";
	private static final String SUBCATEGORY_MIN_LENGTH_SOURCE = "min_length_source";
	private static final String SUBCATEGORY_MAX_LENGTH_SOURCE = "max_length_source";
	private static final String SUBCATEGORY_AVERAGE = "average";

	private static final PairCategorySubcategories[] CATEGORY_SUBCATEGORY;
	private static final List<Method> STRING_METHODS;
	private static final List<Method> NUMBER_METHODS;

	protected static NumberSolution getNumberDefault() {
		return new NumberSolutionImpl(null, 0, 0, null, null, null);
	}

	protected static final class NumberSolutionImpl implements NumberSolution {
		private final Format format;
		private final int occurrences;
		private final int unique;
		private final Number min;
		private final Number max;
		private final Number average;

		public NumberSolutionImpl(
				final Format format,
				final int occurrences,
				final int unique,
				final Number min,
				final Number max,
				final Number average
		) {
			this.format = format;
			this.occurrences = occurrences;
			this.unique = unique;
			this.min = min;
			this.max = max;
			this.average = average;
		}

		@Override
		public int getOccurrences() {
			return occurrences;
		}

		@Override
		public int getUnique() {
			return unique;
		}

		@Override
		public String getMin() {
			return Objects.isNull(min) ? null : format.format(min);
		}

		@Override
		public String getMax() {
			return Objects.isNull(max) ? null : format.format(max);
		}

		@Override
		public String getAverage() {
			return Objects.isNull(average) ? null : format.format(average);
		}
	}

	protected record StringSolutionImpl(int occurrences, int unique, String min, String max, String minLengthSource,
	                                    String maxLengthSource, Number average) implements StringSolution {


		@Override
		public String min() {
			return Objects.isNull(min) ? null : "\"" + min + "\"";
		}

		@Override
		public String max() {
			return Objects.isNull(max) ? null : "\"" + max + "\"";
		}

		@Override
		public String minLengthSource() {
			return Objects.isNull(maxLengthSource) ? null : "\"" + minLengthSource + "\"";
		}

		@Override
		public String maxLengthSource() {
			return Objects.isNull(maxLengthSource) ? null : "\"" + maxLengthSource + "\"";
		}


	}

	protected interface NumberSolution {
		int getOccurrences();

		int getUnique();

		String getMin();

		String getMax();

		String getAverage();
	}

	protected interface StringSolution {
		int occurrences();

		int unique();

		String min();

		String max();

		default int getMinLength() {
			final int len = minLengthSource().length();
			return len > 0 ? len - 2 : 0;
		}

		default int getMaxLength() {
			final int len = maxLengthSource().length();
			return len > 0 ? len - 2 : 0;
		}

		String minLengthSource();

		String maxLengthSource();

		Number average();
	}

	static {
		CATEGORY_SUBCATEGORY = new PairCategorySubcategories[]{
				new PairCategorySubcategories(CATEGORY_SENTENCES, List.of(SUBCATEGORY_OCCURRENCES, SUBCATEGORY_UNIQUE, SUBCATEGORY_MIN, SUBCATEGORY_MAX, SUBCATEGORY_MIN_LENGTH, SUBCATEGORY_MAX_LENGTH, SUBCATEGORY_MIN_LENGTH_SOURCE, SUBCATEGORY_MAX_LENGTH_SOURCE, SUBCATEGORY_AVERAGE)),
				new PairCategorySubcategories(CATEGORY_WORDS, List.of(SUBCATEGORY_OCCURRENCES, SUBCATEGORY_UNIQUE, SUBCATEGORY_MIN, SUBCATEGORY_MAX, SUBCATEGORY_MIN_LENGTH, SUBCATEGORY_MAX_LENGTH, SUBCATEGORY_MIN_LENGTH_SOURCE, SUBCATEGORY_MAX_LENGTH_SOURCE, SUBCATEGORY_AVERAGE)),
				new PairCategorySubcategories(CATEGORY_NUMBERS, List.of(SUBCATEGORY_OCCURRENCES, SUBCATEGORY_UNIQUE, SUBCATEGORY_MIN, SUBCATEGORY_MAX, SUBCATEGORY_AVERAGE)),
				new PairCategorySubcategories(CATEGORY_MONEY, List.of(SUBCATEGORY_OCCURRENCES, SUBCATEGORY_UNIQUE, SUBCATEGORY_MIN, SUBCATEGORY_MAX, SUBCATEGORY_AVERAGE)),
				new PairCategorySubcategories(CATEGORY_DATES, List.of(SUBCATEGORY_OCCURRENCES, SUBCATEGORY_UNIQUE, SUBCATEGORY_MIN, SUBCATEGORY_MAX, SUBCATEGORY_AVERAGE))
		};
		try {
			STRING_METHODS = List.of(
					StringSolution.class.getMethod("occurrences"),
					StringSolution.class.getMethod("unique"),
					StringSolution.class.getMethod("min"),
					StringSolution.class.getMethod("max"),
					StringSolution.class.getMethod("getMinLength"),
					StringSolution.class.getMethod("getMaxLength"),
					StringSolution.class.getMethod("minLengthSource"),
					StringSolution.class.getMethod("maxLengthSource"),
					StringSolution.class.getMethod("average")
			);
			NUMBER_METHODS = List.of(
					NumberSolution.class.getMethod("getOccurrences"),
					NumberSolution.class.getMethod("getUnique"),
					NumberSolution.class.getMethod("getMin"),
					NumberSolution.class.getMethod("getMax"),
					NumberSolution.class.getMethod("getAverage")
			);
		} catch (final NoSuchMethodException e) {
			throw new RuntimeException(e);
		}
	}

	private record PairCategorySubcategories(String left, List<String> right) {
	}
}
