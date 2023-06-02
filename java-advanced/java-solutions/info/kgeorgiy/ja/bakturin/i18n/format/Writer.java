package info.kgeorgiy.ja.bakturin.i18n.format;

import info.kgeorgiy.ja.bakturin.i18n.stats.Statistic;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class Writer {
	public static final String OCCURRENCES = "occurrences";
	public static final String UNIQUE = "unique";
	public static final String MIN = "min";
	public static final String MAX = "max";
	public static final String MIN_LENGTH = "min_length";
	public static final String MIN_LENGTH_SOURCE = "min_length_source";
	public static final String MAX_LENGTH = "max_length";
	public static final String MAX_LENGTH_SOURCE = "max_length_source";
	public static final String AVERAGE = "average";

	private final Formatter formatter;

	public Writer(final Locale locale) {
		formatter = new Formatter(locale);
	}

	public String header(final String filename) {
		return formatter.header(filename);
	}

	public String category(final String category) {
		return formatter.category(Formatter.section(category));
	}

	public String subcategory(
			final String category,
			final String subcategory,
			final Object value,
			final boolean marked
	) {
		return formatter.subcategory(Formatter.subsection(category, subcategory), value, marked);
	}

	public List<String> format(final Statistic target, final Subcategory[] subs) {
		return Arrays.stream(subs).map(stat -> stat.format(target, this)).toList();
	}
}
