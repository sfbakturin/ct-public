package info.kgeorgiy.ja.bakturin.i18n.format;

import java.util.Locale;
import java.util.Objects;
import java.util.ResourceBundle;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class Formatter {
	private static final String BASENAME = "%s.%s".formatted(Formatter.class.getPackageName(), "stats");
	private static final String TAB = "    ";
	private static final String LN = System.lineSeparator();
	private final static String FORMAT_SECTION = "statistics_%s";
	private final static String FORMAT_SUBSECTION = "%s_%s";

	private final ResourceBundle bundle;

	public Formatter(final Locale locale) {
		bundle = ResourceBundle.getBundle(BASENAME, locale);
	}

	public String header(final String filename) {
		return ("%s \"%s\"" + LN).formatted(bundle.getString(section("header")), filename);
	}

	public String category(final String key) {
		return ("%s." + LN).formatted(bundle.getString(key));
	}

	public String subcategory(final String key, final Object value, final boolean marked) {
		if (Objects.isNull(value)) {
			return (TAB + "%s: %s." + LN).formatted(bundle.getString(key), bundle.getString(section("error")));
		} else {
			return (marked ? (TAB + "%s: \"%s\"." + LN) : (TAB + "%s: %s." + LN)).formatted(bundle.getString(key), value.toString());
		}
	}

	public static String section(final String section) {
		return FORMAT_SECTION.formatted(section);
	}

	public static String subsection(final String section, final String subsection) {
		return section(FORMAT_SUBSECTION.formatted(section, subsection));
	}
}
