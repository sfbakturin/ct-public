package info.kgeorgiy.ja.bakturin.i18n.stats.formatted;

import info.kgeorgiy.ja.bakturin.i18n.stats.atom.AtomNumberStatistic;

import java.text.DateFormat;
import java.util.List;
import java.util.Locale;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class FormattedDates extends AtomNumberStatistic {
	public FormattedDates(final List<Number> data, final Locale locale) {
		super(data, DateFormat.getDateInstance(DateFormat.DEFAULT, locale));
	}

	@Override
	public String getId() {
		return "dates";
	}
}
