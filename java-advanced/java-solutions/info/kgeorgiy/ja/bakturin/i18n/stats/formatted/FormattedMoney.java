package info.kgeorgiy.ja.bakturin.i18n.stats.formatted;

import info.kgeorgiy.ja.bakturin.i18n.stats.atom.AtomNumberStatistic;

import java.text.NumberFormat;
import java.util.List;
import java.util.Locale;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class FormattedMoney extends AtomNumberStatistic {
	public FormattedMoney(final List<Number> data, final Locale locale) {
		super(data, NumberFormat.getCurrencyInstance(locale));
	}

	@Override
	public String getId() {
		return "money";
	}
}
