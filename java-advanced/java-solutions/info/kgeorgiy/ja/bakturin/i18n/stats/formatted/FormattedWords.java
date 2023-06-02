package info.kgeorgiy.ja.bakturin.i18n.stats.formatted;

import info.kgeorgiy.ja.bakturin.i18n.stats.atom.AtomStringStatistic;

import java.util.List;
import java.util.Locale;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class FormattedWords extends AtomStringStatistic {
	public FormattedWords(final List<String> data, final Locale locale) {
		super(data, locale);
	}

	@Override
	public String getId() {
		return "words";
	}
}
