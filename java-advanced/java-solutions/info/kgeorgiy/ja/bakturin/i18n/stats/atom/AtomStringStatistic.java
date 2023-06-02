package info.kgeorgiy.ja.bakturin.i18n.stats.atom;

import info.kgeorgiy.ja.bakturin.i18n.stats.AbstractStatistic;

import java.text.Collator;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public abstract class AtomStringStatistic extends AbstractStatistic<String> {
	private final Locale locale;

	protected AtomStringStatistic(final List<String> data, final Locale locale) {
		super(data);
		this.locale = locale;
	}

	@Override
	public Set<String> setOf(final List<String> data) {
		return data.stream().collect(Collectors.toCollection(() -> new TreeSet<>(Collator.getInstance(locale))));
	}

	@Override
	public List<String> sortedStringListOf(final List<String> data) {
		return data.stream().sorted(Comparator.comparingInt(String::length)).toList();
	}

	@Override
	public OptionalDouble averageOf(final List<String> data) {
		return data.stream().mapToInt(String::length).average();
	}

	@Override
	public String formatOf(final String base) {
		return base;
	}

	@Override
	public String formatOf(final double base) {
		return String.valueOf(base);
	}

	@Override
	public boolean isText() {
		return true;
	}
}
