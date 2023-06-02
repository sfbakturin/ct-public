package info.kgeorgiy.ja.bakturin.i18n.stats.atom;

import info.kgeorgiy.ja.bakturin.i18n.stats.AbstractStatistic;

import java.text.Format;
import java.util.*;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public abstract class AtomNumberStatistic extends AbstractStatistic<Number> {
	private final Format format;

	protected AtomNumberStatistic(final List<Number> data, final Format format) {
		super(data);
		this.format = format;
	}

	@Override
	public Set<Number> setOf(final List<Number> data) {
		return new HashSet<>(data);
	}

	@Override
	public List<String> sortedStringListOf(final List<Number> data) {
		throw new UnsupportedOperationException();
	}

	@Override
	public OptionalDouble averageOf(final List<Number> data) {
		return data.stream().mapToDouble(Number::doubleValue).average();
	}

	@Override
	public String formatOf(final Number base) {
		return useFormat(base);
	}

	@Override
	public String formatOf(final double base) {
		return useFormat(base);
	}

	private String useFormat(final Number base) {
		return format.format(base);
	}

	@Override
	public boolean isText() {
		return false;
	}
}
