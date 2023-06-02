package info.kgeorgiy.ja.bakturin.i18n.stats;

import java.util.*;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public abstract class AbstractStatistic<BASE> implements Statistic {
	private final List<BASE> data;

	protected AbstractStatistic(final List<BASE> data) {
		this.data = data;
	}

	@Override
	public int getOccurrences() {
		return data.size();
	}

	@Override
	public int getUnique() {
		return setOf(data).size();
	}

	@Override
	public String getMin() {
		final BASE min = setOf(data).stream().findFirst().orElse(null);
		return Objects.isNull(min) ? null : formatOf(min);
	}

	@Override
	public String getMax() {
		final BASE max = setOf(data).stream().reduce((f, s) -> s).orElse(null);
		return Objects.isNull(max) ? null : formatOf(max);
	}

	@Override
	public String getMinLengthSource() {
		final List<String> sort = sortedStringListOf(data);
		return sort.isEmpty() ? null : sort.get(0);
	}

	@Override
	public String getMaxLengthSource() {
		final List<String> sort = sortedStringListOf(data);
		return sort.isEmpty() ? null : sort.get(sort.size() - 1);
	}

	@Override
	public String getAverage() {
		final OptionalDouble opt = averageOf(data);
		return opt.isEmpty() ? null : formatOf(opt.getAsDouble());
	}

	protected abstract Set<BASE> setOf(final List<BASE> data);
	protected abstract List<String> sortedStringListOf(final List<BASE> data);
	protected abstract OptionalDouble averageOf(final List<BASE> data);
	protected abstract String formatOf(final BASE base);
	protected abstract String formatOf(final double base);
}
