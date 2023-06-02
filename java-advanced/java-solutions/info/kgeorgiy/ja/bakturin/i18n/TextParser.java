package info.kgeorgiy.ja.bakturin.i18n;

import info.kgeorgiy.ja.bakturin.i18n.util.TriFunction;

import java.text.BreakIterator;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.ParsePosition;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class TextParser {
	private final Locale localeInput;
	private final Locale localeOutput;
	private final NumberFormat moneyLocaleFormat;
	private final NumberFormat numberLocaleFormat;
	private final List<DateFormat> availableDateLocaleFormats;

	public TextParser(final Locale localeInput, final Locale localeOutput) {
		this.localeInput = localeInput;
		this.localeOutput = localeOutput;
		moneyLocaleFormat = NumberFormat.getCurrencyInstance(localeInput);
		numberLocaleFormat = NumberFormat.getNumberInstance(localeInput);
		availableDateLocaleFormats = Stream.of(DateFormat.FULL, DateFormat.MEDIUM, DateFormat.SHORT).map(i -> DateFormat.getDateInstance(i, localeInput)).toList();
	}

	public Locale getLocaleInput() {
		return localeInput;
	}

	public Locale getLocaleOutput() {
		return localeOutput;
	}

	public <R> List<R> parse(
			final String text,
			final BreakIterator iterator,
			final TriFunction<ParsePosition, Integer, Integer, R> method
	) {
		return parse(text, iterator, method, (index) -> false);
	}

	public <R> List<R> parse(
			final String text,
			final BreakIterator iterator,
			final TriFunction<ParsePosition, Integer, Integer, R> method,
			final Predicate<ParsePosition> before
	) {
		iterator.setText(text);
		final List<R> result = new ArrayList<>();
		final ParsePosition pos = new ParsePosition(0);
		int skip = 0;
		for (
				int begin = iterator.first(), end = iterator.next();
				end != BreakIterator.DONE;
				begin = end, end = iterator.next()
		) {
			if (skip >= end) {
				continue;
			}
			if (before.test(pos)) {
				begin = pos.getIndex();
			} else {
				final R parsed = method.apply(pos, begin, end);
				if (Objects.nonNull(parsed)) {
					begin = pos.getIndex();
					result.add(parsed);
				}
			}
			skip = begin;
			pos.setIndex(begin + 1);
		}
		return result;
	}

	public Number parseNumber(final String text, final ParsePosition pos) {
		return numberLocaleFormat.parse(text, pos);
	}

	public Number parseMoney(final String text, final ParsePosition pos) {
		return moneyLocaleFormat.parse(text, pos);
	}

	public Number parseDate(final String text, final ParsePosition pos) {
		for (final DateFormat format : availableDateLocaleFormats) {
			final Date parsed = format.parse(text, pos);
			if (Objects.nonNull(parsed)) {
				return parsed.getTime();
			}
		}
		return null;
	}
}
