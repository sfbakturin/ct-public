package info.kgeorgiy.ja.bakturin.i18n.format;

import info.kgeorgiy.ja.bakturin.i18n.TextParser;
import info.kgeorgiy.ja.bakturin.i18n.stats.Statistic;
import info.kgeorgiy.ja.bakturin.i18n.stats.formatted.*;
import info.kgeorgiy.ja.bakturin.i18n.util.Pair;

import java.text.BreakIterator;
import java.util.List;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public enum CategoryImpl implements Category {
	SENTENCES {
		private static boolean isValid(final char c) {
			return Character.isLetter(c) || Character.isDigit(c) || !Character.isWhitespace(c);
		}

		private static String removeAllUnwantedW(final String sentence) {
			int start = 0, end = sentence.length() - 1;
			for (; start < sentence.length(); start++) {
				if (isValid(sentence.charAt(start))) {
					break;
				}
			}
			for (; end > 0; end--) {
				if (isValid(sentence.charAt(end))) {
					end++;
					break;
				}
			}
			return sentence.substring(start, end);
		}

		private static List<String> removeAllUnwantedW(final List<String> sentences) {
			return sentences.stream().map(s -> removeAllUnwantedW(s)).toList();
		}

		@Override
		public Pair<Statistic, Subcategory[]> getCategory(final TextParser parser, final String text) {
			final Statistic target = new FormattedSentences(
					removeAllUnwantedW(
							parser.parse(
									text,
									BreakIterator.getSentenceInstance(parser.getLocaleInput()),
									(pos, begin, end) -> text.substring(begin, end)
							)),
					parser.getLocaleInput()
			);
			return new Pair<>(target, TEXT);
		}
	},
	WORDS {
		@Override
		public Pair<Statistic, Subcategory[]> getCategory(final TextParser parser, final String text) {
			final Statistic target = new FormattedWords(
					parser.parse(
									text,
									BreakIterator.getWordInstance(parser.getLocaleInput()),
									(pos, begin, end) -> text.substring(begin, end))
							.stream()
							.filter(
									word -> word
											.codePoints()
											.anyMatch(Character::isLetter)
							)
							.toList(),
					parser.getLocaleInput()
			);
			return new Pair<>(target, TEXT);
		}
	},
	NUMBERS {
		@Override
		public Pair<Statistic, Subcategory[]> getCategory(final TextParser parser, final String text) {
			final Statistic target = new FormattedNumbers(
					parser.parse(
							text,
							BreakIterator.getWordInstance(parser.getLocaleInput()),
							(pos, begin, end) -> parser.parseNumber(text, pos), (pos) -> {
								final Number parsedDate = parser.parseDate(text, pos);
								final Number parsedMoney = parser.parseMoney(text, pos);
								return Objects.nonNull(parsedMoney) || Objects.nonNull(parsedDate);
							}),
					parser.getLocaleInput()
			);
			return new Pair<>(target, NUMERICAL);
		}
	},
	MONEY {
		@Override
		public Pair<Statistic, Subcategory[]> getCategory(final TextParser parser, final String text) {
			final Statistic target = new FormattedMoney(
					parser.parse(
							text,
							BreakIterator.getWordInstance(parser.getLocaleInput()),
							(pos, begin, end) -> parser.parseMoney(text, pos)),
					parser.getLocaleInput()
			);
			return new Pair<>(target, NUMERICAL);
		}
	},
	DATES {
		@Override
		public Pair<Statistic, Subcategory[]> getCategory(final TextParser parser, final String text) {
			final Statistic target = new FormattedDates(
					parser.parse(
							text,
							BreakIterator.getWordInstance(parser.getLocaleInput()),
							(pos, begin, end) -> parser.parseDate(text, pos)),
					parser.getLocaleOutput()
			);
			return new Pair<>(target, NUMERICAL);
		}
	};

	private static final Subcategory[] TEXT;
	private static final Subcategory[] NUMERICAL;

	static {
		TEXT = SubcategoryImpl.values();
		NUMERICAL = new Subcategory[]{
				SubcategoryImpl.OCCURRENCES,
				SubcategoryImpl.UNIQUE,
				SubcategoryImpl.MIN,
				SubcategoryImpl.MAX,
				SubcategoryImpl.AVERAGE
		};
	}
}
