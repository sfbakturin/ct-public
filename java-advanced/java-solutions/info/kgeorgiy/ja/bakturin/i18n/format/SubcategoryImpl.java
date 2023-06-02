package info.kgeorgiy.ja.bakturin.i18n.format;

import info.kgeorgiy.ja.bakturin.i18n.stats.Statistic;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public enum SubcategoryImpl implements Subcategory {
	OCCURRENCES {
		@Override
		public String format(final Statistic target, final Writer writer) {
			return writer.subcategory(target.getId(), Writer.OCCURRENCES, target.getOccurrences(), false);
		}
	},
	UNIQUE {
		@Override
		public String format(final Statistic target, final Writer writer) {
			return writer.subcategory(target.getId(), Writer.UNIQUE, target.getUnique(), false);
		}
	},
	MIN {
		@Override
		public String format(final Statistic target, final Writer writer) {
			return writer.subcategory(target.getId(), Writer.MIN, target.getMin(), target.isText());
		}
	},
	MAX {
		@Override
		public String format(final Statistic target, final Writer writer) {
			return writer.subcategory(target.getId(), Writer.MAX, target.getMax(), target.isText());
		}
	},
	MIN_LENGTH {
		@Override
		public String format(final Statistic target, final Writer writer) {
			return writer.subcategory(target.getId(), Writer.MIN_LENGTH, target.getMinLength(), false);
		}
	},
	MAX_LENGTH {
		@Override
		public String format(final Statistic target, final Writer writer) {
			return writer.subcategory(target.getId(), Writer.MAX_LENGTH, target.getMaxLength(), false);
		}
	},
	MIN_LENGTH_SOURCE {
		@Override
		public String format(final Statistic target, final Writer writer) {
			return writer.subcategory(
					target.getId(),
					Writer.MIN_LENGTH_SOURCE,
					target.getMinLengthSource(),
					target.isText()
			);
		}
	},
	MAX_LENGTH_SOURCE {
		@Override
		public String format(final Statistic target, final Writer writer) {
			return writer.subcategory(
					target.getId(),
					Writer.MAX_LENGTH_SOURCE,
					target.getMaxLengthSource(),
					target.isText()
			);
		}
	},
	AVERAGE {
		@Override
		public String format(final Statistic target, final Writer writer) {
			return writer.subcategory(target.getId(), Writer.AVERAGE, target.getAverage(), false);
		}
	}
}
