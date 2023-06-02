package info.kgeorgiy.ja.bakturin.i18n.format;

import info.kgeorgiy.ja.bakturin.i18n.stats.Statistic;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface Subcategory {
	String format(Statistic target, Writer writer);
}
