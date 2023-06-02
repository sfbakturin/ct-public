package info.kgeorgiy.ja.bakturin.i18n.format;

import info.kgeorgiy.ja.bakturin.i18n.TextParser;
import info.kgeorgiy.ja.bakturin.i18n.stats.Statistic;
import info.kgeorgiy.ja.bakturin.i18n.util.Pair;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface Category {
	Pair<Statistic, Subcategory[]> getCategory(TextParser parser, String text);
}
