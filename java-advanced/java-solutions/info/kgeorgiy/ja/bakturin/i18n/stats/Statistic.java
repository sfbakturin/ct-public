package info.kgeorgiy.ja.bakturin.i18n.stats;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface Statistic {
	int getOccurrences();
	int getUnique();
	String getMin();
	String getMax();
	default int getMinLength() {
		return getMinLengthSource().length();
	}
	default int getMaxLength() {
		return getMaxLengthSource().length();
	}
	String getMinLengthSource();
	String getMaxLengthSource();
	String getAverage();

	boolean isText();
	String getId();
}
