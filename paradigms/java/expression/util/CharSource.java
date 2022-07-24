package expression.util;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface CharSource {
	boolean hasNext();

	char next();

	char lookNext();

	boolean check(char c);
}
