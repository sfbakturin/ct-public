package bakturin.lab4.dynamo.assets.term;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface TerminalNode {
	boolean isRegular();

	boolean isString();

	boolean isEpsilon();

	boolean isStart();

	String asString();

	String name();

	String create(String grammar, String name);
}
