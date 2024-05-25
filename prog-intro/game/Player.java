package game;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */
public interface Player {
	GameMovement makeMove(final Position position, final int r, final int c);
}
