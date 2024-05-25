package game;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface Board {
	Position getPosition();

	GameState makeMove(GameMovement move);

	int getRow();

	int getCol();
}
