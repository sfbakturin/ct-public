package game;

import java.util.Random;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class RandomPlayer implements Player {
	private final Random random = new Random();

	@Override
	public GameMovement makeMove(final Position position, final int r, final int c) {
		return new GameMovement(
				position.getTurn(),
				random.nextInt(r),
				random.nextInt(c)
		);
	}
}
