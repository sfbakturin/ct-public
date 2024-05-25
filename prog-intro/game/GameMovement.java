package game;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class GameMovement {
	private final Cell val;
	private final int row;
	private final int col;

	public GameMovement(final Cell val, final int row, final int col) {
		this.val = val;
		this.row = row;
		this.col = col;
	}

	public Cell getValue() {
		return this.val;
	}

	public int getRow() {
		return this.row;
	}

	public int getColumn() {
		return this.col;
	}

	@Override
	public String toString() {
		return String.format("Move(%s, %d, %d)", this.val, this.row + 1, this.col + 1);
	}
}
