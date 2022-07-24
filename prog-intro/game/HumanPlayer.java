package game;

import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class HumanPlayer implements Player {
	private final Scanner input;

	public HumanPlayer(final Scanner input) {
		this.input = input;
	}

	@Override
	public GameMovement makeMove(final Position position, final int r, final int c) {
		final TicTacToeBoard board = (TicTacToeBoard) position;
		System.out.println(position);
		System.out.print("Please enter coordinates for your turn, " + position.getTurn() + " --- ");
		String[] s = this.input.nextLine().split(" ");
		while (isNumeric(s[0]) || isNumeric(s[1]) || s.length != 2) {
			System.out.print("Try again, " + position.getTurn() + " --- ");
			s = this.input.nextLine().split(" ");
		}
		int row = Integer.parseInt(s[0]) - 1, col = Integer.parseInt(s[1]) - 1;
		GameMovement checked = new GameMovement(position.getTurn(), row, col);
		while (board.isValid(checked)) {
			System.out.print("Try again, " + position.getTurn() + " --- ");
			s = this.input.nextLine().split(" ");
			while (isNumeric(s[0]) || isNumeric(s[1]) || s.length != 2) {
				System.out.print("Try again, " + position.getTurn() + " --- ");
				s = this.input.nextLine().split(" ");
			}
			row = Integer.parseInt(s[0]) - 1;
			col = Integer.parseInt(s[1]) - 1;
			checked = new GameMovement(position.getTurn(), row, col);
		}
		System.out.println();
		return checked;
	}

	private static boolean isNumeric(final String s) {
		if (s == null) {
			return true;
		}
		try {
			Integer.parseInt(s);
		} catch (final NumberFormatException err) {
			return true;
		}
		return false;
	}
}
