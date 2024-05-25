package game;

import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Main {
	private final static int M, N, K, COUNT;

	static {
		M = 256;
		N = 256;
		K = 3;
		COUNT = 2;
	}

	public static void main(final String... args) {
		System.out.println("**********GAME STARTED**********");
		int p1 = 0, p2 = 0, p = 0;
		boolean swap = false;
		Board board = new TicTacToeBoard(M, N, K, false);
		final Player player1 = new HumanPlayer(
				new Scanner(System.in)
		);
		final Player player2 = new HumanPlayer(
				new Scanner(System.in)
		);
		int result = new Game(
				board, player1, player2
		).play(true);
		while (true) {
			switch (result) {
				case 1 -> {
					p1++;
					p++;
				}
				case 2 -> {
					p2++;
					p++;
				}
				case 3 -> p++;
				default -> throw new AssertionError("Fatal error --- no such result was found: " + result);
			}
			if (p == COUNT) {
				if (p1 > p2) {
					System.out.println("FIRST PLAYER");
					break;
				}
				if (p2 > p1) {
					System.out.println("SECOND PLAYER");
					break;
				}
				System.out.println("DRAW");
				break;
			}
			swap = !swap;
			board = new TicTacToeBoard(M, N, K, swap);
			result = new Game(
					board, player1, player2
			).play(true);
		}
	}
}
