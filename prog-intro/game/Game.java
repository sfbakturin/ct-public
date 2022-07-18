package game;

import java.util.ArrayList;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class Game {
    private final Board board;
    private final ArrayList<Pairs> players;

    public Game(final Board board, final Player player1, final Player player2) {
        this.board = board;
        this.players = new ArrayList<>();
        this.players.add(new Pairs(1, player1));
        this.players.add(new Pairs(2, player2));
    }

    public int play(final boolean log) {
        while (true) {
            for (final Pairs item : this.players) {
                final int res = makeMove(item.getPlayer(), item.getNumber(), log);
                if (res != -1) {
                    return res;
                }
            }
        }
    }

    private int makeMove(final Player player, final int no, final boolean log) {
        final GameMovement move = player.makeMove(board.getPosition(), board.getRow(), board.getCol());
        final GameState state = board.makeMove(move);
        if (log) {
            System.out.println("Logging: Player --- " + no);
            System.out.println(move);
            System.out.println(board);
            System.out.println("Logging: Result --- " + state);
            System.out.println("**********************");
        }
        return switch (state) {
            case WIN -> no;
            case LOSE -> 3 - no;
            case DRAW -> 3;
            case UNKNOWN -> -1;
        };
    }

    static class Pairs {
        private final int no;

        private final Player pl;

        public Pairs(final int no, final Player pl) {
            this.no = no;
            this.pl = pl;
        }

        public int getNumber() {
            return this.no;
        }

        public Player getPlayer() {
            return this.pl;
        }
    }
}
