package game;

import java.util.Arrays;
import java.util.Map;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class TicTacToeBoard implements Board, Position {
    private static final Map<Cell, Character> CELL_CHARACTER_MAP = Map.of(
            Cell.EMPTY, '.',
            Cell.CROSS, 'x',
            Cell.CIRCLE, 'o'
    );
    private final Cell[][] field;
    private Cell turn;
    private final int r;
    private final int c;

    @Override
    public int getRow() {
        return this.r;
    }

    @Override
    public int getCol() {
        return this.c;
    }

    private final int k;

    public TicTacToeBoard(final int r, final int c, final int k, final boolean swap) {
        this.field = new Cell[r][c];
        for (Cell[] row : this.field) {
            Arrays.fill(row, Cell.EMPTY);
        }
        if (swap) {
            this.turn = Cell.CIRCLE;
        } else {
            this.turn = Cell.CROSS;
        }
        this.r = r;
        this.c = c;
        this.k = k;
    }

    @Override
    public Position getPosition() {
        return this;
    }

    @Override
    public GameState makeMove(final GameMovement move) {
        if (this.isValid(move)) {
            return GameState.LOSE;
        }
        this.field[move.getRow()][move.getColumn()] = move.getValue();
        if (this.checkWin()) {
            return GameState.WIN;
        }
        if (this.checkDraw()) {
            return GameState.DRAW;
        }
        this.turn = this.turn == Cell.CROSS ? Cell.CIRCLE : Cell.CROSS;
        return GameState.UNKNOWN;
    }

    private boolean checkWin() {
        for (int r = 0; r < this.r; r++) {
            int count = 0;
            for (int c = 0; c < this.c; c++) {
                if (this.field[r][c] == this.turn) {
                    count++;
                    if (count == this.k) {
                        return true;
                    }
                } else {
                    count = 0;
                }
            }
            if (count == this.k) {
                return true;
            }
        }
        for (int c = 0; c < this.c; c++) {
            int count = 0;
            for (int r = 0; r < this.r; r++) {
                if (this.field[r][c] == this.turn) {
                    count++;
                    if (count == this.k) {
                        return true;
                    }
                } else {
                    count = 0;
                }
            }
            if (count == this.k) {
                return true;
            }
        }
        for (int r = 0; r < this.r; r++) {
            for (int c = 0; c < this.c; c++) {
                int count = 0;
                int r1 = r, c1 = c;
                while (r1 != this.r && c1 != this.c) {
                    if (this.field[r1][c1] == this.turn) {
                        count++;
                        if (count == this.k) {
                            return true;
                        }
                    } else {
                        count = 0;
                    }
                    r1++;
                    c1++;
                }
                if (count == this.k) {
                    return true;
                }
            }
        }
        for (int r = this.r - 1; r >= 0; r--) {
            for (int c = 0; c < this.c; c++) {
                int count = 0;
                int r1 = r, c1 = c;
                while (r1 != -1 && c1 != this.c) {
                    if (this.field[r1][c1] == this.turn) {
                        count++;
                        if (count == this.k) {
                            return true;
                        }
                    } else {
                        count = 0;
                    }
                    r1--;
                    c1++;
                }
                if (count == this.k) {
                    return true;
                }
            }
        }
        return false;
    }

    private boolean checkDraw() {
        int count = 0;
        for (int r = 0; r < this.r; r++) {
            for (int c = 0; c < this.c; c++) {
                if (this.field[r][c] == Cell.EMPTY) {
                    count++;
                }
            }
        }
        return count == 0;
    }

    public boolean isValid(final GameMovement move) {
        return 0 > move.getRow() || move.getRow() >= this.r
                || 0 > move.getColumn() || move.getColumn() >= this.c
                || this.field[move.getRow()][move.getColumn()] != Cell.EMPTY
                || this.turn != move.getValue();
    }

    @Override
    public Cell getTurn() {
        return this.turn;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(" ");
        for (int c = 0; c < this.c; c++) {
            sb.append('\t').append(c + 1);
        }
        sb.append(System.lineSeparator());
        for (int r = 0; r < this.r; r++) {
            sb.append(r + 1);
            for (final Cell cell : this.field[r]) {
                sb.append('\t').append(CELL_CHARACTER_MAP.get(cell));
            }
            sb.append(System.lineSeparator());
        }
        return sb.toString();
    }
}
