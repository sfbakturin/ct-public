package expression.generic;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Main {
	public static void main(final String[] args) {
		final String mode = args[0].substring(1);
		final String expr = args[1];
		final GenericTabulator tab = new GenericTabulator();
		final Object[][][] answ = tab.tabulate(mode, expr, -2, 2, -2, 2, -2, 2);
		for (int i = 0; i < 5; i++) {
			for (int j = 0; j < 5; j++) {
				for (int k = 0; k < 5; k++) {
					System.out.println("R[" + i + "][" + j + "][" + k + "] = " + answ[i][j][k]);
				}
			}
		}
	}
}
