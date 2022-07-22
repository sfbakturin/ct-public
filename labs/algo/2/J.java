import java.util.Scanner;
import static java.lang.Math.min;
import static java.lang.Math.max;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class J {
    private static int tripleMin(final int a, final int b, final int c) {
        return min(min(a, b), c);
    }

    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int N = in.nextInt(), M = in.nextInt();
        final int[][] a = new int[N][M];
        in.nextLine();
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < M; j++) {
                a[i][j] = in.nextInt();
            }
        }
        in.close();
        int maximum = a[0][0];
        for (int i = 1; i < N; i++) {
            for (int j = 1; j < M; j++) {
                if (a[i][j] != 0) {
                    a[i][j] = tripleMin(a[i - 1][j - 1], a[i - 1][j], a[i][j - 1]) + 1;
                    maximum = max(maximum, a[i][j]);
                }
            }
        }
        System.out.println(maximum);
    }
}
