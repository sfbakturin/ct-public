import java.util.Scanner;
import static java.lang.Math.min;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class H {
    private static int m(final char s1, final char s2) {
        if (s1 == s2) {
            return 0;
        } else {
            return 1;
        }
    }

    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final String s1 = " " + in.nextLine(), s2 = " " +  in.nextLine();
        in.close();
        final int[][] d = new int[s1.length() + 1][s2.length() + 1];
        for (int i = 0; i < s1.length() + 1; i++) {
            for (int j = 0; j < s2.length() + 1;j++) {
                if (i == 0 && j == 0) {
                    d[i][j] = 0;
                }
                if (j == 0 && i > 0) {
                    d[i][j] = i;
                }
                if (i == 0 && j > 0) {
                    d[i][j] = j;
                }
                if (i > 0 && j > 0) {
                    d[i][j] = min(min(d[i][j - 1] + 1, d[i - 1][j] + 1), min(d[i - 1][j - 1] + m(s1.charAt(i - 1), s2.charAt(j - 1)), Integer.MAX_VALUE));
                }
            }
        }
        System.out.println(d[s1.length()][s2.length()]);
    }
}
