import java.util.Locale;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class O {
    private static double f(final double i) {
        return Math.pow(i, 2) + Math.sqrt(i);
    }

    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in).useLocale(Locale.US);
        final double c = in.nextDouble();
        in.close();
        double left = 0;
        double right = Math.pow(10, 10);
        final double eps = 0.0000001;
        while (right - left > eps) {
            double mid = (right + left) / 2;
            if (f(mid) <= c) {
                left = mid;
            } else {
                right = mid;
            }
        }
        System.out.println(left);
    }
}
