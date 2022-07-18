import java.util.ArrayList;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class G {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt(), m = in.nextInt();
        in.nextLine();
        final long[] a = new long[n];
        final long[] s = new long[n];
        long sum = 0;
        for (int i = 0; i < n; i++) {
            a[i] = in.nextLong();
            sum += a[i];
            s[i] = sum;
        }
        in.nextLine();
        final ArrayList<Long> answers = new ArrayList<>();
        for (int i = 0; i < m; i++) {
            String[] index = in.nextLine().split(" ");
            answers.add(s[Integer.parseInt(index[1]) - 1] - s[Integer.parseInt(index[0]) - 1] + a[Integer.parseInt(index[0]) - 1]);
        }
        in.close();
        for (final Long longer : answers) {
            System.out.println(longer);
        }
    }
}
