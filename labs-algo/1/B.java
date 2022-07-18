import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class B {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt();
        in.nextLine();
        final int[] a = new int[101];
        for (int i = 0; i < n; i++) {
            a[in.nextInt()]++;
        }
        for (int i = 0; i < 101; i++) {
            while (a[i] != 0) {
                System.out.print(i + " ");
                a[i]--;
            }
        }
        in.close();
    }
}
