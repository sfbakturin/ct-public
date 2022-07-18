import java.util.Scanner;

import static java.lang.Math.abs;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class Task25 {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int N = in.nextInt(), K = in.nextInt();
        boolean impossible = true;
        in.nextLine();
        final int[] a = new int[K];
        for (int i = 0; i < K; i++) {
            a[i] = in.nextInt();
        }
        in.close();
        final int tail = N + 1;
        for (int i = K - 1; i >= 0; i--) {
            if (i == K - 1) {
                if (abs(tail - a[i]) >= 2) {
                    a[i]++;
                    for (int j = i + 1; j < K; j++) {
                        a[j] = a[j - 1] + 1;
                    }
                    impossible = false;
                    break;
                }
            } else {
                if (abs(a[i] - a[i + 1]) >= 2) {
                    a[i]++;
                    for (int j = i + 1; j < K; j++) {
                        a[j] = a[j - 1] + 1;
                    }
                    impossible = false;
                    break;
                }
            }
        }
        if (impossible) {
            System.out.println(-1);
        } else {
            for (int i = 0; i < K; i++) {
                System.out.print(a[i] + " ");
            }
        }
    }
}
