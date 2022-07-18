import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class Task01 {
    private static void gen(final String p, final byte n) {
        if (p.length() == n) {
            System.out.println(p);
            return;
        }

        gen(p + '0', n);
        gen(p + '1', n);
    }

    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final byte n = in.nextByte();
        in.close();
        gen("", n);
    }
}
