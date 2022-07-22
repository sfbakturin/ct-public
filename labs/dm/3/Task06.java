import java.util.LinkedHashSet;
import java.util.Scanner;
import java.util.Set;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class Task06 {
    private static void gen(final String p, final byte n, final Set<String> strings) {
        if (p.length() == n) {
            strings.add(p);
            return;
        }
        if (p.length() == 0) {
            gen(p + '0', n, strings);
            gen(p + '1', n, strings);
        } else {
            gen(p + '0', n, strings);
            if (p.charAt(p.length() - 1) == '1') {
                gen(p + '0', n, strings);
            } else {
                gen(p + '1', n, strings);
            }
        }
    }

    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final byte n = in .nextByte();
        in.close();
        final Set<String> strings = new LinkedHashSet<>();
        gen("", n, strings);
        System.out.println(strings.size());
        for (String s : strings) {
            System.out.println(s);
        }
    }
}
