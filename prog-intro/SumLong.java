
/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class SumLong {
    public static void main(final String... args) {
        long sum = 0;
        for (final String arg : args) {
            final StringBuilder s = new StringBuilder();
            for (int j = 0; j < arg.length(); j++) {
                if (Character.isWhitespace(arg.charAt(j))) {
                    if (s.length() != 0) {
                        sum += Long.parseLong(s.toString());
                    }
                    s.setLength(0);
                } else {
                    s.append(arg.charAt(j));
                }
            }
            if (s.length() != 0) {
                sum += Long.parseLong(s.toString());
            }
        }
        System.out.println(sum);
    }
}
