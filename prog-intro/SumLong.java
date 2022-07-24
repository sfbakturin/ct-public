
/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class SumLong {
	public static void main(final String... args) {
		long sum = 0;
		for (final String arg : args) {
			final StringBuilder s = new StringBuilder();
			for (final char ch : arg.toCharArray()) {
				if (Character.isWhitespace(ch)) {
					if (s.length() != 0) {
						sum += Long.parseLong(s.toString());
					}
					s.setLength(0);
				} else {
					s.append(ch);
				}
			}
			if (s.length() != 0) {
				sum += Long.parseLong(s.toString());
			}
		}
		System.out.println(sum);
	}
}
