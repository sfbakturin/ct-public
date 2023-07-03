import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Task11 {
	private static void gen(final byte n, final List<Byte> m) {
		if (m.size() == n) {
			final StringBuilder sb = new StringBuilder();
			for (final Byte item : m) {
				sb.append(item).append(" ");
			}
			System.out.println(sb);
			return;
		}
		final StringBuilder sb = new StringBuilder();
		for (final Byte item : m) {
			sb.append(item).append(" ");
		}
		System.out.println(sb);
		for (int i = 1; i <= n; i++) {
			if (m.size() != 0) {
				if (m.get(m.size() - 1) < i) {
					final List<Byte> a = new ArrayList<>(m);
					a.add((byte) i);
					gen(n, a);
				}
			} else {
				final List<Byte> a = new ArrayList<>(m);
				a.add((byte) i);
				gen(n, a);
			}
		}
	}

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final byte n = in.nextByte();
		in.close();
		final List<Byte> m = new ArrayList<>();
		gen(n, m);
	}
}
