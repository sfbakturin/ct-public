import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Task08 {
	private static void gen(final List<Byte> p, final byte n, final int k) {
		if (p.size() == k) {
			for (int i = 0; i < k; i++) {
				System.out.print(p.get(i) + " ");
			}
			System.out.println();
			return;
		}
		if (p.size() != 0) {
			for (int i = p.get(p.size() - 1) + 1; i <= n; i++) {
				if (n - i < k - p.size() - 1) {
					return;
				} else {
					final List<Byte> o = new ArrayList<>(p);
					o.add((byte) i);
					gen(o, n, k);
				}
			}
		} else {
			for (int i = 1; i <= n; i++) {
				final List<Byte> o = new ArrayList<>(p);
				o.add((byte) i);
				gen(o, n, k);
			}
		}
	}

	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final byte n = in.nextByte(), k = in.nextByte();
		in.close();
		final List<Byte> p = new ArrayList<>();
		gen(p, n, k);
	}
}
