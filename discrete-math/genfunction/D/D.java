import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class D {
	private static final class Factorial {
		private final BigInteger numerator;
		private final BigInteger denominator;
		private final static Factorial ZERO = new Factorial();
		private final static Factorial ONE = new Factorial(new BigInteger("1"), new BigInteger("1"));

		public Factorial() {
			numerator = BigInteger.ZERO;
			denominator = BigInteger.ONE;
		}

		public Factorial(final int n, final int d) {
			numerator = BigInteger.valueOf(n);
			denominator = BigInteger.valueOf(d);
		}

		public Factorial(final BigInteger n, final BigInteger d) {
			numerator = n;
			denominator = d;
		}

		@Override
		public String toString() {
			final BigInteger[] a = mini();
			return String.format("%s/%s", a[0], a[1]);
		}

		private BigInteger[] mini() {
			final BigInteger g = numerator.gcd(denominator);
			final BigInteger n = numerator.divide(g);
			final BigInteger d = denominator.divide(g);
			return new BigInteger[]{n, d};
		}

		public Factorial add(final Factorial other) {
			final BigInteger[] l = mini();
			final BigInteger[] r = other.mini();
			final BigInteger[] res = new BigInteger[2];
			res[0] = l[0].multiply(r[1]).add(r[0].multiply(l[1]));
			res[1] = l[1].multiply(r[1]);
			return new Factorial(res[0], res[1]);
		}

		public Factorial mul(final Factorial other) {
			final BigInteger[] l = mini();
			final BigInteger[] r = other.mini();
			final BigInteger[] res = new BigInteger[2];
			res[0] = l[0].multiply(r[0]);
			res[1] = l[1].multiply(r[1]);
			return new Factorial(res[0], res[1]);
		}
	}

	private static final class GeneratingFunction {
		private final List<Factorial> values;

		public GeneratingFunction() {
			this.values = new ArrayList<>();
		}

		public GeneratingFunction(final int size) {
			this.values = new ArrayList<>(Collections.nCopies(size, new Factorial()));
		}

		public static GeneratingFunction multiply(final GeneratingFunction lhs, final Factorial rhs) {
			final GeneratingFunction mul = new GeneratingFunction();
			for (int i = 0; i < lhs.size(); i++) {
				mul.add(lhs.get(i).mul(rhs));
			}
			return mul;
		}

		public static GeneratingFunction multiply(final GeneratingFunction lhs, final GeneratingFunction rhs) {
			final GeneratingFunction mul = new GeneratingFunction(lhs.size() + rhs.size() - 1);
			for (int i = 0; i < lhs.size(); i++) {
				for (int j = 0; j < rhs.size() && i + j < mul.size(); j++) {
					mul.set(i + j, mul.get(i + j).add(lhs.get(i).mul(rhs.get(j))));
				}
			}
			return mul;
		}

		public static GeneratingFunction sum(final GeneratingFunction lhs, final GeneratingFunction rhs) {
			final GeneratingFunction sum = new GeneratingFunction();
			for (int i = 0; i < Math.max(lhs.size(), rhs.size()); i++) {
				sum.add(lhs.get(i).add(rhs.get(i)));
			}
			return sum;
		}

		public int size() {
			return values.size();
		}

		public void set(final int i, final Factorial f) {
			values.set(i, f);
		}

		public Factorial get(final int i) {
			return i < size() ? values.get(i) : Factorial.ZERO;
		}

		public void add(final Factorial e) {
			values.add(e);
		}

		@Override
		public String toString() {
			final StringBuilder sb = new StringBuilder();
			for (final Factorial f : values) {
				sb.append(f.toString());
				sb.append(" ");
			}
			return sb.toString();
		}
	}

	private static BigInteger factorial(final int k) {
		final BigInteger kk = BigInteger.valueOf(k);
		BigInteger t = BigInteger.ONE;
		for (BigInteger i = BigInteger.ONE; i.compareTo(kk) <= 0; i = i.add(BigInteger.ONE)) {
			t = t.multiply(i);
		}
		return t;
	}

	public static void main(final String[] args) {
		final Scanner in = new Scanner(System.in);
		final int r = in.nextInt(), k = in.nextInt();
		final int[] pkt = new int[k + 1];
		for (int i = 0; i < k + 1; i++) {
			pkt[i] = in.nextInt();
		}
		in.close();
		GeneratingFunction result = new GeneratingFunction();
		for (int i = 0; i < k + 1; i++) {
			GeneratingFunction temp = new GeneratingFunction();
			temp.add(new Factorial(-i + 1, 1));
			temp.add(Factorial.ONE);
			for (int j = 2; j <= k; j++) {
				final GeneratingFunction t = new GeneratingFunction();
				t.add(new Factorial(-i + j, 1));
				t.add(Factorial.ONE);
				temp = GeneratingFunction.multiply(temp, t);
			}
			final BigInteger num = BigInteger.valueOf(pkt[i]);
			final BigInteger den = factorial(k).multiply(BigInteger.valueOf(r).pow(i));
			result = GeneratingFunction.sum(result, GeneratingFunction.multiply(temp, new Factorial(num, den)));
		}
		System.out.println(result);
	}
}
