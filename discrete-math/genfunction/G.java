import java.math.BigInteger;
import java.util.Arrays;
import java.util.Scanner;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class G {
	private static BigInteger factorial(final BigInteger i) {
		BigInteger f = BigInteger.ONE;
		for (BigInteger j = BigInteger.ONE; j.compareTo(i) <= 0; j = j.add(BigInteger.ONE)) {
			f = f.multiply(j);
		}
		return f;
	}

	private static BigInteger cnk(final BigInteger n, final BigInteger k) {
		if (k.compareTo(n) > 0) {
			return BigInteger.ZERO;
		}
		if (n.compareTo(k) == 0) {
			return BigInteger.ONE;
		}
		final BigInteger start = n.subtract(k).add(BigInteger.ONE);
		BigInteger num = BigInteger.ONE;
		for (BigInteger i = start; i.compareTo(n) <= 0; i = i.add(BigInteger.ONE)) {
			num = num.multiply(i);
		}
		final BigInteger den = factorial(k);
		return num.divide(den);
	}

	private static BigInteger max(final BigInteger l) {
		return (l.compareTo(BigInteger.ZERO) <= 0 ? BigInteger.ZERO : l);
	}

	public static void main(final String[] args) {
		final Scanner in = new Scanner(System.in);
		final String expression = in.nextLine();
		in.close();
		final Expression parsed = new ExpressionParser(expression).parse();
		System.out.println(Arrays.stream(parsed.evaluate()).map(BigInteger::toString).collect(Collectors.joining(" ")));
	}

	private interface Expression {
		BigInteger[] evaluate();
	}

	private final static class Atom implements Expression {
		private final static BigInteger[] B = new BigInteger[]{BigInteger.ZERO, BigInteger.ONE, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO};

		public Atom() {
		}

		@Override
		public BigInteger[] evaluate() {
			return B;
		}
	}

	private final static class Seq implements Expression {
		private final Expression value;

		public Seq(final Expression value) {
			this.value = value;
		}

		@Override
		public BigInteger[] evaluate() {
			final BigInteger[] evaluated = value.evaluate();
			final BigInteger[] result = new BigInteger[7];
			Arrays.fill(result, BigInteger.ZERO);
			result[0] = BigInteger.ONE;
			for (int n = 1; n < 7; n++) {
				for (int i = 1; i <= n; i++) {
					result[n] = result[n].add(evaluated[i].multiply(result[n - i]));
				}
			}
			return result;
		}
	}

	private final static class MSet implements Expression {
		private final Expression value;

		public MSet(final Expression value) {
			this.value = value;
		}

		@Override
		public BigInteger[] evaluate() {
			final BigInteger[] evaluated = value.evaluate();
			final BigInteger[] result = new BigInteger[7];
			Arrays.fill(result, BigInteger.ZERO);
			final BigInteger[][] mnk = {
					{BigInteger.ONE, BigInteger.ONE, BigInteger.ONE, BigInteger.ONE, BigInteger.ONE, BigInteger.ONE, BigInteger.ONE},
					{BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO},
					{BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO},
					{BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO},
					{BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO},
					{BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO},
					{BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO}
			};
			for (int n = 1; n < 7; n++) {
				for (int k = 1; k < 7; k++) {
					BigInteger sum = BigInteger.ZERO;
					for (int i = 0; i <= n / k; i++) {
						sum = sum.add(cnk((max(evaluated[k].add(BigInteger.valueOf(i)).subtract(BigInteger.ONE))), BigInteger.valueOf(i)).multiply(mnk[n - i * k][k - 1]));
					}
					mnk[n][k] = sum;
				}
				result[n] = mnk[n][n];
			}
			result[0] = mnk[0][0];
			return result;
		}
	}

	private final static class Pair implements Expression {
		private final Expression left, right;

		public Pair(final Expression left, final Expression right) {
			this.left = left;
			this.right = right;
		}

		@Override
		public BigInteger[] evaluate() {
			final BigInteger[] evaluatedLeft = left.evaluate();
			final BigInteger[] evaluatedRight = right.evaluate();
			final BigInteger[] result = new BigInteger[7];
			Arrays.fill(result, BigInteger.ZERO);
			for (int n = 0; n < 7; n++) {
				for (int i = 0; i <= n; i++) {
					result[n] = result[n].add(evaluatedLeft[i].multiply(evaluatedRight[n - i]));
				}
			}
			return result;
		}
	}

	private final static class ExpressionParser {
		private final static class CharIterator {
			private final String source;
			private int pos;

			public CharIterator(final String s) {
				this.source = s;
				pos = 0;
			}

			public char next() {
				return source.charAt(pos++);
			}

			public void skip() {
				pos++;
			}
		}

		private final CharIterator iterator;

		public ExpressionParser(final String s) {
			iterator = new CharIterator(s);
		}

		public Expression parse() {
			Expression result;
			final char c = iterator.next();
			if (c == 'P') {
				iterator.skip();
				final Expression left = parse();
				iterator.skip();
				final Expression right = parse();
				iterator.skip();
				result = new Pair(left, right);
			} else if (c == 'B') {
				result = new Atom();
			} else if (c == 'L') {
				iterator.skip();
				result = new Seq(parse());
				iterator.skip();
			} else {
				iterator.skip();
				result = new MSet(parse());
				iterator.skip();
			}
			return result;
		}
	}
}
