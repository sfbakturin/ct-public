import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.*;

import static java.lang.Math.max;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class I {
	public static void main(final String... args) {
		final FastScanner in = new FastScanner(System.in);
		final Writer out = new PrintWriter(System.out);
		final int n = Integer.parseInt(in.nextInt());
		final int[] input = new int[n];
		int maximum = Integer.MIN_VALUE;
		for (int i = 0; i < n; i++) {
			final int INPUT = Integer.parseInt(in.nextInt());
			input[i] = INPUT;
			maximum = max(INPUT, maximum);
		}
		in.close();
		final int[] mp = new int[1600 + 2];
		for (int i = 0; i <= 1600 + 1; i++) {
			mp[i] = -1;
		}
		final Set<Integer> primes = new LinkedHashSet<>();
		for (int i = 2; i <= 1600 + 1; i++) {
			if (mp[i] == -1) {
				primes.add(i);
				mp[i] = i;
			}
			for (final Integer item : primes) {
				if (item <= mp[i] && i * item <= 1600 + 1) {
					mp[i * item] = item;
				} else {
					break;
				}
			}
		}
		for (int i = 0; i < n; i++) {
			final StringBuilder sb = new StringBuilder();
			for (final Integer item : primes) {
				while (input[i] % item == 0) {
					sb.append(item).append(" ");
					input[i] /= item;
				}
				if (input[i] == 1) {
					break;
				}
			}
			if (input[i] != 1) {
				sb.append(input[i]);
			}
			System.out.println(sb);
		}
	}

	public static class FastScanner {
		private Reader scanInput;
		private boolean scanClosed;
		private char scanChar;
		private int scanLine = 1;
		private static final Set<Character> ALPHABET_ORDINARY, ALPHABET_ABC;

		static {
			ALPHABET_ORDINARY = Set.of(
					'-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
			);
			ALPHABET_ABC = Set.of(
					'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '-'
			);
		}

		public FastScanner(final InputStream inputStream) {
			if (inputStream == null) {
				throw new NullPointerException("No such input stream found!");
			}

			this.scanInput = new InputStreamReader(inputStream);
			this.scanClosed = false;
			this.scanChar = (char) 0;
		}

		public void close() {
			if (this.scanClosed) {
				return;
			} else {
				this.scanClosed = true;
				try {
					this.scanInput.close();
				} catch (final IOException e) {
					System.out.println("Scanner can't be closed: " + e.getMessage());
				} finally {
					return;
				}
			}
		}

		public String nextInt() {
			if (this.scanClosed) {
				throw new IllegalStateException("Scanner is closed!");
			}

			final StringBuilder sb = new StringBuilder();

			boolean ws = false;

			try {
				while (true) {
					this.scanChar = (char) this.scanInput.read();
					if (this.scanChar == (char) -1) {
						break;
					} else {
						if (this.reverseLegal(this.scanChar) || this.reverseAbcLegal(this.scanChar)) {
							sb.append(this.scanChar);
							ws = true;
						} else {
							if (this.scanChar == System.lineSeparator().charAt(0)) {
								this.scanLine++;
								break;
							}
							if (Character.isWhitespace(this.scanChar)) {
								if (ws) {
									break;
								}
							}
						}
					}
				}
			} catch (final IOException err) {
				System.out.println("Scanner can't read: " + err.getMessage());
			}
			if (sb.length() == 0) {
				return null;
			}
			return sb.toString();
		}

		private boolean reverseLegal(final char in) {
			if (this.scanClosed) {
				throw new IllegalStateException("Scanner is closed!");
			}

			return ALPHABET_ORDINARY.contains(in);
		}

		private boolean wsppLegal(final char in) {
			if (this.scanClosed) {
				throw new IllegalStateException("Scanner is closed!");
			}

			return (Character.isLetter(in) || in == '\'' || Character.getType(in) == Character.DASH_PUNCTUATION);
		}

		private boolean reverseAbcLegal(final char in) {
			if (this.scanClosed) {
				throw new IllegalStateException("Scanner is closed!");
			}

			return ALPHABET_ABC.contains(in);
		}
	}
}
