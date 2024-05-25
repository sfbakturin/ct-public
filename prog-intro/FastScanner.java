import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.Set;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class FastScanner {
	private Reader scanInput;
	private boolean scanClosed;
	private char scanChar;
	private int scanLine = 1;
	private int scanWord = 1;
	private boolean scanNewLine = false;
	private boolean scanCriticalSituation = false;
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

	public FastScanner(final String file) {
		if (file == null) {
			throw new NullPointerException("No such input stream found!");
		}

		try {
			this.scanInput = new FileReader(file, StandardCharsets.UTF_8);
			this.scanClosed = false;
			this.scanChar = (char) 0;
		} catch (final IOException err) {
			System.out.println("No such input stream found: " + err.getMessage());
		}
	}

	public int getWord() {
		if (this.scanClosed) {
			throw new IllegalStateException("Scanner is closed!");
		}

		return this.scanWord;
	}

	public int getLine() {
		if (this.scanClosed) {
			throw new IllegalStateException("Scanner is closed!");
		}

		return this.scanLine;
	}

	public void resetWord() {
		if (this.scanClosed) {
			throw new IllegalStateException("Scanner is closed!");
		}

		this.scanWord = 1;
	}

	public void addWord() {
		if (this.scanClosed) {
			throw new IllegalStateException("Scanner is closed!");
		}

		this.scanWord++;
	}

	public void addLine() {
		if (this.scanClosed) {
			throw new IllegalStateException("Scanner is closed!");
		}

		this.scanLine++;
	}

	public void close() {
		if (this.scanClosed) {
			return;
		} else {
			this.scanClosed = true;
			try {
				this.scanInput.close();
			} catch (final IOException err) {
				System.out.println("Scanner can't be closed: " + err.getMessage());
			} finally {
				return;
			}
		}
	}

	public boolean hasNextLine() {
		if (this.scanClosed) {
			throw new IllegalStateException("Scanner is closed!");
		}

		return this.scanChar != (char) -1;
	}

	public boolean isNextLine() {
		if (this.scanClosed) {
			throw new IllegalStateException("Scanner is closed!");
		}

		if (this.scanNewLine) {
			this.scanNewLine = false;
			return false;
		} else {
			return true;
		}
	}

	public boolean isCritical() {
		if (this.scanClosed) {
			throw new IllegalStateException("Scanner is closed!");
		}

		return this.scanCriticalSituation;
	}

	public void resetCriticalSituation() {
		if (this.scanClosed) {
			throw new IllegalStateException("Scanner is closed!");
		}

		this.scanCriticalSituation = false;
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
							this.scanNewLine = true;
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

	public String nextWord() {
		if (this.scanClosed) {
			throw new IllegalStateException("Scanner is closed!");
		}

		final StringBuilder sb = new StringBuilder();

		try {
			while (true) {
				this.scanChar = (char) this.scanInput.read();
				if (this.scanChar == (char) -1) {
					break;
				} else {
					if (this.wsppLegal(this.scanChar)) {
						sb.append(this.scanChar);
					} else {
						if (this.scanChar == System.lineSeparator().charAt(0)) {
							this.scanNewLine = true;
						}
						break;
					}
				}
			}
		} catch (final IOException err) {
			System.out.println("Scanner can't read: " + err.getMessage());
		}

		if (this.scanNewLine && sb.length() != 0) {
			this.scanNewLine = false;
			this.scanCriticalSituation = true;
			return sb.toString().toLowerCase();
		}

		if (this.scanNewLine) {
			this.scanNewLine = false;
			return null;
		} else {
			return sb.toString().toLowerCase();
		}
	}

	public String nextIntAbc2() {
		if (this.scanClosed) {
			throw new IllegalStateException("Scanner is closed!");
		}

		final String input = this.nextInt();

		if (input == null) {
			return null;
		} else {
			int answer = 0;
			if (input.charAt(0) == '-') {
				for (int i = 1; i < input.length(); i++) {
					final int c = (input.charAt(i) - 97);
					answer = answer * 10 + c;
				}
				answer = answer * (-1);
			} else {
				for (int i = 0; i < input.length(); i++) {
					final int c = (input.charAt(i) - 97);
					answer = answer * 10 + c;
				}
			}
			return String.valueOf(answer);
		}
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
