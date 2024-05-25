import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class WordStatInput {
	public static void main(final String... args) {
		try {
			final Reader in = new FileReader(args[0], StandardCharsets.UTF_8);
			final Map<String, Integer> list = new LinkedHashMap<>();
			final StringBuilder sb = new StringBuilder();
			try {
				while (true) {
					final int read = in.read();
					final char ch = (char) read;
					if (read == -1) {
						break;
					} else {
						if (checkChar(ch)) {
							sb.append(ch);
						} else {
							if (sb.length() != 0) {
								final String s = lowChars(sb.toString());
								if (list.containsKey(s)) {
									list.put(s, list.get(s) + 1);
								} else {
									list.put(s, 1);
								}
								sb.setLength(0);
							}
						}
					}
				}
			} finally {
				in.close();
				if (sb.length() != 0) {
					final String s = lowChars(sb.toString());
					if (list.containsKey(s)) {
						list.put(s, list.get(s) + 1);
					} else {
						list.put(s, 1);
					}
					sb.setLength(0);
				}
			}
			try (final BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8))) {
				try {
					for (final Map.Entry<String, Integer> item : list.entrySet()) {
						out.write(item.getKey() + " " + item.getValue());
						out.newLine();
					}
				} finally {
					out.close();
				}
			}
		} catch (final FileNotFoundException err) {
			System.out.println("Cannot open file: " + err.getMessage());
		} catch (final IOException err) {
			System.out.println("Cannot read file: " + err.getMessage());
		}
	}

	private static boolean checkChar(final char ch) {
		return ((ch == '\'') || (Character.getType(ch) == Character.DASH_PUNCTUATION) || (Character.isLetter(ch)));
	}

	private static String lowChars(final String s) {
		return s.toLowerCase();
	}
}
