package md2html;

import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class FileOutputHelper {
	private BufferedWriter output;
	private boolean closed = true;

	public FileOutputHelper(final String o) {
		try {
			this.output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(o), StandardCharsets.UTF_8));
			this.closed = false;
		} catch (final IOException err) {
			System.out.println("Can't write to file: " + err.getMessage());
		}
	}

	public void close() {
		if (this.closed) {
			return;
		} else {
			this.closed = true;
			try {
				this.output.close();
			} catch (final IOException err) {
				System.out.println("Scanner cannot be closed: " + err.getMessage());
			}
		}
	}

	public void write(final List<String> list) {
		if (this.closed) {
			throw new IllegalStateException("Scanner is closed!");
		}

		try {
			for (final String s : list) {
				this.output.write(s);
				this.output.write(System.lineSeparator().charAt(0));
			}
		} catch (final IOException e) {
			System.out.println("Scanner can't write: " + e.getMessage());
		}
	}
}
