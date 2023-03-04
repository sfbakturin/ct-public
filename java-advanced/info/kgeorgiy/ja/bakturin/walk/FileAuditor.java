package info.kgeorgiy.ja.bakturin.walk;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Paths;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class FileAuditor {
	private final HashCalculator hash;
	private final InputStream reader;
	private final byte[] bytes;

	public FileAuditor(final String p, final HashCalculator h, final byte[] bs) throws IOException, InvalidPathException, SecurityException {
		reader = Files.newInputStream(Paths.get(p));
		hash = h;
		bytes = bs;
	}

	public void visit() throws IOException {
		int read;
		while ((read = reader.read(bytes)) >= 0) {
			hash.update(bytes, read);
		}
		reader.close();
	}
}
