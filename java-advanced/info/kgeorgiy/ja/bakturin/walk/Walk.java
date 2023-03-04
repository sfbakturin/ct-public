package info.kgeorgiy.ja.bakturin.walk;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.NoSuchAlgorithmException;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Walk {
	private static final String ERROR = "0".repeat(64);
	private static final int BUFFER_SIZE = 1024;
	private static final byte[] BUFFER = new byte[BUFFER_SIZE];

	public static void main(final String... args) {
		if (args == null) {
			System.err.println("ERROR: arguments array is null.");
			return;
		}

		if (args.length != 2) {
			System.err.println("ERROR: usage of class is <class> <input_list> <output_list>");
			return;
		}

		if (args[0] == null || args[1] == null) {
			System.err.println("ERROR: arguments array is invalid");
			return;
		}

		try {
			final Path input = Paths.get(args[0]);
			try {
				final Path output = Paths.get(args[1]);
				if (createDirectories(args, output)) {
					return;
				}
				final HashCalculator hash = new HashCalculator();
				try (final BufferedReader listReader = Files.newBufferedReader(input, StandardCharsets.UTF_8)) {
					try (final BufferedWriter listWriter = Files.newBufferedWriter(output, StandardCharsets.UTF_8)) {
						String name;
						while ((name = listReader.readLine()) != null) {
							try {
								new FileAuditor(name, hash, BUFFER).visit();
								listWriter.write(hash + " " + name);
							} catch (final IOException | InvalidPathException | SecurityException e) {
								listWriter.write(ERROR + " " + name);
							} finally {
								listWriter.newLine();
							}
						}
					}
				} catch (final IOException e) {
					System.err.println("ERROR: cannot open file of input or output file path (" + args[0] + " " + args[1] + "), I/O error.\nEXCEPTION: " + e.getMessage());
				} catch (final SecurityException e) {
					System.err.println("ERROR: cannot open file of input path(" + args[1] + "), security permission error.\nEXCEPTION: " + e.getMessage());
				}
			} catch (final InvalidPathException e) {
				System.err.println("ERROR: cannot resolve path(" + args[1] + ") of output file.\nEXCEPTION: " + e.getMessage());
			} catch (final NoSuchAlgorithmException e) {
				System.err.println("ERROR: no such algorithm SHA-256 was found.\nEXCEPTION ERROR: " + e.getMessage());
			}
		} catch (final InvalidPathException e) {
			System.err.println("ERROR: cannot resolve path(" + args[0] + ") of input file.\nEXCEPTION: " + e.getMessage());
		}
	}

	private static boolean createDirectories(final String[] args, final Path output) {
		try {
			if (output.getParent() != null) {
				Files.createDirectories(output.getParent());
			}
		} catch (final FileAlreadyExistsException ignored) {
		} catch (final IOException e) {
			System.err.println("ERROR: cannot create directories for path(" + args[1] + "), I/O error.\nEXCEPTION: " + e.getMessage());
			return true;
		} catch (final SecurityException e) {
			System.err.println("ERROR: cannot create directories for path(" + args[1] + "), security permission error.\nEXCEPTION: " + e.getMessage());
			return true;
		}
		return false;
	}
}
