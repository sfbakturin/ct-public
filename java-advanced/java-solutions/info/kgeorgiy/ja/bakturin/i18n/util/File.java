package info.kgeorgiy.ja.bakturin.i18n.util;

import info.kgeorgiy.ja.bakturin.i18n.TextStatistics;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class File {
	public static String readText(final Path fileInput) throws IOException {
		return Files.readString(fileInput, TextStatistics.SC);
	}

	public static void writeText(final String text, final Path fileOutput) throws IOException {
		try (final BufferedWriter writer = Files.newBufferedWriter(fileOutput, TextStatistics.SC)) {
			writer.write(text);
		}
	}
}
