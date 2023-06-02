package info.kgeorgiy.ja.bakturin.i18n.util;

import info.kgeorgiy.ja.bakturin.i18n.TextStatistics;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.IllformedLocaleException;
import java.util.Locale;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class Init {
	public static InitResult parse(final String[] args) throws IOException {
		if (Objects.isNull(args)) {
			throw new NullPointerException("Arguments cannot be null object.");
		}

		if (args.length != 4) {
			throw new IllegalArgumentException("Argument's length must be 4.");
		}

		if (Arrays.stream(args).anyMatch(Objects::isNull)) {
			throw new IllegalArgumentException("Some of arguments is invalid.");
		}

		final Locale localeInput = Locale.forLanguageTag(args[0]);
		if (!TextStatistics.LOCALES_INPUT.contains(localeInput)) {
			throw new IllformedLocaleException("No such input locale was found in OS.");
		}

		final Locale localeOutput = Locale.forLanguageTag(args[1]);
		if (!TextStatistics.LOCALES_OUTPUT.contains(localeOutput)) {
			throw new IllformedLocaleException("No such output locale can be handled.");
		}

		final Path inputFile = Paths.get(args[2]);
		final Path outputFile = Paths.get(args[3]);
		try {
			if (Objects.nonNull(outputFile.getParent())) {
				Files.createDirectories(outputFile.getParent());
			}
		} catch (final FileAlreadyExistsException ignored) {
		}
		return new InitResult(
				localeInput,
				localeOutput,
				inputFile,
				outputFile
		);
	}

	public final static class InitResult {
		private final Pair<Locale, Locale> localeIO;
		private final Pair<Path, Path> pathIO;

		public InitResult(
				final Locale localeInput,
				final Locale localeOutput,
				final Path pathInput,
				final Path pathOutput
		) {
			localeIO = new Pair<>(localeInput, localeOutput);
			pathIO = new Pair<>(pathInput, pathOutput);
		}

		public Locale getLocaleInput() {
			return localeIO.left();
		}

		public Locale getLocaleOutput() {
			return localeIO.right();
		}

		public Path getPathInput() {
			return pathIO.left();
		}

		public Path getPathOutput() {
			return pathIO.right();
		}
	}
}
