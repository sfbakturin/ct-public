package info.kgeorgiy.ja.bakturin.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Implementor implements Impler {

	@Override
	public void implement(final Class<?> token, final Path root) throws ImplerException {
		if (!token.isInterface()) {
			throw new ImplerException("Class<?> token should be interface.");
		}

		if (Modifier.isPrivate(token.getModifiers())) {
			throw new ImplerException("Interface token should be not private.");
		}

		final String packageName = token.getPackage().getName();
		final String pureClassName = token.getSimpleName() + "Impl";
		final String className = (packageName.length() == 0 ? pureClassName : packageName + "." + pureClassName);
		final String directoryName = root.toString() + Arrays.stream(className.split("\\.")).map(s -> File.separator + s).collect(Collectors.joining());
		final String fileName = directoryName + ".java";

		try {
			final Path directoryPath = Paths.get(directoryName).getParent();
			final Path classPath = Paths.get(fileName);

			try {
				if (directoryPath != null) {
					Files.createDirectories(directoryPath);
				}
			} catch (final IOException ignored) {
			}

			try (final BufferedWriter writer = Files.newBufferedWriter(classPath, StandardCharsets.UTF_8)) {
				final ClassCreator creator = new ClassCreator(token);
				creator.genClass();
				writer.write(creator.toString());
			} catch (final IOException e) {
				throw new ImplerException(e.getMessage());
			}
		} catch (final InvalidPathException e) {
			throw new ImplerException(e.getMessage());
		}
	}

	public static void main(final String[] args) throws ImplerException {
		if (Objects.isNull(args)) {
			System.err.println("Arguments array is null.");
			return;
		}

		if (args.length != 2) {
			System.err.println("Usage of class is <class> <interface> <path>");
			return;
		}

		if (args[0] == null || args[1] == null) {
			System.err.println("Arguments array is invalid");
			return;
		}

		try {
			new Implementor().implement(Class.forName(args[0]), Paths.get(args[1]));
		} catch (final InvalidPathException e) {
			System.err.println("Invalid path: " + e.getMessage());
		} catch (final ClassNotFoundException e) {
			System.err.println("Interface to implement is not found: " + e.getMessage());
		}
	}
}
