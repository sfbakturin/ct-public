package info.kgeorgiy.ja.bakturin.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Objects;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.zip.ZipEntry;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

/**
 * The main {@code Class<?>} for passing {@code Interface} tests.
 */
public class Implementor implements Impler, JarImpler {
	/**
	 * A {@code static} method that returns the actual pure name of the {@code Class<?>}.
	 * The real pure name is a sequence of strings that is the full {@code package} name and, as the last element, the class name without unnecessary nesting. The last element is simple name from {@code Class<?>}.
	 *
	 * @param token {@code Class<?>} from which we want the pure real name.
	 * @return {@code String[]} - an array whose elements are strings in the correct order for the real pure name of the {@code Class<?>}.
	 * @see Class#getPackage()
	 * @see Package#getName()
	 * @see Class#getSimpleName()
	 */
	private static String[] getFullPureName(final Class<?> token) {
		final String packageName = token.getPackage().getName();
		final String pureClassName = token.getSimpleName() + "Impl";
		final String className = (packageName.length() == 0 ? pureClassName : packageName + "." + pureClassName);
		return className.split("\\.");
	}

	/**
	 * A {@code static} method that is used to get the relative path to a {@code Class<?>} using its pure real name.
	 *
	 * @param token {@code Class<?>} - from which we want the pure real name and generate file system path.
	 * @param root  {@code String} - in the event that the {@code Class<?>} lies in some directory, then the root directory should be submitted.
	 * @return {@code String} - received path to the file as a {@code String}.
	 * @see Implementor#getFullPureName(Class)
	 */
	private static String getFileName(final Class<?> token, final String root) {
		final String name = String.join(File.separator, getFullPureName(token));
		if (root.length() == 0) {
			return name;
		} else {
			return root + File.separator + name;
		}
	}

	/**
	 * A {@code static} method that is used to get the relative path to a {@code Class<?>} using its pure real name and java-extension.
	 *
	 * @param token {@code Class<?>} - from which we want the pure real name and generate file system path with java-extension.
	 * @param root  {@code String} in the event that the {@code Class<?>} lies in some directory, then the root directory should be submitted.
	 * @return {@code String} - received path to the file as a {@code String}.
	 * @see Implementor#getFileName(Class, String)
	 */
	private static String getFileNameJava(final Class<?> token, final String root) {
		return getFileName(token, root) + ".java";
	}

	/**
	 * A {@code static} method that is used to get the relative path to a {@code Class<?>} using its pure real name and class-extension.
	 *
	 * @param token {@code Class<?>} from which we want the pure real name and generate file system path with class-extension.
	 * @param root  {@code String} in the event that the {@code Class<?>} lies in some directory, then the root directory should be submitted.
	 * @return {@code String} received path to the file as a {@code String}.
	 * @see Implementor#getFileName(Class, String)
	 */
	private static String getFileNameClass(final Class<?> token, final String root) {
		return getFileName(token, root) + ".class";
	}

	/**
	 * A {@code static} method that is a wrapper over {@code Implementor#getFileNameJava(Class, String)}, which takes a {@code Path} instead of a {@code String}.
	 *
	 * @param token {@code Class<?>} from which we want the pure real name and generate file system path with java-extension.
	 * @param root  {@code Path} in the event that the {@code Class<?>} lies in some directory, then the root directory should be submitted.
	 * @return {@code String} received path to the file as a {@code String}.
	 * @see Implementor#getFileNameJava(Class, String)
	 */
	private static String getFileNameJava(final Class<?> token, final Path root) {
		return getFileNameJava(token, root.toString());
	}

	/**
	 * A {@code static} method that is a wrapper over {@code Implementor#getFileNameClass(Class, String)}, which takes a {@code Path} instead of a {@code String}.
	 *
	 * @param token {@code Class<?>} from which we want the pure real name and generate file system path with class-extension.
	 * @param root  {@code Path} in the event that the {@code Class<?>} lies in some directory, then the root directory should be submitted.
	 * @return {@code String} received path to the file as a {@code String}.
	 * @see Implementor#getFileNameJava(Class, String)
	 */
	private static String getFileNameClass(final Class<?> token, final Path root) {
		return getFileNameClass(token, root.toString());
	}

	/**
	 * A {@code static} method for compiling files.
	 *
	 * @param token {@code Class<?>} is used to get the absolute path to the compiled file.
	 * @param root  {@code Path} is used to get the root folder before the folder where the file is compiled.
	 * @param file  {@code String} the full path to the Java source code to be compiled.
	 * @throws ImplerException used when no compiler is found or the compilation arguments are invalid.
	 */
	private static void compileFile(final Class<?> token, final Path root, final String file) throws ImplerException {
		final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		if (Objects.isNull(compiler)) {
			throw new ImplerException("Implementing failed due to java compiler not found.");
		}
		final String classpath = root + File.pathSeparator + getClassPath(token);
		final String[] args = new String[]{file, "-cp", classpath};
		try {
			compiler.run(null, null, null, args);
		} catch (final NullPointerException e) {
			throw new ImplerException("Implementing failed due to null as arguments for compiling.", e);
		}
	}

	/**
	 * A {@code static} method that returns the full absolute path to the given {@code Class<?>}.
	 *
	 * @param token {@code Class<?>} used as the main argument to get an absolute path.
	 * @return {@code String} the full absolute path to the {@code Class<?>}.
	 * @throws ImplerException in case of an {@code URISyntaxException}.
	 */
	private static String getClassPath(final Class<?> token) throws ImplerException {
		try {
			return Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
		} catch (final URISyntaxException e) {
			throw new ImplerException("Implementing failed due to URI syntax error.", e);
		}
	}

	/**
	 * A {@code static} method for generating the base manifest of a Jar file.
	 *
	 * @return {@code Manifest} generated base manifest.
	 */
	private static Manifest getManifest() {
		final Manifest man = new Manifest();
		man.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
		return man;
	}

	@Override
	public void implement(final Class<?> token, final Path root) throws ImplerException {
		if (!token.isInterface()) {
			throw new ImplerException("Implementing failed due to class<?> token should be interface.");
		}

		if (Modifier.isPrivate(token.getModifiers())) {
			throw new ImplerException("Implementing failed due to interface token should be not private.");
		}

		final String fileName = getFileNameJava(token, root);

		try {
			final Path classPath = Paths.get(fileName);
			final Path directoryPath = classPath.getParent();

			try {
				if (directoryPath != null) {
					Files.createDirectories(directoryPath);
				}
			} catch (final IOException ignored) {
				System.err.println("Failed to create directories.");
			}

			try (final BufferedWriter writer = Files.newBufferedWriter(classPath, StandardCharsets.UTF_8)) {
				final ClassCreator creator = new ClassCreator(token);
				creator.genClass();
				writer.write(creator.toString());
			} catch (final IOException e) {
				throw new ImplerException("Implementing failed due to writer error.", e);
			}
		} catch (final InvalidPathException e) {
			throw new ImplerException("Implementing failed due to invalid path.", e);
		}
	}

	@Override
	public void implementJar(final Class<?> token, final Path jarFile) throws ImplerException {
		final Path root = jarFile.getParent();
		implement(token, root);
		final String fileJarName = getFileNameClass(token, "");
		final String fileName = getFileNameClass(token, root);
		compileFile(token, root, getFileNameJava(token, root));
		try (final OutputStream os = Files.newOutputStream(jarFile)) {
			try (final JarOutputStream jos = new JarOutputStream(os, getManifest())) {
				final ZipEntry ze = new ZipEntry(fileJarName.replace(File.separatorChar, '/'));
				jos.putNextEntry(ze);
				Files.copy(Path.of(fileName), jos);
				jos.closeEntry();
			} catch (final NullPointerException | IllegalArgumentException | IOException | SecurityException e) {
				throw new ImplerException("Implementing failed due to broken file.", e);
			}
		} catch (final IOException e) {
			throw new ImplerException("Implementing failed due to writer error.", e);
		}
	}

	/**
	 * Main {@code Method}.
	 *
	 * @param args {@code String[]} arguments of the entry point of the main part of the program.
	 */
	public static void main(final String[] args) {
		if (Objects.isNull(args)) {
			System.err.println("Arguments array is null.");
			return;
		}

		if (Arrays.stream(args).anyMatch(Objects::isNull)) {
			System.err.println("Arguments array is invalid");
			return;
		}

		try {
			if (args.length == 2) {
				new Implementor().implement(Class.forName(args[0]), Paths.get(args[1]));
			} else if (args.length == 3 && args[0].equals("-jar") && args[2].endsWith(".jar")) {
				new Implementor().implementJar(Class.forName(args[1]), Paths.get(args[2]));
			} else {
				System.err.println("Invalid number of arguments or invalid arguments");
			}
		} catch (final ClassNotFoundException e) {
			System.err.println("Class not found: " + e.getMessage());
		} catch (final InvalidPathException e) {
			System.err.println("Invalid path was given: " + e.getMessage());
		} catch (final ImplerException e) {
			System.err.println("Failed: " + e.getMessage());
		}
	}
}
