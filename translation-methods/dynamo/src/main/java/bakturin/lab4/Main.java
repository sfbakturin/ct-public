package bakturin.lab4;

import bakturin.lab4.dynamo.Generator;
import bakturin.lab4.dynamo.grammar.Result;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Main {
	public static void main(final String[] args) throws IOException {
		if (Objects.isNull(args) || args.length != 2 || Arrays.stream(args).anyMatch(Objects::isNull)) {
			System.err.println("Usage: program.exe <grammar> <path/to/output-generated>");
			return;
		}

		final Path grammar = Paths.get(args[0]);
		final Path output = Paths.get(args[1]);
		final String read = String.join(System.lineSeparator(), Files.readAllLines(grammar));

		final Result result = Generator.parseGrammar(read);
		final Map<String, String> classes = Generator.generate(result);

		try {
			Files.createDirectories(output);
		} catch (final FileAlreadyExistsException ignored) {}

		for (final Map.Entry<String, String> clazz : classes.entrySet()) {
			final String className = clazz.getKey();
			final String classImpl = clazz.getValue();
			final Path filename = output.resolve(className + ".java");
			Files.writeString(filename, classImpl);
		}
	}
}
