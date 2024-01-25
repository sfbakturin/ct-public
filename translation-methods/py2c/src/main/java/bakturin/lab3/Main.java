package bakturin.lab3;

import bakturin.lab3.py2c.Py2C;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Main {
	public static void main(final String[] args) throws IOException {
		if (Objects.isNull(args) || args.length != 2 || Arrays.stream(args).anyMatch(Objects::isNull)) {
			System.err.println("Usage: program.exe main.py");
			return;
		}

		final Path pIn = Paths.get(args[0]);
		final Path pOut = Paths.get(args[1]);
		final List<String> lines = Files.readAllLines(pIn);

		final Py2C.Py2CResult result = Py2C.translate(String.join(System.lineSeparator(), lines));

		if (result.isFailed()) {
			System.err.println(result.errorMessage());
		} else {
			System.out.printf("/* START OF '%s' */%n", pOut.getFileName());
			System.out.println(result.code);
			System.out.printf("/* END OF '%s'; */%n", pOut.getFileName());
			Files.writeString(pOut, result.code);
		}
	}
}
