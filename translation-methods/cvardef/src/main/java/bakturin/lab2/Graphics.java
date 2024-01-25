package bakturin.lab2;

import bakturin.lab2.cvardef.CVarDef;
import bakturin.lab2.cvardef.assets.CVarDefTree;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Graphics {
	public static void main(final String[] args) throws IOException {
		if (Objects.isNull(args) || args.length != 2 ||
				Arrays.stream(args).anyMatch(Objects::isNull)) {
			System.err.println("Usage: <program.exe> <expression>");
		}
		final CVarDefTree parsed = CVarDef.getParsingTree(true, args[0]);
		Files.writeString(Paths.get(args[1]), parsed.getDot(), StandardCharsets.UTF_8);
	}
}
