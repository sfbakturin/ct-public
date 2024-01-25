package bakturin.lab2;

import java.util.List;
import java.util.Objects;
import java.util.Scanner;

import bakturin.lab2.cvardef.CVarDef;
import bakturin.lab2.cvardef.CVarDefVariable;
import bakturin.lab2.cvardef.assets.CVarDefTree;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Main {
	public static void main(final String[] args) {
		final Scanner in = new Scanner(System.in);

		do {
			System.out.print("expr> ");
			final String user = in.nextLine();

			if (user.equalsIgnoreCase("quit") || user.equalsIgnoreCase("exit")) {
				break;
			}

			final List<CVarDefVariable> parsed = CVarDef.parseNull(user);
			final CVarDefTree t = CVarDef.getParsingTree(false, user);

			if (Objects.isNull(parsed)) {
				System.out.println("Error while parsing.");
			} else {
				parsed.forEach(s -> System.out.println(s.toString()));
			}
		} while (true);

		in.close();
	}
}
