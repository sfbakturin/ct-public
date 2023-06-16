import java.util.*;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Main {
	public static void main(final String[] args) {
		final Scanner in = new Scanner(System.in);
		final Expression parsed = new ExpressionParser(in.nextLine()).parse();
		in.close();
		System.out.println(parsed.disassembly());
	}
}
