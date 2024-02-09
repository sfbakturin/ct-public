import org.junit.Test;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class GeneratorTests extends GeneratorTester {
	public GeneratorTests() {
	}

	@Test
	public void testCalculator() {
		final String grammar = "FibCalculator";
		final String field = "evaluated";
		final String start = "expr";
		generateClasses(grammar);
		except(grammar, field, "1", String.valueOf(1.0), start);
		except(grammar, field, "89 + 21", String.valueOf(89.0 + 21.0), start);
		except(grammar, field, "2 * 56", String.valueOf(2 * 56.0), start);
		except(grammar, field, "7 - 8", String.valueOf(7.0 - 8.0), start);
		except(grammar, field, "(1 + 2 * 3 - 4)", String.valueOf((1.0 + 2.0 * 3.0 - 4.0)), start);
		except(grammar, field, "2 * (1 + 1)", String.valueOf(2.0 * (1 + 1)), start);
		except(grammar, field, "2 * 1 + 1", String.valueOf(2.0 * 1 + 1), start);
		except(grammar, field, "1 / 2 / 3", String.valueOf(1.0 / 2.0 / 3.0), start);
		except(grammar, field, "1 / (2 / 3)", String.valueOf(1.0 / (2.0 / 3.0)), start);
		except(grammar, field, "(1 / 2) / 3", String.valueOf((1.0 / 2.0) / 3.0), start);
		except(grammar, field, "1 - 2 + 3", String.valueOf(1.0 - 2.0 + 3.0), start);
		except(grammar, field, "1 - 2 - 3", String.valueOf(1.0 - 2.0 - 3.0), start);
		except(grammar, field, "fib(1)", String.valueOf(fib(1)), start);
		except(grammar, field, "fib(1) + fib(2) + fib(3)", String.valueOf(fib(1) + fib(2) + fib(3)), start);
		except(grammar, field, "fib(fib(fib(1)))", String.valueOf(fib((int) fib((int) fib(1)))), start);
	}

	@Test
	public void testVariables() {
		final String grammar = "Variables";
		generateClasses(grammar);
		except(grammar, "int a;");
		except(grammar, "int a, b, c, d;");
		except(grammar, "int *a;");
		except(grammar, "int ****a;");
		except(grammar, "int *a, b;");
		except(grammar, "int *a, *b;");
		except(grammar, "int a, *b;");
		except(grammar, "int a, *b, **c, **d;");
		except(grammar, "int ************a");
	}
}
