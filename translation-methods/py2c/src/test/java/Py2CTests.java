import bakturin.lab3.py2c.exceptions.Py2CIncompatibleTypesException;
import bakturin.lab3.py2c.exceptions.Py2CMultipleTypeDefinitionException;
import bakturin.lab3.py2c.exceptions.Py2CUnsupportedBinaryExpressionOperationException;
import org.junit.Test;

import java.util.Collections;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CTests extends Py2CTester {
	public Py2CTests() {
	}

	@Test
	public void testEmpty() {
		final String pySrc = readCode("empty.py");
		final String cSrc = expectSuccess(pySrc);
		compileCode(cSrc);
		runCode("");
	}

	@Test
	public void testHelloWorld() {
		final String pySrc = readCode("hello_world.py");
		final String cSrc = expectSuccess(pySrc);
		compileCode(cSrc);
		runCode("Hello world" + System.lineSeparator());
	}

	@Test
	public void testCalc() {
		final String pySrc = readCode("calc.py");
		final String cSrc = expectSuccess(pySrc);
		compileCode(cSrc);
		runCode("4210 4190 42000 420 8 4202 4194" + System.lineSeparator() + "6" + System.lineSeparator());
	}

	@Test
	public void testIf() {
		final String pySrc = readCode("if.py");
		final String cSrc = expectSuccess(pySrc);
		compileCode(cSrc);
		runCode("even" + System.lineSeparator() + "odd" + System.lineSeparator());
	}

	@Test
	public void testFunc() {
		final String pySrc = readCode("func.py");
		final String cSrc = expectSuccess(pySrc);
		compileCode(cSrc);
		runCode("There is nothing we can do" + System.lineSeparator());
	}

	@Test
	public void testWhile() {
		final String pySrc = readCode("while.py");
		final String cSrc = expectSuccess(pySrc);
		compileCode(cSrc);
		runCode("123" + "42".repeat(42) + System.lineSeparator());
	}

	@Test
	public void testFor() {
		final String pySrc = readCode("for.py");
		final String cSrc = expectSuccess(pySrc);
		compileCode(cSrc);
		runCode(Collections.nCopies(3, "42").stream().collect(Collectors.joining(System.lineSeparator())) + "42".repeat(100 - 5) + System.lineSeparator());
	}

	@Test
	public void testDefArgs() {
		final String pySrc = readCode("func_with_args.py");
		final String cSrc = expectSuccess(pySrc);
		compileCode(cSrc);
		runCode("1" + System.lineSeparator() + "1.100000 2.200000" + System.lineSeparator() + "123 123" + System.lineSeparator());
	}

	@Test
	public void errorDefArgs() {
		final String pySrc = readCode("error_func_with_args.py");
		expectFailed(pySrc, Py2CMultipleTypeDefinitionException.class);
	}

	@Test
	public void errorTypes() {
		final String pySrc = readCode("error_types.py");
		expectFailed(pySrc, Py2CIncompatibleTypesException.class);
	}

	@Test
	public void errorBinary() {
		final String pySrc = readCode("error_binary.py");
		expectFailed(pySrc, Py2CUnsupportedBinaryExpressionOperationException.class);
	}

	@Test
	public void errorNotImplementedYet() {
		final String pySrc = readCode("error_not_implemented_yet.py");
		expectFailed(pySrc, Py2CIncompatibleTypesException.class);
	}
}
