import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import bakturin.lab2.cvardef.CVarDef;
import bakturin.lab2.cvardef.CVarDefVariable;
import bakturin.lab2.cvardef.exceptions.CVarDefASTException;
import bakturin.lab2.cvardef.exceptions.CVarDefLexerException;
import bakturin.lab2.cvardef.exceptions.CVarDefParserException;

import static org.junit.Assert.*;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CVarDefTests extends CVarDefTester {
	private final static List<String> MODIFIERS = List.of("const", "volatile", "static");

	public CVarDefTests() {
	}

	@Test
	public void exampleTest() {
		final Map<String, Integer> option = Map.of("a", 0, "b", 1, "c", 3, "d", 0);
		final List<CVarDefVariable> expected = makeVariables("int", option);
		final List<CVarDefVariable> actual = CVarDef.parse(makeString("int", option));
		assertEquals(actual, expected);
	}

	@Test(expected = CVarDefLexerException.class)
	public void errorEmptyTest() {
		CVarDef.parse("");
	}

	@Test(expected = CVarDefParserException.class)
	public void errorOnlySemicolonTest() {
		CVarDef.parse(";");
	}

	@Test(expected = CVarDefParserException.class)
	public void errorOnlyCommaTest() {
		CVarDef.parse(",");
	}

	@Test(expected = CVarDefParserException.class)
	public void errorOnlyTypeTest() {
		CVarDef.parse("int;");
	}

	@Test(expected = CVarDefLexerException.class)
	public void errorNoSemicolonTest1() {
		CVarDef.parse("int a");
	}

	@Test(expected = CVarDefLexerException.class)
	public void errorCommaNotRequiredTest1() {
		CVarDef.parse("int a,");
	}

	@Test(expected = CVarDefLexerException.class)
	public void errorNoSemicolonTest2() {
		CVarDef.parse("int *************a");
	}

	@Test(expected = CVarDefLexerException.class)
	public void errorCommaNotRequiredTest2() {
		CVarDef.parse("int ****************************a,");
	}

	@Test
	public void errorsDoubleSameModifierTest() {
		final String base = "int a;";
		for (final String mod : MODIFIERS) {
			try {
				CVarDef.parse(mod + " " + mod + " " + base);
				fail();
			} catch (final CVarDefASTException ignored) {
			} catch (final Exception e) {
				fail();
			}
		}
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest1() {
		CVarDef.parse("int int a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest2() {
		CVarDef.parse("char int a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest3() {
		CVarDef.parse("float int a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest4() {
		CVarDef.parse("char float a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest5() {
		CVarDef.parse("char double a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest6() {
		CVarDef.parse("char long double a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest7() {
		CVarDef.parse("char long long int a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest8() {
		CVarDef.parse("char long int a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest9() {
		CVarDef.parse("short float a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest10() {
		CVarDef.parse("short double a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest11() {
		CVarDef.parse("short long double a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest12() {
		CVarDef.parse("short long long int a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest13() {
		CVarDef.parse("short long int a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest14() {
		CVarDef.parse("long long unsigned float a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest15() {
		CVarDef.parse("long long unsigned double a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest16() {
		CVarDef.parse("long long unsigned long double a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest17() {
		CVarDef.parse("long long unsigned long long int a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest18() {
		CVarDef.parse("long long unsigned long int a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorTypesTest19() {
		CVarDef.parse("const unsigned volatile static float double long long a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorUnsignedFractionalTest1() {
		CVarDef.parse("unsigned float a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorUnsignedFractionalTest2() {
		CVarDef.parse("unsigned double a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorSignedFractionalTest1() {
		CVarDef.parse("signed float a;");
	}

	@Test(expected = CVarDefASTException.class)
	public void errorSignedFractionalTest2() {
		CVarDef.parse("signed double a;");
	}

	@Test(expected = CVarDefLexerException.class)
	public void errorDefaultValueTest1() {
		CVarDef.parse("int a =");
	}

	@Test(expected = CVarDefParserException.class)
	public void errorDefaultValueTest2() {
		CVarDef.parse("int =");
	}

	@Test(expected = CVarDefParserException.class)
	public void errorDefaultValueTest3() {
		CVarDef.parse("   = 123");
	}

	@Test(expected = CVarDefLexerException.class)
	public void errorDefaultValueTest4() {
		CVarDef.parse("int a = 1");
	}

	@Test
	public void modTest() {
		CVarDef.parse("int a = 1, *b, ***c = NULL, d;").forEach(System.out::println);
	}

	@Test
	public void bigSuccessTest() {
		for (final List<String> rawMods : List.of(List.of("static"), List.of("const"), List.of("volatile"),
				List.of("static", "const"), List.of("static", "volatile"), List.of("volatile", "const"),
				List.of("static", "const", "volatile"))) {
			for (final List<String> rawType : List.of(List.of("char"), List.of("int"), List.of("long"),
					List.of("int", "long"),
					List.of("int", "long", "long"), List.of("long", "long"), List.of("float"), List.of("double"),
					List.of("long", "double"))) {
				final String mods = makeSortedString(rawMods);
				final String type = makeSortedString(rawType);
				final String modifiersType = mods + " " + type;
				final String base = "var";
				final Map<String, Integer> option = new HashMap<>();
				for (int i = 0; i < 75; i++) {
					option.put(base + i, i);
				}
				final List<CVarDefVariable> expected = makeVariables(modifiersType, option);
				final List<CVarDefVariable> actual = CVarDef.parse(makeString(modifiersType, option));
				assertEquals(actual, expected);
			}
		}
	}
}
