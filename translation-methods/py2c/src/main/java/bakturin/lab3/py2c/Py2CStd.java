package bakturin.lab3.py2c;

import bakturin.lab3.py2c.assets.CExpression;
import bakturin.lab3.py2c.assets.CType;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CStd {
	private final static Function<List<CExpression>, String> STD_FUN_PRINT = (args) -> "printf" + "(" + "\"" +
			args.stream().map(CExpression::getType).map(CType::getFormat).collect(Collectors.joining(" ")) +
			"\\n" + "\"" + "," +
			args.stream().map(CExpression::asExpression).collect(Collectors.joining(",")) +
			")";

	public final static Map<String, Function<List<CExpression>, String>> STD_FUNCTIONS = new HashMap<>();
	public final static String STD_INITIALIZE = """
			#include <stdio.h>
			#include <stdlib.h>
			#include <string.h>
			#include <math.h>

			char *std_string_concat(char *left, char *right)
			{
			\tchar *cat;
			\tsize_t len_l = strlen(left), len_r = strlen(right);
			\tsize_t len = len_l + len_r;
			\tcat = malloc(sizeof(char) * (len + 1));
			\tif (!cat)
			\t{
			\t\tprintf("Runtime error: out of memory.\\n");
			\t\texit(-1);
			\t}
			\tcat[len] = '\\0';
			\tstrcpy(cat, left);
			\tstrcpy(cat + len_l, right);
			\treturn cat;
			}
			""";

	static {
		STD_FUNCTIONS.put("print", STD_FUN_PRINT);
	}

	private Py2CStd() {
		throw new AssertionError("No bakturin.lab3.py2c.Py2CStd instances for you!");
	}

	public static Function<List<CExpression>, String> makeDefaultFunction(final String functionName) {
		return (args) -> functionName + "(" + args.stream().map(CExpression::asExpression).collect(Collectors.joining(",")) + ")";
	}
}
