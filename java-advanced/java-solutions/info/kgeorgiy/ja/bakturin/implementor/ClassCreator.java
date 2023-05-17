package info.kgeorgiy.ja.bakturin.implementor;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.util.Arrays;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

/**
 * The {@code Class<?>} that generates the subject interface.
 */
public class ClassCreator {
	/**
	 * Constant value of line separator.
	 * Used in class generator to divide by newlines for human-readable text formatting of Java source code.
	 */
	private static final String LINE_SEPARATOR = System.lineSeparator();
	/**
	 * Constant value for formatting {@code package} name.
	 * Used in the class generator to correctly form a string containing information about the {@code package} in which the source code of the class is located, if the supplied {@code Class<?>} really lies in some non-root {@code package}.
	 */
	private static final String FORMAT_PACKAGE = "package %s;";
	/**
	 * Constant value for formatting {@code Class<?>} declaration of the generated class.
	 */
	private static final String FORMAT_CLASS = "public class %sImpl implements %s";
	/**
	 * Constant value for formatting method's declaration of the generated class.
	 */
	private static final String FORMAT_METHOD = "public %s %s(%s)";
	/**
	 * Constant value for formatting a string used as an enumeration point for {@code Exception} types from a method of the generated class.
	 */
	private static final String FORMAT_THROWS = "throws %s";
	/**
	 * Constant value for formatting a string used as a return point from a method of the generated class.
	 */
	private static final String FORMAT_RETURN = "return %s;";
	/**
	 * Constant value for initiation of any constructs of the generated class.
	 */
	private static final String FORMAT_BEGIN = " " + "{" + LINE_SEPARATOR;
	/**
	 * Constant value for completion of any constructs of the generated class.
	 */
	private static final String FORMAT_END = "}" + LINE_SEPARATOR;
	/**
	 * Constant value for formatting sequence of objects in {@code Parameter} pack or exceptions.
	 */
	private static final String FORMAT_NEXT = "," + " ";
	/**
	 * Constant value of tabulation.
	 */
	private static final String FORMAT_TABULATION = "\t";
	/**
	 * Constant value of string meaning overriding methods from {@code Interface}.
	 */
	private static final String FORMAT_OVERRIDE = "@Override";
	/**
	 * The buffer value of generalized {@code Class<?>}.
	 * Used for buffered storage of the current state of the generated class in the form of mandatory human-readable formatted Java source code.
	 *
	 * @see StringBuilder
	 */
	private final StringBuilder sb;
	/**
	 * The reference value of {@code Class<?>} that is being generated.
	 * Used to access the field from outside the generator without copying the token itself.
	 *
	 * @see Class
	 */
	private final Class<?> tok;

	/**
	 * Construct of {@code ClassCreator} from reference of {@code Class<?>}.
	 *
	 * @param token {@code Class<?>}, a reference to {@code Class<?>} that is being generated.
	 */
	public ClassCreator(final Class<?> token) {
		sb = new StringBuilder();
		tok = token;
	}

	/**
	 * Main function of generating {@code Class<?>} from specified {@code Interface}.
	 */
	public void genClass() {
		writePackage(tok.getPackage());
		writeNewLine();
		writeNewLine();
		writeName(tok);
		writeBegin();
		writeNewLine();
		for (final Method item : tok.getMethods()) {
			writeMethod(item);
		}
		writeEnd();
		writeNewLine();
	}

	/**
	 * Return human-readable {@code String} of generated specified {@code Class<?>}.
	 *
	 * @return generated and formatted {@code String} from {@code StringBuilder}.
	 * @see StringBuilder#toString()
	 */
	@Override
	public String toString() {
		return sb.toString();
	}

	/**
	 * {@code Method} for writing the {@code package} of the generated {@code Class<?>}.
	 *
	 * @param name {@code Package} that should be written to private {@code StringBuilder} for buffer.
	 * @see Package#getName()
	 */
	private void writePackage(final Package name) {
		final String packageName = name.getName();
		if (packageName.length() != 0) {
			sb.append(String.format(FORMAT_PACKAGE, name.getName()));
		}
	}

	/**
	 * {@code Method} for writing the signature of the generated {@code Class<?>}.
	 *
	 * @param name {@code Class<?>}, that should be used to get real name of generated {@code Class<?>} and real name of implemented {@code Interface}.
	 * @see Class#getSimpleName()
	 * @see Class#getCanonicalName()
	 */
	private void writeName(final Class<?> name) {
		sb.append(String.format(FORMAT_CLASS, name.getSimpleName(), name.getCanonicalName()));
	}

	/**
	 * An abstract {@code Method} for formatting a given getter into a list with commas.
	 *
	 * @param ts {@code <T>[]} objects from which to format into a string.
	 * @param getter {@code Function<? super T, ? extends String>} a function to get a string from the supplied objects.
	 * @return {@code String} reformatted by getter and FORMAT_NEXT.
	 * @param <T> {@code <T>} type of transferred objects.
	 */
	private static <T> String format(final T[] ts, final Function<? super T, ? extends String> getter) {
		return Arrays.stream(ts).map(getter).collect(Collectors.joining(FORMAT_NEXT));
	}

	/**
	 * {@code Method} for formatting the list of parameters passed to the method.
	 *
	 * @param parameters {@code Parameter[]} parameters to be formatted on a single line.
	 * @return {@code String} formatting result.
	 */
	private static String formatMethodParameters(final Parameter[] parameters) {
		return format(parameters, s -> s.getType().getCanonicalName() + " " + s.getName());
	}

	/**
	 * {@code Method} for formatting the list of method exceptions thrown.
	 *
	 * @param exceptions {@code Class<?>[]} exception types to be formatted on a single line.
	 * @return {@code String} formatting result.
	 */
	private static String formatMethodExceptions(final Class<?>[] exceptions) {
		return format(exceptions, Class::getCanonicalName);
	}

	/**
	 * {@code Method} for writing the signature of the given method in the generated {@code Class<?>}.
	 *
	 * @param method {@code Method} from which we get the signature of this method.
	 */
	private void writeMethodHead(final Method method) {
		final Class<?>[] exceptions = method.getExceptionTypes();
		final Parameter[] parameters = method.getParameters();
		sb.append(String.format(FORMAT_METHOD, method.getReturnType().getCanonicalName(), method.getName(), formatMethodParameters(parameters)));
		if (exceptions.length != 0) {
			sb.append(String.format(" " + FORMAT_THROWS, formatMethodExceptions(exceptions)));
		}
	}

	/**
	 * {@code Method} for writing a default return value in the generated {@code Class<?>}.
	 *
	 * @param method {@code Method} from which we get the default value of the return type.
	 */
	private void writeMethodReturn(final Method method) {
		if (method.getReturnType() != Void.TYPE) {
			final String type = (method.getReturnType().isPrimitive() ? (method.getReturnType() == Boolean.TYPE ? "false" : "0") : "null");
			sb.append(String.format(FORMAT_RETURN, type));
		}
	}

	/**
	 * {@code Method} that is a sequence of other methods to generate the given method in the generated {@code Class<?>}.
	 *
	 * @param method {@code Method} method that will be implemented at the moment.
	 */
	private void writeMethod(final Method method) {
		if (method.isDefault() || Modifier.isStatic(method.getModifiers())) {
			return;
		}
		writeTab();
		writeOverride();
		writeNewLine();
		writeTab();
		writeMethodHead(method);
		writeBegin();
		writeTab();
		writeTab();
		writeMethodReturn(method);
		writeNewLine();
		writeTab();
		writeEnd();
		writeNewLine();
	}

	/**
	 * {@code Method} for writing a new line in the generated {@code Class<?>}.
	 */
	private void writeNewLine() {
		sb.append(LINE_SEPARATOR);
	}

	/**
	 * {@code Method} for writing a tab in the generated {@code Class<?>}.
	 */
	private void writeTab() {
		sb.append(FORMAT_TABULATION);
	}

	/**
	 * {@code Method} for writing a curly brace, denoting the start of a construct of a generated {@code Class<?>}.
	 */
	private void writeBegin() {
		sb.append(FORMAT_BEGIN);
	}

	/**
	 * {@code Method} for writing a curly brace that marks the end of a construct generated by a {@code Class<?>}.
	 */
	private void writeEnd() {
		sb.append(FORMAT_END);
	}

	/**
	 * {@code Method} for writing the inscription {@code @Override} of the generated {@code Class<?>}.
	 */
	private void writeOverride() {
		sb.append(FORMAT_OVERRIDE);
	}
}
