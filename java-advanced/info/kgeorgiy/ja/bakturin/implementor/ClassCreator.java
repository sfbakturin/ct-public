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

public class ClassCreator {
	private static final String LINE_SEPARATOR = System.lineSeparator();
	private static final String FORMAT_PACKAGE = "package %s;";
	private static final String FORMAT_CLASS = "public class %sImpl implements %s";
	private static final String FORMAT_METHOD = "public %s %s(%s)";
	private static final String FORMAT_THROWS = "throws %s";
	private static final String FORMAT_RETURN = "return %s;";
	private static final String FORMAT_BEGIN = " " + "{" + LINE_SEPARATOR;
	private static final String FORMAT_END = "}" + LINE_SEPARATOR;
	private static final String FORMAT_NEXT = ",";
	private static final String FORMAT_TABULATION = "\t";
	private final StringBuilder sb;
	private final Class<?> token;

	public ClassCreator(final Class<?> token) {
		sb = new StringBuilder();
		this.token = token;
	}

	public void genClass() {
		writePackage(token.getPackage());
		writeNewLine();
		writeNewLine();
		writeName(token);
		writeBegin();
		writeNewLine();
		for (final Method item : token.getMethods()) {
			writeMethod(item);
		}
		writeEnd();
		writeNewLine();
	}

	@Override
	public String toString() {
		return sb.toString();
	}

	private void writePackage(final Package name) {
		final String packageName = name.getName();
		if (packageName.length() != 0) {
			sb.append(String.format(FORMAT_PACKAGE, name.getName()));
		}
	}

	private void writeName(final Class<?> name) {
		sb.append(String.format(FORMAT_CLASS, name.getSimpleName(), name.getCanonicalName()));
	}

	private static <T> String format(final T[] ts, final Function<? super T, ? extends String> getter) {
		return Arrays.stream(ts).map(getter).collect(Collectors.joining(FORMAT_NEXT));
	}

	private static String formatMethodParameters(final Parameter[] parameters) {
		return format(parameters, s -> s.getType().getCanonicalName() + " " + s.getName());
	}

	private static String formatMethodExceptions(final Class<?>[] exceptions) {
		return format(exceptions, Class::getCanonicalName);
	}

	private void writeMethodHead(final Method method) {
		final Class<?>[] exceptions = method.getExceptionTypes();
		final Parameter[] parameters = method.getParameters();
		sb.append(String.format(FORMAT_METHOD, method.getReturnType().getCanonicalName(), method.getName(), formatMethodParameters(parameters)));
		if (exceptions.length != 0) {
			sb.append(String.format(" " + FORMAT_THROWS, formatMethodExceptions(exceptions)));
		}
	}

	private void writeMethodReturn(final Method method) {
		if (method.getReturnType() != Void.TYPE) {
			final String type = (method.getReturnType().isPrimitive() ? (method.getReturnType() == Boolean.TYPE ? "false" : "0") : "null");
			sb.append(String.format(FORMAT_RETURN, type));
		}
	}

	private void writeMethod(final Method method) {
		if (method.isDefault() || Modifier.isStatic(method.getModifiers())) {
			return;
		}
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

	private void writeNewLine() {
		sb.append(LINE_SEPARATOR);
	}

	private void writeTab() {
		sb.append(FORMAT_TABULATION);
	}

	private void writeBegin() {
		sb.append(FORMAT_BEGIN);
	}

	private void writeEnd() {
		sb.append(FORMAT_END);
	}
}
