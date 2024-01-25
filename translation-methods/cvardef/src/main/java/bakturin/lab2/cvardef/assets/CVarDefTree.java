package bakturin.lab2.cvardef.assets;

import java.util.Map;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import bakturin.lab2.cvardef.CVarDefVariable;
import bakturin.lab2.cvardef.exceptions.CVarDefASTException;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CVarDefTree {
	private final static String DOT_NODE_FORMAT = "l%de%d[label = \"%s\"]";
	private final static String DOT_LINK_FORMAT = "l%de%d -> l%de%d";
	private final static String NEWLINE_CHAR = System.lineSeparator();

	private final static Map<String, Integer> POSSIBLE_COUNT_MODS = Map.of(
			"volatile", 1, "static", 1, "const", 1);
	private final static Map<String, Integer> POSSIBLE_COUNT_TYPE = Map.of("double", 1, "float", 1, "int", 1,
			"short", 1, "char", 1, "long", 2, "unsigned", 1);
	private final boolean exceptions;
	final Node node;
	final String name;
	final List<CVarDefTree> children;
	final String defaultValue;
	private final boolean isInitialized;

	public CVarDefTree(final boolean exceptions, final Node node, final CVarDefTree... children) {
		this.exceptions = exceptions;
		this.node = node;
		name = null;
		this.children = Arrays.asList(children);
		isInitialized = false;
		defaultValue = "";
	}

	public CVarDefTree(final boolean exceptions, final Node node, final String name) {
		this.exceptions = exceptions;
		this.node = node;
		this.name = name;
		children = Collections.emptyList();
		isInitialized = false;
		defaultValue = "";
	}

	public CVarDefTree(final boolean exceptions, final Node node, final String name, final CVarDefTree... children) {
		this.exceptions = exceptions;
		this.node = node;
		this.name = name;
		this.children = Arrays.asList(children);
		isInitialized = false;
		defaultValue = "";
	}

	public CVarDefTree(final boolean exceptions, final Node node, final String name, final String defaultValue,
			final CVarDefTree... children) {
		this.exceptions = exceptions;
		this.node = node;
		this.name = name;
		this.children = (Arrays.stream(children).anyMatch(Objects::isNull) ? Collections.emptyList()
				: Arrays.asList(children));
		isInitialized = true;
		this.defaultValue = defaultValue;
	}

	private List<String> getAbstract(final Predicate<String> fun) {
		final List<String> mods = new ArrayList<>();
		CVarDefTree parent = children.get(0);
		while (!parent.children.isEmpty()) {
			if (fun.test(parent.name)) {
				mods.add(parent.name);
			}
			parent = parent.children.get(0);
		}
		if (fun.test(parent.name)) {
			mods.add(parent.name);
		}
		return mods;
	}

	private List<String> getModifiers() {
		return getAbstract(POSSIBLE_COUNT_MODS::containsKey);
	}

	private List<String> getTypes() {
		return getAbstract(s -> !POSSIBLE_COUNT_MODS.containsKey(s));
	}

	private static <T> long nFilter(final List<T> list, final T eq) {
		return list.stream().filter(s -> s.equals(eq)).count();
	}

	@SafeVarargs
	private static <T> long nFilter(final List<T> list, final T... ts) {
		return Arrays.stream(ts).mapToLong(s -> nFilter(list, s)).sum();
	}

	private String getDots(final int parent, final int depth, final int startIndex) {
		final StringBuilder builder = new StringBuilder();

		if (node == Node.TYPE || node == Node.NAME) {
			builder.append(addDotNode(depth, startIndex, "\\\"" + name + "\\\""));
			builder.append(addDotLink(depth - 1, parent, depth, startIndex));
		}

		if (node == Node.NAME && isInitialized) {
			builder.append(addDotNode(depth, startIndex + 1, "\\\"" + defaultValue + "\\\""));
			builder.append(addDotLink(depth - 1, parent, depth, startIndex + 1));
		}

		for (int i = 0; i < children.size(); i++) {
			switch (children.get(i).node) {
				case START -> {
					builder.append(addDotNode(depth, i, "S"));
					builder.append(children.get(i).getDots(i, depth + 1, -1));
					builder.append(addDotLink(depth - 1, parent, depth, i));
				}
				case TYPE -> {
					builder.append(addDotNode(depth, i, "TYPE"));
					builder.append(children.get(i).getDots(i, depth + 1, (i % children.size()) + children.size()));
					builder.append(addDotLink(depth - 1, parent, depth, i));
				}
				case POINTER -> {
					builder.append(addDotNode(depth, i, "POINTER"));
					builder.append(children.get(i).getDots(i, depth + 1, -1));
					builder.append(addDotLink(depth - 1, parent, depth, i));
				}
				case NAME -> {
					builder.append(addDotNode(depth, i, "NAME"));
					builder.append(children.get(i).getDots(i, depth + 1, (i % children.size()) + children.size()));
					builder.append(addDotLink(depth - 1, parent, depth, i));
				}
			}
		}

		return builder.toString();
	}

	private static String addDotLink(final int depthSrc, final int nmembSrc, final int depthDest, final int nmembDest) {
		return DOT_LINK_FORMAT.formatted(depthSrc, nmembSrc, depthDest, nmembDest) + NEWLINE_CHAR;
	}

	private static String addDotNode(final int depth, final int nmemb, final String name) {

		return DOT_NODE_FORMAT.formatted(depth, nmemb, name) + NEWLINE_CHAR;
	}

	private static String addDotNodeInit() {
		return addDotNode(0, 0, "S");
	}

	public String getDot() {
		return "strict digraph {" + NEWLINE_CHAR +
				addDotNodeInit() +
				getDots(0, 1, -1) +
				"}" + NEWLINE_CHAR;
	}

	public List<CVarDefVariable> getVariables() {
		final List<String> rawModifiers = getModifiers();
		final List<String> rawTypes = getTypes();

		for (final String mod : POSSIBLE_COUNT_MODS.keySet()) {
			if (rawModifiers.stream().filter(s -> s.equals(mod)).count() > POSSIBLE_COUNT_MODS.get(mod)) {
				if (exceptions) {
					throw new CVarDefASTException(mod, POSSIBLE_COUNT_MODS.get(mod));
				} else {
					return null;
				}
			}
		}

		for (final String type : POSSIBLE_COUNT_TYPE.keySet()) {
			if (rawTypes.stream().filter(s -> s.equals(type)).count() > POSSIBLE_COUNT_TYPE.get(type)) {
				if (exceptions) {
					throw new CVarDefASTException(type, POSSIBLE_COUNT_TYPE.get(type));
				} else {
					return null;
				}
			}
		}

		for (final String type : rawTypes) {
			switch (type) {
				case "char": {
					final long nSum = nFilter(rawTypes, "short", "int", "long", "float", "double");
					if (nSum != 0) {
						if (exceptions) {
							throw new CVarDefASTException("char", "short", "int", "long", "float", "double");
						} else {
							return null;
						}
					}
					break;
				}
				case "short": {
					final long nSum = nFilter(rawTypes, "char", "long", "float", "double");
					if (nSum != 0) {
						if (exceptions) {
							throw new CVarDefASTException("short", "char", "long", "float", "double");
						} else {
							return null;
						}
					}
					break;
				}
				case "int": {
					final long nSum = nFilter(rawTypes, "char", "float", "double");
					if (nSum != 0) {
						if (exceptions) {
							throw new CVarDefASTException("int", "char", "float", "double");
						} else {
							return null;
						}
					}
					break;
				}
				case "long": {
					final long nSum = nFilter(rawTypes, "char", "short", "float");
					if (nSum != 0) {
						if (exceptions) {
							throw new CVarDefASTException("long", "char", "short", "float");
						} else {
							return null;
						}
					}
					break;
				}
				case "float": {
					final long nSum = nFilter(rawTypes, "char", "short", "int", "long", "double");
					if (nSum != 0) {
						if (exceptions) {
							throw new CVarDefASTException("float", "char", "short", "int", "long", "double");
						} else {
							return null;
						}
					}
					if (nFilter(rawTypes, "unsigned") != 0 || nFilter(rawTypes, "signed") != 0) {
						if (exceptions) {
							throw new CVarDefASTException("fractional can't be signed or unsigned");
						} else {
							return null;
						}
					}
					break;
				}
				case "double": {
					final long nSum = nFilter(rawTypes, "char", "short", "int", "float");
					if (nSum != 0) {
						if (exceptions) {
							throw new CVarDefASTException("double", "char", "short", "int", "float");
						} else {
							return null;
						}
					}
					if (nFilter(rawTypes, "long") == 2) {
						if (exceptions) {
							throw new CVarDefASTException("'long long double' is impossible");
						} else {
							return null;
						}
					}
					if (nFilter(rawTypes, "unsigned") != 0 || nFilter(rawTypes, "signed") != 0) {
						if (exceptions) {
							throw new CVarDefASTException("fractional can't be signed or unsigned");
						} else {
							return null;
						}
					}
					break;
				}
				default:
					break;
			}
		}

		final String type = (rawModifiers.isEmpty() ? ""
				: rawModifiers.stream().sorted().collect(Collectors.joining(" ")) + " ")
				+ rawTypes.stream().sorted().collect(Collectors.joining(" "));
		final List<CVarDefVariable> variables = new ArrayList<>();
		CVarDefTree child = children.get(1);
		Node node = child.node;
		int pointer = 0;
		while (node == Node.POINTER) {
			pointer++;
			child = child.children.get(0);
			node = child.node;
		}
		if (child.isInitialized) {
			variables.add(new CVarDefVariable(child.name, type, pointer, child.defaultValue));
		} else {
			variables.add(new CVarDefVariable(child.name, type, pointer));
		}
		while (!child.children.isEmpty()) {
			child = child.children.get(0);
			node = child.node;
			pointer = 0;
			while (node == Node.POINTER) {
				pointer++;
				child = child.children.get(0);
				node = child.node;
			}
			if (child.isInitialized) {
				variables.add(new CVarDefVariable(child.name, type, pointer, child.defaultValue));
			} else {
				variables.add(new CVarDefVariable(child.name, type, pointer));
			}
		}

		if (variables.stream().map(s -> s.name).collect(Collectors.toSet()).size() != variables.size()) {
			if (exceptions) {
				throw new CVarDefASTException("in variables definitions there should be unique names");
			} else {
				return null;
			}
		}

		return variables;
	}

	public enum Node {
		START, TYPE, POINTER, NAME
	}
}
