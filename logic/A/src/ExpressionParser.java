/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class ExpressionParser {
	private final CharIterator iterator;

	public ExpressionParser(final CharIterator iterator) {
		this.iterator = iterator;
	}

	public ExpressionParser(final String source) {
		this(new CharIterator(source));
	}

	public Expression parse() {
		return parseLowest();
	}

	private Expression parseLowest() {
		Expression result = parseNormal();
		char c = iterator.next();
		if (c == '-') {
			iterator.skip();
			result = new Implicatio(result, parseLowest());
		} else if (c == ')') {
			return result;
		}
		return result;
	}

	private Expression parseNormal() {
		Expression result = parseHighest();
		char c = 0;
		while (iterator.hasNext() && ((c = iterator.next()) == '|')) {
			result = new Disjunctio(result, parseHighest());
		}
		iterator.prev();
		return result;
	}

	private Expression parseHighest() {
		Expression result = parseAtom();
		char c = 0;
		while (iterator.hasNext() && ((c = iterator.next()) == '&')) {
			result = new Conjunctio(result, parseAtom());
		}
		iterator.prev();
		return result;
	}

	private Expression parseAtom() {
		Expression result = null;
		char c = iterator.next();
		if (isValidVariableChar(c)) {
			result = parseVariable(c);
		} else if (c == '!') {
			result = new Inversio(parseAtom());
		} else if (c == '(') {
			result = parseLowest();
		}
		return result;
	}

	private Expression parseVariable(final char c) {
		final StringBuilder sb = new StringBuilder();
		sb.append(c);
		char cur = iterator.next();
		while (isValidVariableChar(cur)) {
			sb.append(cur);
			cur = iterator.next();
		}
		iterator.prev();
		return new Variable(sb.toString());
	}

	private static boolean isValidVariableChar(final char c) {
		return  Character.isAlphabetic(c) || Character.isDigit(c) || (c == ((char)39));
	}
}
