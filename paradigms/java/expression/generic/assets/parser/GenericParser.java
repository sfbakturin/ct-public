package expression.generic.assets.parser;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface GenericParser<T> {
	GenericExpression<T> parse(String expression) throws Exception;
}
