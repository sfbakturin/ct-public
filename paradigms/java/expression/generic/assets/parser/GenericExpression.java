package expression.generic.assets.parser;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface GenericExpression<T> {
	T evaluate(T x, T y, T z);
}
