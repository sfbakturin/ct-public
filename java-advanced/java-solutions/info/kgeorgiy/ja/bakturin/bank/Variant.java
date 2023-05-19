package info.kgeorgiy.ja.bakturin.bank;

import java.io.Serializable;
import java.util.function.BiFunction;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface Variant<F, S> extends Serializable {
	F getFirst() throws IllegalArgumentException;

	S getSecond() throws IllegalArgumentException;

	default <R, T> R applyFirst(final BiFunction<F, T, R> bi, final T t) throws IllegalArgumentException {
		return bi.apply(getFirst(), t);
	}

	default <R, T> R applySecond(final BiFunction<S, T, R> bi, final T t) throws IllegalArgumentException {
		return bi.apply(getSecond(), t);
	}
}
