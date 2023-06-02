package info.kgeorgiy.ja.bakturin.i18n.util;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

@FunctionalInterface
public interface TriFunction<F, S, T, R> {
	R apply(F f, S s, T t);
}
