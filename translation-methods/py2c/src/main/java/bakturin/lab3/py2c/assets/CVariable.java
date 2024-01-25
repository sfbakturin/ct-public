package bakturin.lab3.py2c.assets;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface CVariable extends CExpression {
	String asDeclaration();

	String getName();
}
