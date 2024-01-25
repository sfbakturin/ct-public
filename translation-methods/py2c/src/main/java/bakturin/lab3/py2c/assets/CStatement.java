package bakturin.lab3.py2c.assets;

import java.util.Map;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface CStatement extends CExpression {
	String asStatement();

	String asStatement(Map<String, CVariable> args);

	void fixTypes(Map<String, CVariable> args);

	void fixTypes(Map<String, CVariable> args, Map<String, CVariable> defArgs);
}
