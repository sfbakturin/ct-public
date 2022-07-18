package expression.generic;

import expression.exceptions.custom.EvaluateException;
import expression.generic.assets.operation.*;
import expression.generic.assets.parser.GenericExpression;
import expression.generic.assets.parser.GenericExpressionParser;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class GenericTabulator implements Tabulator {
    private static final Map<String, GenericOperationType<?>> SELECTED_MODES = new LinkedHashMap<>();
    private static int X_START, X_END;
    private static int Y_START, Y_END;
    private static int Z_START, Z_END;

    static {
        SELECTED_MODES.put("i", new GenericOperationCheckedInteger());
        SELECTED_MODES.put("d", new GenericOperationDouble());
        SELECTED_MODES.put("bi", new GenericOperationBigInteger());
        SELECTED_MODES.put("u", new GenericOperationUncheckedInteger());
        SELECTED_MODES.put("l", new GenericOperationLong());
        SELECTED_MODES.put("f", new GenericOperationFloat());
        SELECTED_MODES.put("s", new GenericOperationShort());
        SELECTED_MODES.put("t", new GenericOperationTruncateInteger());
    }

    @Override
    public Object[][][] tabulate(final String mode, final String expression, final int x1, final int x2, final int y1, final int y2, final int z1, final int z2) {
        X_START = x1;
        X_END = x2;
        Y_START = y1;
        Y_END = y2;
        Z_START = z1;
        Z_END = z2;
        return this.returnMatrix(SELECTED_MODES.get(mode), expression);
    }

    private <T> Object[][][] returnMatrix(final GenericOperationType<T> mode, final String expr) {
        final Object[][][] matrix = new Object[X_END - X_START + 1][Y_END - Y_START + 1][Z_END - Z_START + 1];
        final GenericExpression<T> generated = (new GenericExpressionParser<>(mode).parse(expr));
        for (int x = X_START; x <= X_END; x++) {
            for (int y = Y_START; y <= Y_END; y++) {
                for (int z = Z_START; z <= Z_END; z++) {
                    try {
                        matrix[x - X_START][y - Y_START][z - Z_START] = generated.evaluate(mode.generate(String.valueOf(x)), mode.generate(String.valueOf(y)), mode.generate(String.valueOf(z)));
                    } catch (final EvaluateException err) {
                        matrix[x - X_START][y - Y_START][z - Z_START] = null;
                    }
                }
            }
        }
        return matrix;
    }
}
