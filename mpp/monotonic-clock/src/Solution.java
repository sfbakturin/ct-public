import org.jetbrains.annotations.NotNull;

/**
 * В теле класса решения разрешено использовать только финальные переменные типа RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author :TODO: Bakturin Saveliy
 */
public class Solution implements MonotonicClock {
    private final RegularInt c11 = new RegularInt(0);
    private final RegularInt c12 = new RegularInt(0);
    private final RegularInt c13 = new RegularInt(0);
    private final RegularInt c21 = new RegularInt(0);
    private final RegularInt c22 = new RegularInt(0);
    private final RegularInt c23 = new RegularInt(0);

    @Override
    public void write(@NotNull Time time) {
        // c2: left -> right
        c21.setValue(time.getD1());
        c22.setValue(time.getD2());
        c23.setValue(time.getD3());

        // c1: right -> left
        c13.setValue(c23.getValue());
        c12.setValue(c22.getValue());
        c11.setValue(c21.getValue());
    }

    @NotNull
    @Override
    public Time read() {
        // c1: left -> right
        final int r11 = c11.getValue();
        final int r12 = c12.getValue();
        final int r13 = c13.getValue();

        // c2: right -> left
        final int r23 = c23.getValue();
        final int r22 = c22.getValue();
        final int r21 = c21.getValue();

        // set arrays for any bad cases
        final int[] r1s = new int[]{r11, r12, r13};
        final int[] r2s = new int[]{r21, r22, r23};

        // case: r1 == r2
        if (r11 == r21 && r12 == r22 && r13 == r23) {
            return new Time(r11, r12, r13);
        } else {
            // case: finding p and return [r1_{p + 1} , r2_{p + 1}, 0, ...]
            for (int p = 0; p < 3; p++) {
                 if (r1s[p] != r2s[p]) {
                     final int[] ret = new int[3];
                     for (int j = 0; j < p; j++) {
                         ret[j] = r1s[j];
                     }
                     ret[p] = r2s[p];
                     return new Time(ret[0], ret[1], ret[2]);
                 }
            }
        }

        // case: there is should be not that return...
        return new Time(r21, r22, r23);
    }
}
