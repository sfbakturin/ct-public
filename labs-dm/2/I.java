import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class I {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int start = in.nextInt();
        in.nextLine();
        final StringBuilder sb = new StringBuilder();
        final byte[] matrix = new byte[200000];
        if (start == 1) {
            code(in, sb, matrix);
        } else {
            decode(in, sb, matrix);
        }
        System.out.println(sb);
    }

    private static int shift(final int in) {
        return in << 1;
    }

    private static void decode(final Scanner in, final StringBuilder sb, final byte[] matrix) {
        int error = 0;
        int temp = 1, uArray = 1, uString = 0;
        final String s = in.nextLine();
        in.close();
        do {
            if (uArray == temp) {
                temp *= 2;
            }
            matrix[uArray] = Byte.parseByte(String.valueOf(s.charAt(uString)));
            uArray++;
            uString++;
        } while (uString != s.length());
        int lastPosition;
        if (uArray > temp) {
            lastPosition = temp;
        } else {
            lastPosition = temp / 2;
        }
        temp = 1;
        do {
            int sum = 0;
            for (int i = temp + 1; i < uArray; i++) {
                if ((i & temp) == temp) {
                    sum ^= matrix[i];
                }
            }
            if (sum != matrix[temp]) {
                error += temp;
            }
            temp *= 2;
        } while (temp <= lastPosition);
        if (matrix[error] == 0) {
            matrix[error] = 1;
        } else {
            matrix[error] = 0;
        }
        temp = 1;
        for (int i = 1; i < uArray; i++) {
            if (temp == i) {
                temp *= 2;
            } else {
                sb.append(matrix[i]);
            }
        }
    }

    private static void code(final Scanner in, final StringBuilder sb, final byte[] matrix) {
        int temp = 1, uArray = 1, uString = 0;
        final String s = in.nextLine();
        in.close();
        do {
            if (uArray != temp) {
                matrix[uArray] = Byte.parseByte(String.valueOf(s.charAt(uString)));
                uArray++;
                uString++;
            } else {
                temp = shift(temp);
                uArray++;
            }
        } while (uString != s.length());
        int lastPosition;
        if (uArray > temp) {
            lastPosition = temp;
        } else {
            lastPosition = temp >> 1;
        }
        temp = 1;
        do {
            int sum = 0;
            for (int i = temp; i < uArray; i++) {
                if ((i & temp) == temp) {
                    sum ^= matrix[i];
                }
            }
            matrix[temp] = (byte) sum;
            temp = shift(temp);
        } while (temp <= lastPosition);
        for (int i = 1; i < uArray; i++) {
            sb.append(matrix[i]);
        }
    }
}
