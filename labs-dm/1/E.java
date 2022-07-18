import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class E {
    public static void main(final String[] www) {
        final Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        in.nextLine();

        final int[] func = new int[(int) Math.pow(2, n)];
        final String[] args = new String[(int) Math.pow(2, n)];
        final int[] poly = new int[(int) Math.pow(2, n)];

        for (int i = 0; i < ((int) Math.pow(2, n)); i++) {
            String s = in.nextLine();
            args[i] = s.substring(0, n);
            func[i] = Integer.parseInt(String.valueOf(s.charAt(s.length() - 1)));
        }

        in.close();

        for (int i = 0; i < ((int) Math.pow(2, n)); i++) {
            String s1 = args[i];
            poly[i] = func[i];
            for (int j = i - 1; j >= 0; j--) {
                String s2 = args[j];
                boolean flag = true;
                for (int q = 0; q < s1.length(); q++) {
                    if (s1.charAt(q) < s2.charAt(q)) {
                        flag = false;
                        break;
                    }
                }
                if (flag) {
                    poly[i] = poly[i] ^ func[j];
                }
            }
        }

        for (int i = 0; i < (int) Math.pow(2, n); i++) {
            System.out.println(args[i] + " " + poly[i]);
        }
    }
}
