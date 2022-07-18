import java.util.ArrayList;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class D {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt();
        in.nextLine();

        final int[][] a0 = new int[(int) Math.pow(2, n)][n];
        final int[][] a1 = new int[(int) Math.pow(2, n)][n];
        final int[][] x0 = new int[(int) Math.pow(2, n)][n];
        final int[][] x1 = new int[(int) Math.pow(2, n)][n];
        final ArrayList<String> ans = new ArrayList<>();
        int k0, k1, count, curr;
        k0 = 0;
        k1 = 0;
        count = 0;
        curr = n;

        for (int i = 0; i < (int) Math.pow(2, n); i++) {
            final String[] s = in.nextLine().split(" ");
            final int f = Integer.parseInt(s[1]);
            if (f == 0) {
                k0++;
            } else {
                k1++;
            }
            for (int j = 0; j < n; j++) {
                if (f == 0) {
                    x0[k0 - 1][j] = Integer.parseInt(String.valueOf(s[0].charAt(j)));
                    a0[k0 - 1][j] = (j + 1);
                } else {
                    x1[k1 - 1][j] = Integer.parseInt(String.valueOf(s[0].charAt(j)));
                    a1[k1 - 1][j] = (j + 1);
                }
            }
        }

        in.close();

        curr++;

        count = getCount(n, a0, a1, x0, x1, ans, k0, k1, count, curr);

        System.out.println(count + n);

        for (int i = 0; i < count; i++) {
            System.out.println(ans.get(i));
        }

    }

    private static int getCount(final int n, final int[][] a0, final int[][] a1, final int[][] x0, final int[][] x1, final ArrayList<String> ans, final int k0, final int k1, int count, int curr) {
        if (k1 != 0) {
            for (int i = 0; i < k1; i++) {
                for (int j = 1; j < n; j++) {
                    if (x1[i][j - 1] == 0) {
                        ans.add("1" + " " + a1[i][j - 1]);
                        a1[i][j - 1] = curr;
                        curr++;
                        count++;
                        x1[i][j - 1] = -1;
                    }
                    if (x1[i][j] == 0) {
                        ans.add("1" + " " + a1[i][j]);
                        a1[i][j] = curr;
                        curr++;
                        count++;
                        x1[i][j] = -1;
                    }
                    ans.add("2" + " " + a1[i][j - 1] + " " + a1[i][j]);
                    a1[i][j - 1] = -1;
                    a1[i][j] = curr;
                    curr++;
                    count++;
                }
            }
            for (int i = 1; i < k1; i++) {
                ans.add("3" + " " + a1[i - 1][n - 1] + " " + a1[i][n - 1]);
                a1[i - 1][n - 1] = -1;
                a1[i][n - 1] = curr;
                curr++;
                count++;
            }
        } else {
            for (int i = 0; i < k0; i++) {
                for (int j = 1; j < n; j++) {
                    if (x0[i][j - 1] == 1) {
                        ans.add("1" + " " + a0[i][j - 1]);
                        a0[i][j - 1] = curr;
                        curr++;
                        count++;
                        x0[i][j - 1] = -1;
                    }
                    if (x0[i][j] == 1) {
                        ans.add("1" + " " + a0[i][j]);
                        a0[i][j] = curr;
                        curr++;
                        count++;
                        x0[i][j] = -1;
                    }
                    ans.add("3" + " " + a0[i][j - 1] + " " + a0[i][j]);
                    a0[i][j - 1] = -1;
                    a0[i][j] = curr;
                    curr++;
                    count++;
                }
            }
            for (int i = 1; i < k0; i++) {
                ans.add("2" + " " + a0[i - 1][n - 1] + " " + a0[i][n - 1]);
                a0[i - 1][n - 1] = -1;
                a0[i][n - 1] = curr;
                curr++;
                count++;
            }
        }
        return count;
    }
}
