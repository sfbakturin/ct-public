import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class F {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);

        final int n = in.nextInt();
        final int k = in.nextInt();

        in.nextLine();

        final int[][] x = new int[k][n];
        final int[] count = new int[k];
        final int[] single = new int[k];
        boolean alone = false;

        for (int i = 0; i < k; i++) {
            String[] s = in.nextLine().split(" ");
            int temp1, temp2;
            temp1 = 0;
            temp2 = -1;
            for (int j = 0; j < n; j++) {
                x[i][j] = Integer.parseInt(s[j]);
                if ((x[i][j] == 0) || (x[i][j] == 1)) {
                    temp1++;
                    temp2 = j;
                }
            }
            if (temp1 == 1) {
                alone = true;
                single[i] = temp2;
            }
            count[i] = temp1;
        }

        in.close();

        boolean flag = true;

        if (!alone) {
            flag = false;
        } else {
            boolean loop = true;
            boolean changed;
            boolean last = false;

            while (loop) {
                changed = false;
                for (int u = 0; u < k; u++) {
                    if (count[u] == 1) {
                        int ch = x[u][single[u]];

                        changed = isChangedCheck1(n, k, x, count, single, changed, u, ch);

                        changed = isChangedCheck2(n, k, x, count, single, changed, u, ch);

                        for (int i = 0; i < k; i++) {
                            count[i] = 0;
                            single[i] = 0;
                        }

                        int temp = 0;

                        for (int i = 0; i < k; i++) {
                            for (int j = 0; j < n; j++) {
                                if ((x[i][j] == 0) || (x[i][j] == 1)) {
                                    count[i]++;
                                    temp = j;
                                }
                            }
                            if (count[i] == 1) {
                                single[i] = temp;
                            }
                        }
                    }
                }

                if ((!changed) && (!last)) {
                    loop = false;
                }

                last = changed;
            }

            for (int i = 0; i < k; i++) {
                int temp = 0;
                for (int j = 0; j < n; j++) {
                    if ((x[i][j] == 0) || (x[i][j] == 1)) {
                        temp++;
                        if (temp > 1) {
                            break;
                        }
                    }
                }
                if (temp > 1) {
                    flag = false;
                    break;
                }
            }

            if (flag) {
                flag = false;
                for (int i = 0; i < n; i++) {
                    int k0, k1;
                    k0 = 0;
                    k1 = 0;
                    for (int j = 0; j < k; j++) {
                        if (x[j][i] == 0) {
                            k0++;
                        }
                        if (x[j][i] == 1) {
                            k1++;
                        }
                    }
                    if ((k0 > 0) && (k1 > 0)) {
                        flag = true;
                        break;
                    }
                }
            }
        }

        if (flag) {
            System.out.println("YES");
        } else {
            System.out.println("NO");
        }
    }

    private static boolean isChangedCheck2(final int n, final int k, final int[][] x, final int[] count, final int[] single, boolean changed, final int u, final int ch) {
        if (ch == 1) {
            for (int i = 0; i < k; i++) {
                if ((x[i][single[u]] == 0) && (count[i] != 1)) {
                    x[i][single[u]] = -1;
                    count[i]--;
                    changed = true;
                } else {
                    if ((x[i][single[u]] == 1) && (count[i] != 1)) {
                        for (int j = 0; j < n; j++) {
                            x[i][j] = -1;
                        }
                        count[i] = 0;
                        changed = true;
                    }
                }
            }
        }
        return changed;
    }

    private static boolean isChangedCheck1(final int n, final int k, final int[][] x, final int[] count, final int[] single, boolean changed, final int u, final int ch) {
        if (ch == 0) {
            for (int i = 0; i < k; i++) {
                if ((x[i][single[u]] == 0) && (count[i] != 1)) {
                    for (int j = 0; j < n; j++) {
                        x[i][j] = -1;
                    }
                    count[i] = 0;
                    changed = true;
                } else {
                    if ((x[i][single[u]] == 1) && (count[i] != 1)) {
                        x[i][single[u]] = -1;
                        count[i]--;
                        changed = true;
                    }
                }
            }
        }
        return changed;
    }
}
