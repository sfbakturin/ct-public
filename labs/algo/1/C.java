import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.StringTokenizer;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class C {
    public static long merge(final int[] s, final int[] a, final int[] b) {
        int i = 0, j = 0;
        long count = 0;
        while (i < a.length || j < b.length) {
            if (i == a.length) {
                s[i + j] = b[j];
                j++;
            } else if (j == b.length) {
                s[i + j] = a[i];
                i++;
            } else if (a[i] <= b[j]) {
                s[i + j] = a[i];
                i++;
            } else {
                s[i + j] = b[j];
                count += (a.length - i);
                j++;
            }
        }
        return count;
    }

    public static long mergesort(final int[] s) {
        if (s.length < 2) {
            return 0;
        }

        final int mid = (s.length + 1) / 2;
        final int[] a = Arrays.copyOfRange(s, 0, mid);
        final int[] b = Arrays.copyOfRange(s, mid, s.length);

        return mergesort(a) + mergesort(b) + merge(s, a, b);
    }

    public static void main(final String... args) {
        final FastScanner in = new FastScanner(System.in);
        final int n = in.nextInt();
        int[] a = new int[n];
        for (int i = 0; i < n; i++) {
            a[i] = in.nextInt();
        }
        in.close();
        System.out.println(mergesort(a));
    }

    static class FastScanner {
        private final BufferedReader in;
        private StringTokenizer st;
        private boolean closed;

        public FastScanner(final InputStream i) {
            this.in = new BufferedReader(new InputStreamReader(i));
            this.closed = false;
        }

        public String next() {
            if (this.closed) {
                throw new IllegalStateException("FastScanner is closed");
            }

            while (this.st == null || !this.st.hasMoreTokens()) {
                try {
                    this.st = new StringTokenizer(this.in.readLine());
                } catch (final IOException err) {
                    err.printStackTrace();
                }
            }
            return this.st.nextToken();
        }

        public int nextInt() {
            return Integer.parseInt(this.next());
        }

        public void close() {
            if (closed) {
                return;
            } else {
                this.closed = true;
                try {
                    this.in.close();
                } catch (final IOException err) {
                    err.printStackTrace();
                }
            }
        }
    }
}
