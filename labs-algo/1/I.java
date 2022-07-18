import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.Stack;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class I {
    public static void main(final String... args) {
        final Scanner in = new Scanner(System.in);
        final int n = in.nextInt();
        in.nextLine();
        final int[] a = new int[n];
        for (int i = 0; i < n; i++) {
            a[i] = in.nextInt();
        }
        in.close();
        final Stack<Integer> sort = new Stack<>();
        final List<String> answer = new ArrayList<>();
        int current = 1;
        for (int i = 0; i < n; i++) {
            if (!sort.empty()) {
                while (sort.peek() == current) {
                    sort.pop();
                    answer.add("2 1");
                    current++;
                    if (sort.empty()) {
                        break;
                    }
                }
            }
            sort.push(a[i]);
            answer.add("1 1");
            if (!sort.empty()) {
                while (sort.peek() == current) {
                    sort.pop();
                    answer.add("2 1");
                    current++;
                    if (sort.empty()) {
                        break;
                    }
                }
            }
        }
        if (sort.empty()) {
            for (final String integer : answer) {
                System.out.println(integer);
            }
        } else {
            System.out.println(0);
        }
    }
}
