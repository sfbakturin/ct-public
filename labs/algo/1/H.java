import java.util.Scanner;
import java.util.Stack;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class H {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final char[] s = in.nextLine().toCharArray();
		in.close();
		final Stack<Character> stack = new Stack<>();
		boolean flag = true;
		for (final char c : s) {
			switch (c) {
				case '{':
				case '[':
				case '(':
					stack.push(c);
					break;
				case ')':
					if (!stack.empty()) {
						if (stack.peek() == '(') {
							stack.pop();
						} else {
							flag = false;
						}
					} else {
						flag = false;
					}
					break;
				case ']':
					if (!stack.empty()) {
						if (stack.peek() == '[') {
							stack.pop();
						} else {
							flag = false;
						}
					} else {
						flag = false;
					}
					break;
				case '}':
					if (!stack.empty()) {
						if (stack.peek() == '{') {
							stack.pop();
						} else {
							flag = false;
						}
					} else {
						flag = false;
					}
					break;
			}
			if (!flag) {
				break;
			}
		}
		System.out.println(flag && stack.empty() ? "YES" : "NO");
	}
}
