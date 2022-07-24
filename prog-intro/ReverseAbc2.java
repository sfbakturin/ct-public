import java.util.Arrays;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class ReverseAbc2 {
	public static void main(final String... args) {
		final FastScanner line = new FastScanner(System.in);
		int[][] ints2d = new int[256][];
		int i = 0;
		boolean flag = false;
		while (line.hasNextLine()) {
			int j = 0;
			int[] temp = new int[256];
			while (line.isNextLine()) {
				final String temp2 = line.nextIntAbc2();
				if (temp2 == null) {
					break;
				}
				temp[j] = Integer.parseInt(temp2);
				j++;
				if (j >= temp.length) {
					temp = Arrays.copyOf(temp, temp.length * 2);
				}
			}
			if (j == 0) {
				if (!flag) {
					ints2d[i] = Arrays.copyOf(temp, j);
					i++;
					if (i >= ints2d.length) {
						ints2d = extendsArray(ints2d, ints2d.length * 2);
					}
					flag = true;
				} else {
					flag = false;
				}
			} else {
				ints2d[i] = Arrays.copyOf(temp, j);
				i++;
				if (i >= ints2d.length) {
					ints2d = extendsArray(ints2d, ints2d.length * 2);
				}
				flag = false;
			}
		}
		line.close();
		for (int m = i - 2; m >= 0; m--) {
			for (int o = ints2d[m].length - 1; o >= 0; o--) {
				System.out.print(ints2d[m][o] + " ");
			}
			System.out.println();
		}
	}

	private static int[][] extendsArray(final int[][] inArray, final int inSize) {
		final int[][] result = Arrays.copyOf(inArray, inSize);
		for (int i = inArray.length; i < inSize; i++) {
			result[i] = new int[0];
		}
		return result;
	}
}
