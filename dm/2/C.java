import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class C {
	public static void main(final String... args) {
		final Scanner in = new Scanner(System.in);
		final String unsorted = in.nextLine();
		in.close();
		final String sorted = sort(unsorted);
		final Map<Character, ArrayList<Integer>> alphabet = new LinkedHashMap<>();
		final Map<Character, Integer> pointer = new LinkedHashMap<>();
		fillAlphabet(unsorted, alphabet, pointer);
		final int[] f = new int[unsorted.length()];
		fillPointers(unsorted, sorted, alphabet, pointer, f);
		final StringBuilder answer = new StringBuilder();
		int index = 0;
		build(unsorted, f, answer, index);
		System.out.println(answer);
	}

	private static String sort(final String unsorted) {
		final char[] sorted = unsorted.toCharArray();
		Arrays.sort(sorted);
		return new String(sorted);
	}

	private static void build(final String unsorted, final int[] f, final StringBuilder answer, int index) {
		for (int i = 0; i < unsorted.length(); i++) {
			index = f[index];
			answer.append(unsorted.charAt(index));
		}
	}

	private static void fillPointers(final String unsorted, final String sorted, final Map<Character, ArrayList<Integer>> alphabet, final Map<Character, Integer> pointer, final int[] f) {
		for (int i = 0; i < unsorted.length(); i++) {
			final ArrayList<Integer> temp = alphabet.get(sorted.charAt(i));
			int j = pointer.get(sorted.charAt(i));
			f[i] = temp.get(j);
			pointer.put(sorted.charAt(i), pointer.get(sorted.charAt(i)) + 1);
		}
	}

	private static void fillAlphabet(final String unsorted, final Map<Character, ArrayList<Integer>> alphabet, final Map<Character, Integer> pointer) {
		for (int i = 0; i < unsorted.length(); i++) {
			final ArrayList<Integer> temp;
			if (alphabet.containsKey(unsorted.charAt(i))) {
				temp = alphabet.get(unsorted.charAt(i));
			} else {
				temp = new ArrayList<>();
				pointer.put(unsorted.charAt(i), 0);
			}
			temp.add(i);
			alphabet.put(unsorted.charAt(i), temp);
		}
	}
}
