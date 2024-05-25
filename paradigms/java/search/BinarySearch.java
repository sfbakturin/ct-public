package search;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class BinarySearch {

	/**
		Pred:
			array of arguments is exists and it's a integer <=> args.length > 0 && args.length <= Integer.MAX_VALUE
			argument to find is exists and it's a integer <=> args.length > 0
			argument to find == x

		Post:
			array of arguments is exists <=> args.length > 0
			argument to find is exists <=> args.length > 0
			argument to find == x
			found index == r
			found index >= 1
			(args[r] == x || args[r] != x || args[r] == NULL) && (r >= 1 && r <= Integer.MAX_VALUE)
	*/

	public static void main(final String... args) {
		/*
		CONDITION: is there an array <=> args.length > 1
			TRUE:
				return r := foundIndex
			FALSE:
				return 0
		*/
		if (args.length > 1) {
		/*
			args.length > 1 => there is argument no. 0 => args[0] != NULL && args[0] is int
		*/
			final int x = Integer.parseInt(args[0]);
			/*
				args.length > 1 => there is array => args[0] == x => int[] a is sized [args.length - 1]
			*/
			final int[] a = new int[args.length - 1];
			/*
				args.length > 1 => there is array that sized of [args.length - 1] => there arguments that is integers
			*/
			for (int i = 0; i < args.length - 1; i++) {
				a[i] = Integer.parseInt(args[i + 1]);
			}
			/*
				there is array that filled with integers => searchBaseIterativeVersion and searchBaseRecursiveVersion can be used without errors
			*/
			//System.out.println(searchBaseIterativeVersion(x, a));
			System.out.println(searchBaseRecursiveVersion(x, a, -1, args.length - 1));
		} else {
			System.out.println(0);
		}
	}

	private static int searchBaseIterativeVersion(final int x, final int[] a) {
		/*
			let l = -1 && l in [-1, a.length)
			let r = a.length && r in (-1, a.length]
		*/
		int l = -1, r = a.length;
		/*
			I: (a[foundIndex] == x || a[foundIndex] != x) && (r - l > 1)
			let l' := l and r' := r
		*/
		while (l < r - 1) {
			/*
				r' - l' > 1 && l' in [-1, a.length) && r' in (-1, a.length] => l' < m < r' && m = (l' + r') / 2 (java's int)
			*/
			final int m = (l + r) / 2;
			/*
				CONDITION: l' < m < r' && 0 <= m <= a.length - 1 => a[m] != NULL && ? a[m] <= x
					TRUE:
						let r'' := m, so:
							r'' in (-1, r' / 2] && l' in [-1, a.length) =>  a[m'] ? exists, where m' = (l' + r'') / 2 (java's int)
							let l'' := l'
					FALSE:
						let l'' := m, so:
							r' in (-1, a.length] && l'' in [-1, l' / 2) =>  a[m'] ? exists, where m' = (l'' + r') / 2 (java's int)
							let r'' := r'
			*/
			if (a[m] <= x) {
				r = m;
			} else {
				l = m;
			}
			/*
				let l' := l'' and r' := r''
			*/
		}
		/*
			r' - l' == 1
			let r := r'
		*/
		return r;
	}

	private static int searchBaseRecursiveVersion(final int x, final int[] a, final int left, final int right) {
		/*
			let left = -1 && l in [-1, a.length)
			let right = a.length && r in (-1, a.length]
		*/
		int l = left, r = right;
		/*
			CONDITION: (l - r > 1) && (a[foundIndex] <= x || a[foundIndex] != NULL)
				TRUE:
					r - l > 1 && l in [-1, a.length) && r in (-1, a.length]
				FALSE:
					r - l == 1 && a[foundIndex := r] != NULL && a[foundIndex := r] ? == ? x
			let l' := l and r' := r
		*/
		if (l < r - 1) {
			/*
				r' - l' > 1 && l' in [-1, a.length) && r' in (-1, a.length] => l' < m < r' && m = (l' + r') / 2 (java's int)
			*/
			final int m = (l + r) / 2;
			/*
				CONDITION: l' < m < r' && 0 <= m <= a.length - 1 => a[m] != NULL && ? a[m] <= x
					TRUE:
						let r'' := m, so:
							r'' in (-1, r' / 2] && l' in [-1, a.length) =>  a[m'] ? exists, where m' = (l' + r'') / 2
							let l'' := l'
					FALSE:
						let l'' := m, so:
							r' in (-1, a.length] && l'' in [-1, l' / 2) =>  a[m'] ? exists, where m' = (l'' + r') / 2
							let r'' := r'
			*/
			if (a[m] <= x) {
				r = m;
			} else {
				l = m;
			}
			/*
				let l' := l'' and r' := r''
			*/
			return searchBaseRecursiveVersion(x, a, l, r);
		} else {
			return r;
		}
	}
}
