import java.util.Scanner;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class F {
	private static final double ZERO = 0.0;

	private static double getPrecisionOrRecall(final double left, final double right) {
		return left + right == ZERO ? ZERO : left / (left + right);
	}

	private static double getMeasure(final double precision, final double recall) {
		return precision + recall == ZERO ? ZERO : 2.0 * precision * recall / (precision + recall);
	}

	private static double getMicro(final int[] FN, final int[] TP, final int[] ms, final int s, final int k) {
		int mTP = 0;
		for (int i = 0; i < k; i++)
			mTP += ms[i] * (FN[i] + TP[i]);
		return (double) mTP / (double) s;
	}

	private static double getMacro(
			final int[] FN,
			final int[] TP,
			final int[] ms,
			final int[] ns,
			final int s,
			final int k
	) {
		double macro = ZERO;
		for (int i = 0; i < k; i++)
			macro += getPrecisionOrRecall(ms[i], ns[i]) * (FN[i] + TP[i]);
		return macro / (double) s;
	}

	private static double getF(final int[] FN, final int[] TP, final int[] FP, final int s, final int k) {
		double f = ZERO;
		for (int i = 0; i < k; i++) {
			final double precision = getPrecisionOrRecall(TP[i], FP[i]);
			final double recall = getPrecisionOrRecall(TP[i], FN[i]);
			f += getMeasure(precision, recall) * (FN[i] + TP[i]);
		}
		return f / (double) s;
	}

	public static void main(final String[] args) {
		final Scanner input = new Scanner(System.in);
		final int k = input.nextInt();
		final int[] CM = new int[k * k];
		final int[] TP = new int[k];
		final int[] FP = new int[k];
		final int[] FN = new int[k];
		int s = 0;
		for (int y = 0; y < k; y++) {
			for (int x = 0; x < k; x++) {
				final int c = input.nextInt();
				CM[k * y + x] = c;
				s += c;
			}
		}
		input.close();
		for (int y = 0; y < k; y++) {
			for (int x = 0; x < k; x++) {
				if (y != x) {
					FN[y] += CM[y * k + x];
					FP[y] += CM[x * k + y];
				}
			}
			TP[y] = CM[y * k + y];
		}
		final double microTP = getMicro(FN, TP, TP, s, k);
		final double microFP = getMicro(FN, TP, FP, s, k);
		final double microFN = getMicro(FN, TP, FN, s, k);
		final double macroPrecision = getMacro(FN, TP, TP, FP, s, k);
		final double macroRecall = getMacro(FN, TP, TP, FN, s, k);
		final double F = getF(FN, TP, FP, s, k);
		final double microPrecision = getPrecisionOrRecall(microTP, microFP);
		final double microRecall = getPrecisionOrRecall(microTP, microFN);
		final double microF = getMeasure(microPrecision, microRecall);
		final double macroF = getMeasure(macroPrecision, macroRecall);
		System.out.println(microF);
		System.out.println(macroF);
		System.out.println(F);
	}
}
