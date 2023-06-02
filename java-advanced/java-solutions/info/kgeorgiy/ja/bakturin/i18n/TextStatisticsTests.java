package info.kgeorgiy.ja.bakturin.i18n;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.nio.file.Path;
import java.text.DateFormat;
import java.text.ParsePosition;
import java.util.Locale;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

@RunWith(JUnit4.class)
@SuppressWarnings("all")
public class TextStatisticsTests extends TextStatisticsTester {
	private static final Locale ENGLISH = Locale.forLanguageTag("en-US");
	private static final Locale RUSSIAN = Locale.forLanguageTag("ru-RU");

	private static Path getInPath(final String arg) {
		return getPath("samples", arg);
	}

	private static Path getOutPath() {
		return getPath("samples", "test.out");
	}

	@Test
	public void test1_sentences_english_english() {
		final Path pI = getInPath("test1.in");
		final Path pO = getOutPath();
		test1(ENGLISH, pI, pO);
	}

	@Test
	public void test1_sentences_english_russian() {
		final Path pI = getInPath("test1.in");
		final Path pO = getOutPath();
		test1(RUSSIAN, pI, pO);
	}

	@Test
	public void test2_sentences_russian_english() {
		final Path pI = getInPath("test2.in");
		final Path pO = getOutPath();
		test2(ENGLISH, pI, pO);
	}

	@Test
	public void test2_sentences_russian_russian() {
		final Path pI = getInPath("test2.in");
		final Path pO = getOutPath();
		test2(RUSSIAN, pI, pO);
	}

	@Test
	public void test3_numbers_english_english() {
		final Path pI = getInPath("test3.in");
		final Path pO = getOutPath();
		test3(ENGLISH, pI, pO);
	}

	@Test
	public void test3_numbers_english_russian() {
		final Path pI = getInPath("test3.in");
		final Path pO = getOutPath();
		test3(RUSSIAN, pI, pO);
	}

	@Test
	public void test4_numbers_russian_english() {
		final Path pI = getInPath("test4.in");
		final Path pO = getOutPath();
		test4(ENGLISH, pI, pO);
	}

	@Test
	public void test4_numbers_russian_russian() {
		final Path pI = getInPath("test4.in");
		final Path pO = getOutPath();
		test4(RUSSIAN, pI, pO);
	}

	@Test
	public void test5_money_english_english() {
		final Path pI = getInPath("test5.in");
		final Path pO = getOutPath();
		test5(ENGLISH, pI, pO);
	}

	@Test
	public void test5_money_english_russian() {
		final Path pI = getInPath("test5.in");
		final Path pO = getOutPath();
		test5(RUSSIAN, pI, pO);
	}

	@Test
	public void test6_money_russian_english() {
		final Path pI = getInPath("test6.in");
		final Path pO = getOutPath();
		test6(Locale.forLanguageTag("ru-RU-#Cyrl"), ENGLISH, pI, pO);
	}

	@Test
	public void test6_money_russian_russian() {
		final Path pI = getInPath("test6.in");
		final Path pO = getOutPath();
		test6(Locale.forLanguageTag("ru-RU-#Cyrl"), RUSSIAN, pI, pO);
	}

	@Test
	public void test7_dates_english_english() {
		final Path pI = getInPath("test7.in");
		final Path pO = getOutPath();
		test7(ENGLISH, pI, pO);
	}

	@Test
	public void test7_dates_english_russian() {
		final Path pI = getInPath("test7.in");
		final Path pO = getOutPath();
		test7(RUSSIAN, pI, pO);
	}

	@Test
	public void test8_dates_russian_english() {
		final Path pI = getInPath("test8.in");
		final Path pO = getOutPath();
		test8(Locale.forLanguageTag("ru-RU-#Cyrl"), ENGLISH, pI, pO);
	}

	@Test
	public void test8_dates_russian_russian() {
		final Path pI = getInPath("test8.in");
		final Path pO = getOutPath();
		test8(Locale.forLanguageTag("ru-RU-#Cyrl"), RUSSIAN, pI, pO);
	}

	private static void test1(final Locale lO, final Path pI, final Path pO) {
		runMain(TextStatisticsTests.ENGLISH, lO, pI, pO);
		final StringSolution sentences = new StringSolutionImpl(
				3,
				3,
				"And today he was great!",
				"This sentence is a metaphor.",
				"And today he was great!",
				"The company on a strange day earned a lot.",
				31.0
		);
		final StringSolution words = new StringSolutionImpl(
				19,
				17,
				"a",
				"was",
				"a",
				"metaphor",
				3.8947368421052633
		);
		final NumberSolution numbers = getNumberDefault();
		final NumberSolution money = getNumberDefault();
		final NumberSolution dates = getNumberDefault();
		final String solution = writeSolution(pI.toString(), lO, sentences, words, numbers, money, dates);
		checkSolution(solution, pO);
	}

	private static void test2(final Locale lO, final Path pI, final Path pO) {
		runMain(TextStatisticsTests.RUSSIAN, lO, pI, pO);
		final StringSolution sentences = new StringSolutionImpl(
				3,
				3,
				"\u0410\u0020\u0441\u0435\u0433\u043e\u0434\u043d\u044f\u0020\u0431\u044b\u043b\u0020\u043c\u043e\u043b\u043e\u0434\u0446\u043e\u043c\u0021",
				"\u042d\u0442\u043e\u0020\u043f\u0440\u0435\u0434\u043b\u043e\u0436\u0435\u043d\u0438\u0435\u0020\u002d\u0020\u043c\u0435\u0442\u0430\u0444\u043e\u0440\u0430\u002e",
				"\u0410\u0020\u0441\u0435\u0433\u043e\u0434\u043d\u044f\u0020\u0431\u044b\u043b\u0020\u043c\u043e\u043b\u043e\u0434\u0446\u043e\u043c\u0021",
				"\u041a\u043e\u043c\u043f\u0430\u043d\u0438\u044f\u0020\u043d\u0430\u0020\u0441\u0442\u0440\u0430\u043d\u043d\u044b\u0439\u0020\u0434\u0435\u043d\u044c\u0020\u0437\u0430\u0440\u0430\u0431\u043e\u0442\u0430\u043b\u0430\u0020\u043e\u0447\u0435\u043d\u044c\u0020\u043c\u043d\u043e\u0433\u043e\u002e",
				33.0
		);
		final StringSolution words = new StringSolutionImpl(
				14,
				14,
				"\u0410",
				"\u042d\u0442\u043e",
				"\u0410",
				"\u043f\u0440\u0435\u0434\u043b\u043e\u0436\u0435\u043d\u0438\u0435",
				5.928571428571429
		);
		final NumberSolution numbers = getNumberDefault();
		final NumberSolution money = getNumberDefault();
		final NumberSolution dates = getNumberDefault();
		final String solution = writeSolution(pI.toString(), lO, sentences, words, numbers, money, dates);
		checkSolution(solution, pO);
	}

	private static void test3(final Locale lO, final Path pI, final Path pO) {
		runMain(TextStatisticsTests.ENGLISH, lO, pI, pO);
		final StringSolution sentences = new StringSolutionImpl(
				12,
				12,
				"7 minute for lunch break and a 4.3 minute for coffee break timed precisely by his wristwatch.",
				"This is a story about a man named Harold Crick.",
				"And his wristwatch.",
				"Every weekday, for 12 years Harold would tie his tie in a single Windsor knot instead of the double thereby saving up to 43 seconds.",
				79.0
		);
		final StringSolution words = new StringSolutionImpl(
				158,
				91,
				"a",
				"years",
				"a",
				"calculations",
				4.632911392405063
		);
		final NumberSolution numbers = new NumberSolutionImpl(
				instanceNumber(TextStatisticsTests.ENGLISH),
				15,
				12,
				32,
				7134,
				647.553
		);
		final NumberSolution money = getNumberDefault();
		final NumberSolution dates = getNumberDefault();
		final String solution = writeSolution(pI.toString(), lO, sentences, words, numbers, money, dates);
		checkSolution(solution, pO);
	}

	private static void test4(final Locale lO, final Path pI, final Path pO) {
		runMain(TextStatisticsTests.RUSSIAN, lO, pI, pO);
		final StringSolution sentences = new StringSolutionImpl(
				12,
				12,
				"\u0037\u0020\u043c\u0438\u043d\u0443\u0442\u0020\u043d\u0430\u0020\u043e\u0431\u0435\u0434\u0435\u043d\u043d\u044b\u0439\u0020\u043f\u0435\u0440\u0435\u0440\u044b\u0432\u0020\u0438\u0020\u0034\u002c\u0033\u0020\u043c\u0438\u043d\u0443\u0442\u044b\u0020\u043d\u0430\u0020\u043a\u043e\u0444\u0435\u002d\u0431\u0440\u0435\u0439\u043a\u002c\u0020\u0442\u043e\u0447\u043d\u043e\u0020\u0440\u0430\u0441\u0441\u0447\u0438\u0442\u0430\u043d\u043d\u044b\u0435\u0020\u0435\u0433\u043e\u0020\u043d\u0430\u0440\u0443\u0447\u043d\u044b\u043c\u0438\u0020\u0447\u0430\u0441\u0430\u043c\u0438\u002e",
				"\u042d\u0442\u043e\u0020\u0438\u0441\u0442\u043e\u0440\u0438\u044f\u0020\u043e\u0020\u0447\u0435\u043b\u043e\u0432\u0435\u043a\u0435\u0020\u043f\u043e\u0020\u0438\u043c\u0435\u043d\u0438\u0020\u0413\u0430\u0440\u043e\u043b\u044c\u0434\u0020\u041a\u0440\u0438\u043a\u002e",
				"\u0418\u0020\u0435\u0433\u043e\u0020\u043d\u0430\u0440\u0443\u0447\u043d\u044b\u0435\u0020\u0447\u0430\u0441\u044b\u002e",
				"\u041a\u0430\u0436\u0434\u044b\u0439\u0020\u0431\u0443\u0434\u043d\u0438\u0439\u0020\u0434\u0435\u043d\u044c\u0020\u0432\u0020\u0442\u0435\u0447\u0435\u043d\u0438\u0435\u0020\u0031\u0032\u0020\u043b\u0435\u0442\u0020\u0413\u0430\u0440\u043e\u043b\u044c\u0434\u0020\u0437\u0430\u0432\u044f\u0437\u044b\u0432\u0430\u043b\u0020\u0441\u0432\u043e\u0439\u0020\u0433\u0430\u043b\u0441\u0442\u0443\u043a\u0020\u043e\u0434\u043d\u0438\u043c\u0020\u0432\u0438\u043d\u0434\u0437\u043e\u0440\u0441\u043a\u0438\u043c\u0020\u0443\u0437\u043b\u043e\u043c\u0020\u0432\u043c\u0435\u0441\u0442\u043e\u0020\u0434\u0432\u043e\u0439\u043d\u043e\u0433\u043e\u002c\u0020\u0442\u0435\u043c\u0020\u0441\u0430\u043c\u044b\u043c\u0020\u044d\u043a\u043e\u043d\u043e\u043c\u044f\u0020\u0434\u043e\u0020\u0034\u0033\u0020\u0441\u0435\u043a\u0443\u043d\u0434\u002e",
				84.58333333333333
		);
		final StringSolution words = new StringSolutionImpl(
				149,
				103,
				"\u0410",
				"\u042d\u0442\u043e",
				"\u043e",
				"\u043d\u0435\u043c\u043d\u043e\u0433\u043e\u0441\u043b\u043e\u0432\u0435\u043d",
				5.402684563758389
		);
		final NumberSolution numbers = new NumberSolutionImpl(
				instanceNumber(TextStatisticsTests.RUSSIAN),
				15,
				12,
				32,
				7134,
				647.553
		);
		final NumberSolution money = getNumberDefault();
		final NumberSolution dates = getNumberDefault();
		final String solution = writeSolution(pI.toString(), lO, sentences, words, numbers, money, dates);
		checkSolution(solution, pO);
	}

	private static void test5(final Locale lO, final Path pI, final Path pO) {
		runMain(TextStatisticsTests.ENGLISH, lO, pI, pO);
		final StringSolution sentences = new StringSolutionImpl(
				9,
				9,
				"A number 6 with extra dip.",
				"Two number 45s, one with cheese and a large soda.",
				"A number 9 large.",
				"Two number 45s, one with cheese and a large soda.",
				25.666666666666668
		);
		final StringSolution words = new StringSolutionImpl(
				40,
				20,
				"45s",
				"with",
				"s",
				"cheese",
				3.6
		);
		final NumberSolution numbers = new NumberSolutionImpl(
				instanceNumber(TextStatisticsTests.ENGLISH),
				5,
				4,
				6,
				45,
				15.2
		);
		final NumberSolution money = new NumberSolutionImpl(
				instanceMoney(TextStatisticsTests.ENGLISH),
				5,
				5,
				91.10,
				24.98,
				26.59
		);
		final NumberSolution dates = getNumberDefault();
		final String solution = writeSolution(pI.toString(), lO, sentences, words, numbers, money, dates);
		checkSolution(solution, pO);
	}

	private static void test6(final Locale lI, final Locale lO, final Path pI, final Path pO) {
		runMain(lI, lO, pI, pO);
		final StringSolution sentences = new StringSolutionImpl(
				10,
				10,
				"\u0414\u0432\u0430\u0020\u043d\u043e\u043c\u0435\u0440\u0430\u0020\u0034\u0035\u002c\u0020\u043e\u0434\u0438\u043d\u0020\u0441\u0020\u0441\u044b\u0440\u043e\u043c\u0020\u0438\u0020\u0431\u043e\u043b\u044c\u0448\u0430\u044f\u0020\u0433\u0430\u0437\u0438\u0440\u043e\u0432\u043a\u0430\u002e",
				"\u042d\u0442\u043e\u0020\u0431\u0443\u0434\u0435\u0442\u0020\u0441\u0442\u043e\u0438\u0442\u044c\u0020\u0037\u0039\u002c\u0031\u0032\u00a0\u20bd\u002e",
				"\u041d\u043e\u043c\u0435\u0440\u0020\u0037\u002e",
				"\u0414\u0432\u0430\u0020\u043d\u043e\u043c\u0435\u0440\u0430\u0020\u0034\u0035\u002c\u0020\u043e\u0434\u0438\u043d\u0020\u0441\u0020\u0441\u044b\u0440\u043e\u043c\u0020\u0438\u0020\u0431\u043e\u043b\u044c\u0448\u0430\u044f\u0020\u0433\u0430\u0437\u0438\u0440\u043e\u0432\u043a\u0430\u002e",
				27.3
		);
		final StringSolution words = new StringSolutionImpl(
				36,
				20,
				"\u0431\u043e\u043b\u044c\u0448\u0430\u044f",
				"\u042d\u0442\u043e",
				"\u0441",
				"\u0434\u043e\u043f\u043e\u043b\u043d\u0438\u0442\u0435\u043b\u044c\u043d\u044b\u043c",
				4.944444444444445
		);
		final NumberSolution numbers = new NumberSolutionImpl(
				instanceNumber(lI),
				5,
				4,
				6,
				45,
				15.2
		);
		final NumberSolution money = new NumberSolutionImpl(
				instanceMoney(lI),
				5,
				5,
				321.32,
				79.12,
				245.84
		);
		final NumberSolution dates = getNumberDefault();
		final String solution = writeSolution(pI.toString(), lO, sentences, words, numbers, money, dates);
		checkSolution(solution, pO);
	}

	private static void test7(final Locale lO, final Path pI, final Path pO) {
		runMain(TextStatisticsTests.ENGLISH, lO, pI, pO);
		final StringSolution sentences = new StringSolutionImpl(
				4,
				4,
				"And also be a baron forever December 31, 2021.",
				"Thanks to short date, for example, 10/10/10, for their existence.",
				"And also be a baron forever December 31, 2021.",
				"I want to be free like a long venomous snake at night of September 13, 2000.",
				61.0
		);
		final StringSolution words = new StringSolutionImpl(
				40,
				33,
				"a",
				"Wednesday",
				"a",
				"existence",
				4.1
		);
		final NumberSolution numbers = getNumberDefault();
		final NumberSolution money = getNumberDefault();
		final NumberSolution dates = new NumberSolutionImpl(
				instanceDate(lO),
				4,
				4,
				DateFormat.getDateInstance(DateFormat.DEFAULT, TextStatisticsTests.ENGLISH).parse("December 31, 2021", new ParsePosition(0)).getTime(),
				DateFormat.getDateInstance(DateFormat.DEFAULT, TextStatisticsTests.ENGLISH).parse("October 10, 2010", new ParsePosition(0)).getTime(),
				DateFormat.getDateInstance(DateFormat.DEFAULT, TextStatisticsTests.ENGLISH).parse("March 22, 2014", new ParsePosition(0)).getTime()
		);
		final String solution = writeSolution(pI.toString(), lO, sentences, words, numbers, money, dates);
		checkSolution(solution, pO);
	}

	private static void test8(final Locale lI, final Locale lO, final Path pI, final Path pO) {
		runMain(lI, lO, pI, pO);
		final StringSolution sentences = new StringSolutionImpl(
				4,
				4,
				"\u0410\u0020\u0442\u0430\u043a\u0436\u0435\u0020\u0431\u044b\u0442\u044c\u0020\u0431\u0430\u0440\u043e\u043d\u043e\u043c\u0020\u043d\u0430\u0432\u0441\u0435\u0433\u0434\u0430\u0020\u0033\u0031\u0020\u0434\u0435\u043a\u0430\u0431\u0440\u044f\u0020\u0032\u0030\u0032\u0031\u0020\u0433\u002e\u002e",
				"\u042f\u0020\u0445\u043e\u0447\u0443\u0020\u0431\u044b\u0442\u044c\u0020\u0441\u0432\u043e\u0431\u043e\u0434\u043d\u044b\u043c\u002c\u0020\u043a\u0430\u043a\u0020\u0434\u043b\u0438\u043d\u043d\u0430\u044f\u0020\u044f\u0434\u043e\u0432\u0438\u0442\u0430\u044f\u0020\u0437\u043c\u0435\u044f\u0020\u043d\u043e\u0447\u044c\u044e\u0020\u0031\u0033\u0020\u0441\u0435\u043d\u0442\u044f\u0431\u0440\u044f\u0020\u0032\u0030\u0030\u0030\u0020\u0433\u002e\u002e",
				"\u0410\u0020\u0442\u0430\u043a\u0436\u0435\u0020\u0431\u044b\u0442\u044c\u0020\u0431\u0430\u0440\u043e\u043d\u043e\u043c\u0020\u043d\u0430\u0432\u0441\u0435\u0433\u0434\u0430\u0020\u0033\u0031\u0020\u0434\u0435\u043a\u0430\u0431\u0440\u044f\u0020\u0032\u0030\u0032\u0031\u0020\u0433\u002e\u002e",
				"\u042f\u0020\u0445\u043e\u0447\u0443\u0020\u0431\u044b\u0442\u044c\u0020\u0441\u0432\u043e\u0431\u043e\u0434\u043d\u044b\u043c\u002c\u0020\u043a\u0430\u043a\u0020\u0434\u043b\u0438\u043d\u043d\u0430\u044f\u0020\u044f\u0434\u043e\u0432\u0438\u0442\u0430\u044f\u0020\u0437\u043c\u0435\u044f\u0020\u043d\u043e\u0447\u044c\u044e\u0020\u0031\u0033\u0020\u0441\u0435\u043d\u0442\u044f\u0431\u0440\u044f\u0020\u0032\u0030\u0030\u0030\u0020\u0433\u002e\u002e",
				63.0
		);
		final StringSolution words = new StringSolutionImpl(
				36,
				32,
				"\u0410",
				"\u044f\u0441\u043d\u044b\u0439",
				"\u0432",
				"\u0441\u0443\u0449\u0435\u0441\u0442\u0432\u043e\u0432\u0430\u043d\u0438\u0435",
				4.805555555555555
		);
		final NumberSolution numbers = getNumberDefault();
		final NumberSolution money = getNumberDefault();
		final NumberSolution dates = new NumberSolutionImpl(
				instanceDate(lO),
				4,
				4,
				DateFormat.getDateInstance(DateFormat.DEFAULT, lI).parse("31 дек. 2021 г.", new ParsePosition(0)).getTime(),
				DateFormat.getDateInstance(DateFormat.DEFAULT, lI).parse("10 окт. 2010 г.", new ParsePosition(0)).getTime(),
				DateFormat.getDateInstance(DateFormat.DEFAULT, lI).parse("22 мар. 2014 г.", new ParsePosition(0)).getTime()
		);
		final String solution = writeSolution(pI.toString(), lO, sentences, words, numbers, money, dates);
		checkSolution(solution, pO);
	}
}
