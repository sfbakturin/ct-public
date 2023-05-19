package info.kgeorgiy.ja.bakturin.bank;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.net.MalformedURLException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Phaser;
import java.util.stream.IntStream;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

@RunWith(JUnit4.class)
public class BankTests extends BankTester {
	private final static Random RANDOMIZER = new Random();
	private final static int PORT_RMI = 8088;
	private final static int PORT_SERVER = 8888;
	private final static String NAME = "//localhost/bank";
	private final static int TEST_ACCOUNT_COUNT = 100;
	private final static int TEST_PERSON_COUNT = 20;
//	private final static int TEST_DAMAGE_MAXIMUM_VALUE = 1024;
//	private final static int TEST_DAMAGE_BREAK_POINT = TEST_DAMAGE_MAXIMUM_VALUE / 2;
	private final static String[] DOSSIER = new String[]{
			"Saveliy", "Bakturin", "sfbakturin"
	};

	public BankTests() {
		super(PORT_RMI, PORT_SERVER, NAME);
	}

	@BeforeClass
	public static void beforeClass() {
		try {
			killRMI();
			killServer(NAME);
		} catch (final RemoteException | MalformedURLException | NotBoundException ignored) {
		}
	}

	@Test
	public void test1_basicManipulationsWithAccounts() throws RemoteException {
		final Bank expectedBank = new BankChecker.TestBank();
		final Bank actualBank = getVirtualClient(NAME);
		final List<String> ids = new ArrayList<>();
		for (int i = 0; i < TEST_ACCOUNT_COUNT; i++) {
			final String id = generateFullId(i);
			ids.add(id);
			try {
				BankChecker.equalsNull(actualBank.getAccount(id));
				BankChecker.equals(expectedBank.createAccount(id), actualBank.createAccount(id));
				final int nextInt = RANDOMIZER.nextInt();
				final boolean isNegate = RANDOMIZER.nextBoolean();
				expectedBank.getAccount(id).setAmount(isNegate ? -nextInt : nextInt);
				actualBank.getAccount(id).setAmount(isNegate ? -nextInt : nextInt);
			} catch (final RemoteException e) {
				throw new RuntimeException(e);
			}
		}
		BankChecker.equalsBanks(actualBank, expectedBank, ids);
	}

	@Test
	public void test2_creatingNewPersons() throws RemoteException {
		final Bank expectedBank = new BankChecker.TestBank();
		final Bank actualBank = getVirtualClient(NAME);
		final List<String> ids = new ArrayList<>();
		for (int i = 0; i < TEST_PERSON_COUNT; i++) {
			final String id = "john-doe-" + i;
			ids.add(id);
			try {
				BankChecker.equalsNull(actualBank.getPersonRemote(id));
				expectedBank.createPerson("John", "Doe", id);
				actualBank.createPerson("John", "Doe", id);
			} catch (final RemoteException e) {
				throw new RuntimeException(e);
			}
		}
		BankChecker.equalsBanks(actualBank, expectedBank, ids);
	}

	@Test
	public void test3_personsAndAccounts() throws RemoteException {
		final Bank expectedBank = new BankChecker.TestBank();
		final Bank actualBank = getVirtualClient(NAME);
		final List<String> ids = new ArrayList<>();
		randomActions(expectedBank, actualBank);
		BankChecker.equalsBanks(actualBank, expectedBank, ids);
	}

	@Test
	public void test4_remoteVsLocal() {
		final Bank expectedBank = new BankChecker.TestBank();
		final Bank actualBank = getVirtualClient(NAME);
		randomActions(expectedBank, actualBank);
		final List<Person> expectedLocalPersonsBefore = new ArrayList<>();
		final List<Person> actualLocalPersonsBefore = new ArrayList<>();
		for (int i = 0; i < TEST_PERSON_COUNT; i++) {
			final String passport = "john-doe-" + i;
			try {
				expectedLocalPersonsBefore.add(expectedBank.getPersonLocal(passport));
				actualLocalPersonsBefore.add(actualBank.getPersonLocal(passport));
			} catch (final RemoteException e) {
				throw new RuntimeException(e);
			}
		}
		randomActions(expectedBank, actualBank);
		final List<Person> expectedLocalPersonsAfter = new ArrayList<>();
		final List<Person> actualLocalPersonsAfter = new ArrayList<>();
		for (int i = 0; i < TEST_PERSON_COUNT; i++) {
			final String passport = "john-doe-" + i;
			try {
				expectedLocalPersonsAfter.add(expectedBank.getPersonLocal(passport));
				actualLocalPersonsAfter.add(actualBank.getPersonLocal(passport));
			} catch (final RemoteException e) {
				throw new RuntimeException(e);
			}
		}
		for (int i = 0; i < TEST_PERSON_COUNT; i++) {
			final Person expectedPersonBefore = expectedLocalPersonsBefore.get(i);
			final Person actualPersonBefore = actualLocalPersonsBefore.get(i);
			final Person expectedPersonAfter = expectedLocalPersonsAfter.get(i);
			final Person actualPersonAfter = actualLocalPersonsAfter.get(i);
			for (int j = 0; j < TEST_ACCOUNT_COUNT; j++) {
				final String subId = Integer.toString(j);
				try {
					BankChecker.equals(expectedPersonBefore.getAccount(subId), actualPersonBefore.getAccount(subId));
					BankChecker.equals(expectedPersonAfter.getAccount(subId), actualPersonAfter.getAccount(subId));
					BankChecker.notEquals(expectedPersonBefore.getAccount(subId), expectedPersonAfter.getAccount(subId));
					BankChecker.notEquals(actualPersonBefore.getAccount(subId), actualPersonAfter.getAccount(subId));
				} catch (final RemoteException e) {
					throw new RuntimeException(e);
				}
			}
		}
	}

	@Test
	public void test5_realClientTesting() {
		final Bank expectedBank = new BankChecker.TestBank();
		final Bank actualBank = getVirtualClient(NAME);
		emulateClient(expectedBank, DOSSIER, "first", 100);
		emulateClient(actualBank, DOSSIER, "first", 100);
		try {
			BankChecker.equals(
					expectedBank.getAccount(generateFullId(DOSSIER[2], "first")),
					actualBank.getAccount(generateFullId(DOSSIER[2], "first"))
			);
		} catch (final RemoteException e) {
			throw new RuntimeException(e);
		}
		emulateClient(expectedBank, DOSSIER, "first", -2100);
		emulateClient(actualBank, DOSSIER, "first", -2100);
		try {
			BankChecker.equals(
					expectedBank.getAccount(generateFullId(DOSSIER[2], "first")),
					actualBank.getAccount(generateFullId(DOSSIER[2], "first"))
			);
		} catch (final RemoteException e) {
			throw new RuntimeException(e);
		}
		emulateClient(expectedBank, DOSSIER, "second", -2100);
		emulateClient(actualBank, DOSSIER, "second", -2100);
		try {
			BankChecker.equals(
					expectedBank.getAccount(generateFullId(DOSSIER[2], "second")),
					actualBank.getAccount(generateFullId(DOSSIER[2], "second"))
			);
		} catch (final RemoteException e) {
			throw new RuntimeException(e);
		}
	}

	@Test
	public void test6_multithreadingCreating() throws RemoteException {
		final Bank expectedBank = new BankChecker.TestBank();
		final Bank actualBank = getVirtualClient(NAME);
		for (int i = 0; i < TEST_PERSON_COUNT; i++) {
			expectedBank.createPerson("John", "Doe", "john-doe-" + i);
		}
		final Phaser waiter = new Phaser(1);
		final ExecutorService services = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
		for (int i = 0; i < TEST_PERSON_COUNT; i++) {
			waiter.register();
			final int finalI = i;
			services.submit(() -> {
				try {
					actualBank.createPerson("John", "Doe", "john-doe-" + finalI);
					waiter.arrive();
				} catch (final RemoteException e) {
					throw new RuntimeException(e);
				}
			});
		}
		waiter.arriveAndAwaitAdvance();
		BankChecker.equalsBanks(actualBank, expectedBank, IntStream.range(0, TEST_PERSON_COUNT).mapToObj(s -> "john-doe-" + s).toList());
	}

	@Test
	public void test7_multithreadingAccounts() throws RemoteException {
		final Bank expectedBank = new BankChecker.TestBank();
		final Bank actualBank = getVirtualClient(NAME);
		for (int i = 0; i < TEST_PERSON_COUNT; i++) {
			final String passport = "john-doe-" + i;
			expectedBank.createPerson("John", "Doe", passport);
			actualBank.createPerson("John", "Doe", passport);
			for (int j = 0; j < TEST_ACCOUNT_COUNT; j++) {
				decOrIncByIndexAmount(expectedBank, passport, j, i);
			}
		}
		final Phaser waiter = new Phaser(1);
		final ExecutorService services = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
		for (int i = 0; i < TEST_PERSON_COUNT; i++) {
			final String passport = "john-doe-" + i;
			for (int j = 0; j < TEST_ACCOUNT_COUNT; j++) {
				waiter.register();
				final int finalJ = j;
				final int finalI = i;
				services.submit(() -> {
					try {
						decOrIncByIndexAmount(actualBank, passport, finalJ, finalI);
						Thread.sleep((short) ((Short.MAX_VALUE / 1024) * Math.random()));
						waiter.arrive();
					} catch (final RemoteException | InterruptedException e) {
						throw new RuntimeException(e);
					}
				});
			}
		}
		waiter.arriveAndAwaitAdvance();
		BankChecker.equalsBanks(actualBank, expectedBank, IntStream.range(0, TEST_PERSON_COUNT).mapToObj(s -> "john-doe-" + s).toList());
	}

	private void decOrIncByIndexAmount(
			final Bank bank,
			final String passport,
			final int j,
			final int i
	) throws RemoteException {
		final Account account = bank
				.createAccount(generateFullId(passport, j));
		final int amount = account.getAmount();
		if (i % 2 == 0 && j % 2 != 0) {
			account.setAmount(amount + (int) Math.pow(2, i));
		} else {
			account.setAmount(amount - (int) Math.pow(2, i));
		}
	}

//	@Test(expected = RuntimeException.class)
//	public void test8_damage() {
//		final Bank damagedBank = getVirtualClient(NAME);
//		int expected = 0, actual = 0;
//		boolean isKilled = false;
//		for (int i = 1; i <= TEST_DAMAGE_MAXIMUM_VALUE; i++) {
//			if (i >= TEST_DAMAGE_BREAK_POINT && !isKilled) {
//				try {
//					damageServer(NAME, PORT_RMI);
//					isKilled = true;
//				} catch (RemoteException | MalformedURLException | NotBoundException ignored) {
//				}
//			}
//			if (isKilled) {
//				expected++;
//			}
//			try {
//				damagedBank
//						.createPerson("John", "Doe", "john-doe-" + i);
//			} catch (final Exception e) {
//				if (isKilled) {
//					actual++;
//				}
//			}
//		}
//		Assert.assertEquals(expected, actual);
//	}

	private static void randomActions(final Bank expectedBank, final Bank actualBank) {
		for (int i = 0; i < BankTests.TEST_PERSON_COUNT; i++) {
			final String person = "john-doe-" + i;
			try {
				expectedBank.createPerson("John", "Doe", person);
				actualBank.createPerson("John", "Doe", person);
				for (int j = 0; j < BankTests.TEST_ACCOUNT_COUNT; j++) {
					final String subId = generateFullId(person, j);
					try {
						final int nextInt = RANDOMIZER.nextInt();
						if (Objects.isNull(expectedBank.getAccount(subId))) {
							expectedBank.createAccount(subId).setAmount(nextInt);
							actualBank.createAccount(subId).setAmount(nextInt);
						} else {
							expectedBank.getAccount(subId).setAmount(expectedBank.getAccount(subId).getAmount() + nextInt);
							actualBank.getAccount(subId).setAmount(actualBank.getAccount(subId).getAmount() + nextInt);
						}
					} catch (final RemoteException e) {
						throw new RuntimeException(e);
					}
				}
			} catch (final RemoteException e) {
				throw new RuntimeException(e);
			}
		}
	}
}
