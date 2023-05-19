package info.kgeorgiy.ja.bakturin.bank;

import org.junit.Assert;

import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.BiFunction;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class BankChecker {
	private final static BiFunction<Bank, List<String>, List<Account>> GET_ALL_ACCOUNTS = (bank, id) -> id.stream().map(s -> {
		try {
			return bank.getAccount(s);
		} catch (final RemoteException e) {
			throw new RuntimeException(e);
		}
	}).toList();

	private final static BiFunction<Bank, List<String>, List<Person>> GET_ALL_PERSONS = (bank, id) -> id.stream().map(s -> {
		try {
			if (s.indexOf(':') == -1) {
				return null;
			}
			return bank.getPersonRemote(s.substring(0, s.indexOf(':')));
		} catch (final RemoteException e) {
			throw new RuntimeException(e);
		}
	}).toList();

	public static void equals(final Account actual, final Account expected) throws RemoteException {
		if (Objects.isNull(expected)) {
			equalsNull(actual);
			return;
		}
		Assert.assertEquals(actual.getId(), expected.getId());
		Assert.assertEquals(actual.getAmount(), expected.getAmount());
	}

	public static void equals(final Person actual, final Person expected) throws RemoteException {
		if (Objects.isNull(expected)) {
			equalsNull(actual);
			return;
		}
		Assert.assertEquals(actual.getName(), expected.getName());
		Assert.assertEquals(actual.getSurname(), expected.getSurname());
		Assert.assertEquals(actual.getPassport(), expected.getPassport());
	}

	public static void notEquals(final Account actual, final Account expected) throws RemoteException {
		Assert.assertTrue(
				!actual.getId().equals(expected.getId()) ||
						actual.getAmount() != expected.getAmount()
		);
	}

	public static void notEquals(final Person actual, final Person expected) throws RemoteException {
		Assert.assertTrue(
				!actual.getName().equals(expected.getName()) ||
						!actual.getSurname().equals(expected.getSurname()) ||
						!actual.getPassport().equals(expected.getSurname())
		);
	}

	public static void equalsAccounts(final List<Account> actual, final List<Account> expected) throws RemoteException {
		Assert.assertEquals(actual.size(), expected.size());
		for (int i = 0; i < actual.size(); i++) {
			equals(actual.get(i), expected.get(i));
		}
	}

	public static void equalsPersons(final List<Person> actual, final List<Person> expected) throws RemoteException {
		Assert.assertEquals(actual.size(), expected.size());
		for (int i = 0; i < actual.size(); i++) {
			equals(actual.get(i), expected.get(i));
		}
	}

	public static void notEqualsAccounts(final List<Account> actual, final List<Account> expected) throws RemoteException {
		for (int i = 0; i < actual.size(); i++) {
			notEquals(actual.get(i), expected.get(i));
		}
	}

	public static void notEqualsPersons(final List<Person> actual, final List<Person> expected) throws RemoteException {
		for (int i = 0; i < actual.size(); i++) {
			notEquals(actual.get(i), expected.get(i));
		}
	}

	public static void equalsBanks(final Bank actual, final Bank expected, final List<String> id) throws RemoteException {
		equalsAccounts(GET_ALL_ACCOUNTS.apply(actual, id), GET_ALL_ACCOUNTS.apply(expected, id));
		equalsPersons(GET_ALL_PERSONS.apply(actual, id), GET_ALL_PERSONS.apply(expected, id));
	}

	public static void notEqualsBanks(final Bank actual, final Bank expected, final List<String> id) throws RemoteException {
		notEqualsAccounts(GET_ALL_ACCOUNTS.apply(actual, id), GET_ALL_ACCOUNTS.apply(expected, id));
		notEqualsPersons(GET_ALL_PERSONS.apply(actual, id), GET_ALL_PERSONS.apply(expected, id));
	}

	public static <T> void equalsNull(final T actual) {
		Assert.assertNull(actual);
	}

	public static <T> void equalsNotNull(final T actual) {
		Assert.assertNotNull(actual);
	}

	public final static class TestPerson implements Person {
		private final String name;
		private final String surname;
		private final String passport;
		private final Bank bank;
		private final Map<String, Account> accounts;

		public TestPerson(final String name, final String surname, final String passport, final Bank bank) {
			this.name = name;
			this.surname = surname;
			this.passport = passport;
			this.bank = bank;
			this.accounts = null;
		}

		public TestPerson(final String name, final String surname, final String passport, final Map<String, Account> accounts) {
			this.name = name;
			this.surname = surname;
			this.passport = passport;
			this.bank = null;
			this.accounts = accounts;
		}

		@Override
		public String getName() throws RemoteException {
			return this.name;
		}

		@Override
		public String getSurname() throws RemoteException {
			return this.surname;
		}

		@Override
		public String getPassport() throws RemoteException {
			return this.passport;
		}

		@Override
		public Account createAccount(final String subId) throws RemoteException {
			final String fullId = passport + ":" + subId;
			if (Objects.isNull(bank)) {
				assert this.accounts != null;
				final Account account = new TestAccount(fullId);
				if (Objects.isNull(this.accounts.putIfAbsent(fullId, account))) {
					return account;
				} else {
					return getAccount(subId);
				}
			} else {
				assert this.accounts == null;
				return this.bank.createAccount(fullId);
			}
		}

		@Override
		public Account getAccount(final String subId) throws RemoteException {
			final String fullId = passport + ":" + subId;
			if (Objects.isNull(bank)) {
				assert this.accounts != null;
				return this.accounts.get(fullId);
			} else {
				assert this.accounts == null;
				return this.bank.getAccount(fullId);
			}
		}
	}

	public final static class TestBank implements Bank {
		private final Map<String, Person> persons;
		private final Map<String, Account> accounts;

		public TestBank() {
			this.persons = new HashMap<>();
			this.accounts = new HashMap<>();
		}

		@Override
		public Person createPerson(final String name, final String surname, final String passport) throws RemoteException {
			final Person person = new TestPerson(name, surname, passport, this);
			if (Objects.isNull(persons.putIfAbsent(passport, person))) {
				return person;
			} else {
				return getPersonRemote(passport);
			}
		}

		@Override
		public Person getPersonRemote(final String passport) throws RemoteException {
			return this.persons.get(passport);
		}

		@Override
		public Person getPersonLocal(final String passport) throws RemoteException {
			final Map<String, Account> copies = new HashMap<>();
			final Person person = persons.get(passport);
			accounts.entrySet().stream().filter(s -> s.getKey().contains(passport)).forEach((s) -> {
				final Account local = new TestAccount(s.getKey());
				try {
					local.setAmount(s.getValue().getAmount());
				} catch (final RemoteException e) {
					throw new RuntimeException(e);
				}
				copies.put(s.getKey(), local);
			});
			return new TestPerson(
					person.getName(),
					person.getSurname(),
					person.getPassport(),
					copies
			);
		}

		@Override
		public Account createAccount(final String id) throws RemoteException {
			final Account account = new TestAccount(id);
			if (Objects.isNull(accounts.putIfAbsent(id, account))) {
				return account;
			} else {
				return getAccount(id);
			}
		}

		@Override
		public Account getAccount(final String id) throws RemoteException {
			return this.accounts.get(id);
		}
	}

	public final static class TestAccount implements Account {
		private final String id;
		private int amount;

		public TestAccount(final String id) {
			this.id = id;
			this.amount = 0;
		}

		@Override
		public String getId() throws RemoteException {
			return this.id;
		}

		@Override
		public int getAmount() throws RemoteException {
			return this.amount;
		}

		@Override
		public void setAmount(final int amount) throws RemoteException {
			this.amount = amount;
		}
	}
}
