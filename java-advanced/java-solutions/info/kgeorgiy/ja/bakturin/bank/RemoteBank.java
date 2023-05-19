package info.kgeorgiy.ja.bakturin.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class RemoteBank implements Bank {
	private final ConcurrentMap<String, Person> persons;
	private final ConcurrentMap<String, Account> accounts;
	private final int port;

	public RemoteBank(final int port) {
		this.persons = new ConcurrentHashMap<>();
		this.accounts = new ConcurrentHashMap<>();
		this.port = port;
	}

	@Override
	public Person createPerson(
			final String name,
			final String surname,
			final String passport
	) throws RemoteException {
		return persons.computeIfAbsent(
				passport,
				s -> {
					final Person person = new RemotePerson(
							name, surname, passport, new BankMapVariant(this)
					);
					try {
						registerRemote(person);
					} catch (final RemoteException e) {
						throw new RuntimeException(e);
					}
					return person;
				}
		);
	}

	@Override
	public Person getPersonRemote(final String passport) throws RemoteException {
		return persons.get(passport);
	}

	@Override
	public Person getPersonLocal(final String passport) throws RemoteException {
		final Map<String, Account> copyAccounts = new HashMap<>();
		final Person person = persons.get(passport);
		for (final Map.Entry<String, Account> s : accounts.entrySet()) {
			if (s.getKey().startsWith(passport)) {
				final Account local = new LocalAccount(s.getKey());
				local.setAmount(s.getValue().getAmount());
				copyAccounts.put(s.getKey(), local);
			}
		}
		return new LocalPerson(
				person.getName(),
				person.getSurname(),
				person.getPassport(),
				new BankMapVariant(copyAccounts)
		);
	}

	@Override
	public Account createAccount(final String id) throws RemoteException {
		return accounts.computeIfAbsent(
				id,
				s -> {
					final Account account = new RemoteAccount(id);
					try {
						registerRemote(account);
					} catch (final RemoteException e) {
						throw new RuntimeException(e);
					}
					return account;
				}
		);
	}

	@Override
	public Account getAccount(final String id) throws RemoteException {
		return accounts.get(id);
	}

	private void registerRemote(final Remote obj) throws RemoteException {
		UnicastRemoteObject.exportObject(obj, port);
	}
}
