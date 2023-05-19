package info.kgeorgiy.ja.bakturin.bank;

import org.junit.Rule;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import java.net.MalformedURLException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class BankTester {
	private final static String LS = System.lineSeparator();
	private final static String TAB = "    ";
	private final static String FORMAT_START = "===> Start of" + " " + "\"" + "%s" + "\"." + LS;
	private final static String FORMAT_SUCCESS = TAB + " " + "%s" + " " + "successfully finished." + LS;
	private final static String FORMAT_FAIL = TAB + " " + "%s" + " " + "failed, because of" + " " + "%s." + LS;

	private String testMethodName;
	private static Registry RMI;
	private static Bank BANK;
	private final int portRMI;
	private final int portServer;
	private final String name;

	public BankTester(final int portRMI, final int portServer, final String name) {
		this.portRMI = portRMI;
		this.portServer = portServer;
		this.name = name;
	}

	@Rule
	public final TestRule watcher = new TestWatcher() {
		@Override
		protected void starting(final Description description) {
			try {
				createRMI(portRMI);
				createServer(portServer, name);
			} catch (final RemoteException e) {
				throw new RuntimeException(e);
			}
			testMethodName = description.getMethodName();
			System.out.printf(FORMAT_START, testMethodName);
		}

		@Override
		protected void succeeded(final Description description) {
			System.out.printf(FORMAT_SUCCESS, testMethodName);
		}

		@Override
		protected void failed(final Throwable e, final Description description) {
			System.err.printf(FORMAT_FAIL, testMethodName, e.getMessage());
		}

		@Override
		protected void finished(final Description description) {
			try {
				killRMI();
				killServer(name);
			} catch (final RemoteException | MalformedURLException | NotBoundException e) {
				throw new RuntimeException(e);
			}
		}
	};

	protected Bank getVirtualClient(final String name) {
		try {
			return ((Bank) RMI.lookup(name));
		} catch (final NotBoundException | RemoteException e) {
			throw new RuntimeException(e);
		}
	}

	protected static void createRMI(final int port) throws RemoteException {
		RMI = LocateRegistry.createRegistry(port);
	}

	protected static void createServer(final int port, final String name) throws RemoteException {
		BANK = new RemoteBank(port);
		try {
			UnicastRemoteObject.exportObject(BANK, port);
			RMI.rebind(name, BANK);
		} catch (final RemoteException e) {
			throw new RuntimeException(e);
		}
	}

	protected static void killRMI() throws RemoteException {
		UnicastRemoteObject.unexportObject(RMI, true);
	}

	protected static void killServer(final String name) throws RemoteException, MalformedURLException, NotBoundException {
		RMI.unbind(name);
		UnicastRemoteObject.unexportObject(BANK, true);
	}

//	protected static void damageServer(final String name, final int port) throws RemoteException, MalformedURLException, NotBoundException {
//		killServer(name);
//		killRMI();
//		BANK = null;
//		RMI = null;
//		System.gc();
//	}

	protected static void emulateClient(final Bank bank, final String[] dossier, final String subId, final int deltaAmount) {
		try {
			final String name = dossier[0];
			final String surname = dossier[1];
			final String passport = dossier[2];
			Person person = bank.getPersonRemote(passport);
			if (Objects.isNull(person)) {
				System.out.println("No person named" + " " + "\"" + name + " " + surname + "\"" + "." + " " + "Creating new person.");
				person = bank.createPerson(name, surname, passport);
			} else {
				System.out.println("Person named" + " " + "\"" + name + " " + surname + "\"" + " " + "was found in db. Checking.");
				if (!name.equals(person.getName()) || !surname.equals(person.getSurname())) {
					System.out.println("Invalid data.");
					return;
				}
				System.out.println("OK.");
			}
			Account account = person.getAccount(subId);
			if (Objects.isNull(account)) {
				System.out.println("No account" + " " + "\"" + subId + "\"" + " " + "of person named" + " " + "\"" + name + " " + surname + "\"" + "." + " " + "Creating new account.");
				account = person.createAccount(subId);
			} else {
				System.out.println("Account" + " " + "\"" + subId + "\"" + " " + "of person named" + " " + "\"" + name + " " + surname + "\"" + " " + "was found");
			}
			System.out.println("Current amount:" + " " + account.getAmount());
			System.out.println("Adding:" + " " + deltaAmount);
			account.setAmount(account.getAmount() + deltaAmount);
			System.out.println("Updated amount:" + " " + account.getAmount());
		} catch (final RemoteException e) {
			throw new RuntimeException(e);
		}
	}

	protected static String generateFullId(final String person, final String num) {
		return String.format("%s:%s", person, num);
	}

	protected static String generateFullId(final String person, final int num) {
		return generateFullId(person, Integer.toString(num));
	}

	protected static String generateFullId(final String num) {
		return generateFullId("person", num);
	}

	protected static String generateFullId(final int num) {
		return generateFullId(Integer.toString(num));
	}
}
