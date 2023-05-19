package info.kgeorgiy.ja.bakturin.bank;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.util.Arrays;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class Client {
	private final static String NAME = "//localhost/bank";

	public static void main(final String[] args) throws RemoteException {
		final Bank bank;
		try {
			bank = (Bank) Naming.lookup(NAME);
		} catch (final NotBoundException e) {
			System.err.println("Bank is not bound.");
			return;
		} catch (final MalformedURLException e) {
			System.err.println("Bank URL is invalid.");
			return;
		}

		if (checkIfArgsInvalid(args)) {
			return;
		}

		try {
			final String name = args[0];
			final String surname = args[1];
			final String passport = args[2];
			final String subId = args[3];
			final int deltaAmount = Integer.parseInt(args[4]);
			Person person = bank.getPersonRemote(passport);
			if (Objects.isNull(person)) {
				System.out.println("No person named" + " " + "\"" + name + " " + surname + "\"" + "." + " " + "Creating new person.");
				person = bank.createPerson(name, surname, passport);
			} else {
				System.out.println("Person named" + " " + "\"" + name + " " + surname + "\"" + " " + "was found in db. Checking.");
				if (!name.equals(person.getName()) || !surname.equals(person.getSurname())) {
					System.err.println("Invalid data.");
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
		} catch (final NumberFormatException e) {
			System.err.println(e.getMessage());
		}
	}

	private static boolean checkIfArgsInvalid(final String[] args) {
		if (Objects.isNull(args)) {
			System.err.println("Arguments is null object.");
			return true;
		}

		if (args.length != 5) {
			System.err.println("Invalid number of arguments.");
			return true;
		}

		if (Arrays.stream(args).anyMatch(Objects::isNull)) {
			System.err.println("Some of arguments is invalid.");
			return true;
		}
		return false;
	}
}
