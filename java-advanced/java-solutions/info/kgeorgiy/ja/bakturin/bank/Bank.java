package info.kgeorgiy.ja.bakturin.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface Bank extends Remote {
	/**
	 * Creates a new {@code Person} with specified name, surname and passport if it does not already exists.
	 *
	 * @param name     person's first name
	 * @param surname  person's second name
	 * @param passport person's passport identifier
	 * @return created or existing person
	 */
	Person createPerson(String name, String surname, String passport) throws RemoteException;

	/**
	 * Returns {@code Person} by passport.
	 *
	 * @param passport identifier.
	 * @return person with specified passport or {@code null} if such account does not exist.
	 */
	Person getPersonRemote(String passport) throws RemoteException;

	/**
	 * Returns {@code Person} by passport.
	 *
	 * @param passport identifier.
	 * @return person with specified passport or {@code null} if such account does not exist.
	 */
	Person getPersonLocal(String passport) throws RemoteException;

	/**
	 * Creates a new {@code Account} with specified identifier if it does not already exist.
	 *
	 * @param id account id
	 * @return created or existing account.
	 */
	Account createAccount(String id) throws RemoteException;

	/**
	 * Returns {@code Account} by identifier.
	 *
	 * @param id account id
	 * @return account with specified identifier or {@code null} if such account does not exist.
	 */
	Account getAccount(String id) throws RemoteException;
}
