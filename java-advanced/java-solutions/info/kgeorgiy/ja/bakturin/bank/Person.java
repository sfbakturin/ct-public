package info.kgeorgiy.ja.bakturin.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface Person extends Remote {
	/**
	 * Returns person's first name.
	 *
	 * @return person's first name as {@code String}.
	 */
	String getName() throws RemoteException;

	/**
	 * Returns person's second name.
	 *
	 * @return person's second name as {@code String}.
	 */
	String getSurname() throws RemoteException;

	/**
	 * Returns person's passport.
	 *
	 * @return person's passport as {@code String}.
	 */
	String getPassport() throws RemoteException;

	/**
	 * Adding to {@code Person} new account with specified identifier.
	 *
	 * @param subId identifier.
	 * @return account by subId.
	 */
	Account createAccount(String subId) throws RemoteException;

	/**
	 * Returns {@code Account} by subId from {@code Person}.
	 *
	 * @param subId account's subId.
	 * @return account by subId.
	 */
	Account getAccount(String subId) throws RemoteException;
}
