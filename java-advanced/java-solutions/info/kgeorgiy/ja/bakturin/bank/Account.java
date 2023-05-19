package info.kgeorgiy.ja.bakturin.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public interface Account extends Remote {
	String getId() throws RemoteException;

	int getAmount() throws RemoteException;

	void setAmount(int amount) throws RemoteException;
}
