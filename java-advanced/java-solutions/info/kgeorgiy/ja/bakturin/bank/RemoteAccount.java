package info.kgeorgiy.ja.bakturin.bank;

import java.rmi.RemoteException;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class RemoteAccount implements Account {
	private final String id;
	public int amount;

	public RemoteAccount(final String id) {
		this.id = id;
		this.amount = 0;
	}

	@Override
	public String getId() throws RemoteException {
		return this.id;
	}

	@Override
	public synchronized int getAmount() throws RemoteException {
		return this.amount;
	}

	@Override
	public synchronized void setAmount(final int amount) throws RemoteException {
		this.amount = amount;
	}
}
