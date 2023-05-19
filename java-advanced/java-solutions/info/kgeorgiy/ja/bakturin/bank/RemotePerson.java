package info.kgeorgiy.ja.bakturin.bank;

import java.rmi.RemoteException;
import java.util.function.BiFunction;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class RemotePerson extends AbstractPerson {
	private final static BiFunction<Bank, String, Account> ACCOUNT_GETTER = (bank, id) -> {
		try {
			return bank.getAccount(id);
		} catch (final RemoteException e) {
			throw new RuntimeException(e);
		}
	};

	private final static BiFunction<Bank, String, Account> ACCOUNT_SETTER = (bank, id) -> {
		try {
			return bank.createAccount(id);
		} catch (final RemoteException e) {
			throw new RuntimeException(e);
		}
	};

	public RemotePerson(
			final String name,
			final String surname,
			final String passport,
			final BankMapVariant variant
	) {
		super(name, surname, passport, variant);
	}

	@Override
	protected Account getterImpl(final String id, final BankMapVariant v) throws RemoteException {
		return v.applyFirst(ACCOUNT_GETTER, id);
	}

	@Override
	protected Account setterImpl(final String id, final BankMapVariant v) throws RemoteException {
		return v.applyFirst(ACCOUNT_SETTER, id);
	}
}
