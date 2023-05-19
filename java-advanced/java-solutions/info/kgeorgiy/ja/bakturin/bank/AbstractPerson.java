package info.kgeorgiy.ja.bakturin.bank;

import java.rmi.RemoteException;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public abstract class AbstractPerson implements Person {
	private final String name;
	private final String surname;
	private final String passport;
	private final BankMapVariant variant;

	public AbstractPerson(
			final String name,
			final String surname,
			final String passport,
			final BankMapVariant variant
	) {
		this.name = name;
		this.surname = surname;
		this.passport = passport;
		this.variant = variant;
	}

	@Override
	public String getName() {
		return this.name;
	}

	@Override
	public String getSurname() {
		return this.surname;
	}

	@Override
	public String getPassport() {
		return this.passport;
	}

	@Override
	public Account createAccount(final String subId) throws RemoteException {
		final String fullId = getFullId(subId);
		final Account account = getterImpl(fullId, variant);
		if (Objects.isNull(account)) {
			return setterImpl(fullId, variant);
		} else {
			return account;
		}
	}

	@Override
	public Account getAccount(final String subId) throws RemoteException {
		return getterImpl(getFullId(subId), variant);
	}

	private String getFullId(final String subId) {
		return String.format("%s:%s", passport, subId);
	}

	protected abstract Account getterImpl(final String id, final BankMapVariant v) throws RemoteException;

	protected abstract Account setterImpl(final String id, final BankMapVariant v) throws RemoteException;
}
