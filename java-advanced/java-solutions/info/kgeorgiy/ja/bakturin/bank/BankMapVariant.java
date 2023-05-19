package info.kgeorgiy.ja.bakturin.bank;

import java.util.Map;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class BankMapVariant implements Variant<Bank, Map<String, Account>> {
	private final Bank bank;
	private final Map<String, Account> map;

	public BankMapVariant(final Bank b) {
		this.bank = b;
		this.map = null;
	}

	public BankMapVariant(final Map<String, Account> m) {
		this.bank = null;
		this.map = m;
	}

	@Override
	public Bank getFirst() throws IllegalArgumentException {
		if (Objects.isNull(this.bank)) {
			throw new IllegalArgumentException("Requested Bank, but was storaged Map.");
		} else {
			return this.bank;
		}
	}

	@Override
	public Map<String, Account> getSecond() throws IllegalArgumentException {
		if (Objects.isNull(this.map)) {
			throw new IllegalArgumentException("Requested Map, but was storaged Bank.");
		} else {
			return this.map;
		}
	}
}
