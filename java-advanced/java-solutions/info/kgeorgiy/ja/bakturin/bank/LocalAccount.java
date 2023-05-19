package info.kgeorgiy.ja.bakturin.bank;

import java.io.Serializable;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class LocalAccount implements Account, Serializable {
	private final String id;
	private int amount;

	public LocalAccount(final String id) {
		this.id = id;
		this.amount = 0;
	}

	@Override
	public String getId() {
		return this.id;
	}

	@Override
	public int getAmount() {
		return this.amount;
	}

	@Override
	public void setAmount(final int amount) {
		this.amount = amount;
	}
}
