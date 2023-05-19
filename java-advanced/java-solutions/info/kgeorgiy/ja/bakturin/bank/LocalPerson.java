package info.kgeorgiy.ja.bakturin.bank;

import java.io.Serializable;
import java.util.Map;
import java.util.function.BiFunction;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class LocalPerson extends AbstractPerson implements Serializable {
	private final static BiFunction<Map<String, Account>, String, Account>
			ACCOUNT_GETTER = Map::get;
	private final static BiFunction<Map<String, Account>, String, Account>
			ACCOUNT_SETTER = (b, s) -> b.put(s, new LocalAccount(s));

	public LocalPerson(
			final String name,
			final String surname,
			final String passport,
			final BankMapVariant variant
	) {
		super(name, surname, passport, variant);
	}

	@Override
	protected Account getterImpl(final String id, final BankMapVariant v) {
		return v.applySecond(ACCOUNT_GETTER, id);
	}

	@Override
	protected Account setterImpl(final String id, final BankMapVariant v) {
		return v.applySecond(ACCOUNT_SETTER, id);
	}
}
