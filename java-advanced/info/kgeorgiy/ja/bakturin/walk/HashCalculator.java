package info.kgeorgiy.ja.bakturin.walk;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class HashCalculator {
	private final MessageDigest md = MessageDigest.getInstance("SHA-256");

	public HashCalculator() throws NoSuchAlgorithmException {
	}

	public void update(final byte[] bytes, final int length) {
		md.update(bytes, 0, length);
	}

	@Override
	public String toString() {
		return HexFormat.of().formatHex(md.digest());
	}
}
