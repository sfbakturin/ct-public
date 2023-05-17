package info.kgeorgiy.ja.bakturin.crawler;

import java.io.IOException;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.LinkedBlockingQueue;

public class Session {
	public final ConcurrentMap<String, IOException> errors;
	public final Set<String> cache;
	public final BlockingQueue<String> result;
	public BlockingQueue<String> pending;

	public Session() {
		errors = new ConcurrentHashMap<>();
		cache = ConcurrentHashMap.newKeySet();
		result = new LinkedBlockingQueue<>();
		pending = new LinkedBlockingQueue<>();
	}
}
