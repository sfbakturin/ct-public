package info.kgeorgiy.ja.bakturin.crawler;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class Factory {
	private final ExecutorService conveyor;

	public Factory(final int size) {
		conveyor = Executors.newFixedThreadPool(size);
	}

	public void add(final Runnable run) {
		conveyor.submit(run);
	}

	public void close() {
		conveyor.shutdown();
		try {
			final boolean ignored = conveyor.awaitTermination(60L, TimeUnit.MILLISECONDS);
		} catch (final InterruptedException e) {
			System.err.println("Thread is interrupted.");
			conveyor.shutdownNow();
			Thread.currentThread().interrupt();
		}
	}
}
