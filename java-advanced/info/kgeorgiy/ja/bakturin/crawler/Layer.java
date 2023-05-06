package info.kgeorgiy.ja.bakturin.crawler;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.Phaser;

public class Layer {
	public final BlockingQueue<String> local;
	public final boolean flag;
	public final Phaser waiter;

	public Layer(final boolean b, final Phaser w) {
		local = new LinkedBlockingQueue<>();
		waiter = w;
		flag = b;
	}
}
