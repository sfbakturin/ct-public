package info.kgeorgiy.ja.bakturin.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.Arrays;
import java.util.Objects;
import java.util.Scanner;
import java.util.Set;
import java.util.concurrent.*;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class HelloUDPServer implements HelloServer {
	private final static long HANDLER_TIMEOUT;
	private final static TimeUnit HANDLER_TIMEOUT_TIME_UNIT;

	public final static String FORMAT_RESPONSE;

	private final static Set<String> EXIT;

	private final BlockingQueue<DatagramSocket> sockets = new LinkedBlockingQueue<>();
	private final BlockingQueue<ExecutorService> handlers = new LinkedBlockingQueue<>();

	private static DatagramPacket makePacket(final DatagramSocket socket) throws SocketException {
		return new DatagramPacket(
				new byte[socket.getReceiveBufferSize()],
				socket.getReceiveBufferSize()
		);
	}

	private static String makeCleanResponse(final DatagramPacket received) {
		return new String(
				received.getData(),
				received.getOffset(),
				received.getLength()
		);
	}

	private static String makeFormattedResponse(final DatagramPacket received) {
		return String.format(
				FORMAT_RESPONSE,
				makeCleanResponse(received)
		);
	}

	private static byte[] makeResponse(final DatagramPacket received) {
		return makeFormattedResponse(received).getBytes();
	}

	@Override
	public void start(final int port, final int threads) {
		final ExecutorService handler;
		final DatagramSocket socket;
		try {
			socket = new DatagramSocket(port);
			handler = Executors.newFixedThreadPool(threads);
			sockets.add(socket);
			handlers.add(handler);
		} catch (final IOException e) {
			throw new RuntimeException(e);
		}
		final Runnable daemon = new ServerDaemon(socket);
		for (int i = 0; i < threads; i++) {
			handler.submit(daemon);
		}
	}

	@Override
	public void close() {
		while (!sockets.isEmpty()) {
			final DatagramSocket socket = sockets.poll();
			socket.close();
		}
		while (!handlers.isEmpty()) {
			final ExecutorService handler = handlers.poll();
			handler.shutdown();
			while (true) {
				try {
					if (!handler.awaitTermination(
							HANDLER_TIMEOUT,
							HANDLER_TIMEOUT_TIME_UNIT
					)) {
						continue;
					}
					break;
				} catch (final InterruptedException ignored) {
					handler.shutdownNow();
				}
			}
		}
	}

	private record ServerDaemon(DatagramSocket socket) implements Runnable {
		@Override
		public void run() {
			try {
				final DatagramPacket received = makePacket(socket);
				while (!Thread.interrupted() && !socket.isClosed()) {
					socket.receive(received);
					final byte[] response = makeResponse(received);
					final DatagramPacket sent = new DatagramPacket(
							response,
							response.length,
							received.getSocketAddress()
					);
					socket.send(sent);
				}
			} catch (final IOException ignored) {
			}
		}
	}

	static {
		HANDLER_TIMEOUT = 60L;
		HANDLER_TIMEOUT_TIME_UNIT = TimeUnit.MILLISECONDS;
		FORMAT_RESPONSE = "Hello" + "," + " " + "%s";
		EXIT = Set.of(
				"exit",
				"power off",
				"shutdown"
		);
	}

	public static void main(final String[] args) {
		if (Objects.isNull(args)) {
			System.err.println("Arguments is null object.");
			return;
		}

		if (args.length != 2) {
			System.err.println("Invalid number of arguments.");
			return;
		}

		if (Arrays.stream(args).anyMatch(Objects::isNull)) {
			System.err.println("Any of arguments is null.");
			return;
		}

		try {
			final int port = Integer.parseInt(args[0]);
			final int threads = Integer.parseInt(args[1]);

			final Scanner user = new Scanner(System.in);
			final HelloServer server = new HelloUDPServer();

			server.start(port, threads);
			while (true) {
				final String command = user.nextLine();
				if (EXIT.contains(command)) {
					break;
				}
			}
			server.close();
		} catch (final NumberFormatException e) {
			System.err.println(e.getMessage());
		}
	}
}
