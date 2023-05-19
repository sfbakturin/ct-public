package info.kgeorgiy.ja.bakturin.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Phaser;
import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;

import static info.kgeorgiy.ja.bakturin.hello.HelloUDPServer.closeService;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class HelloUDPClient implements HelloClient {
	private final static String FORMAT_REQUEST;
	private final static String FORMAT_EXPECT;

	private final static int SOCKET_TIMEOUT;
	private final static long SERVICE_TIMEOUT;
	private final static TimeUnit SERVICE_TIMEOUT_TIME_UNIT;

	private static String makeCleanRequest(
			final String prefix,
			final int numberThread,
			final int numberRequest
	) {
		return String.format(
				FORMAT_REQUEST,
				prefix,
				numberThread,
				numberRequest
		);
	}

	private static byte[] makeRequest(final String message) {
		return message.getBytes();
	}

	private static byte[] makeExpected(final String message) {
		return String.format(FORMAT_EXPECT, message).getBytes();
	}

	private static boolean isEquals(final byte[] expected, final byte[] actual, final int offset) {
		return IntStream
				.range(0, expected.length)
				.allMatch((i) -> expected[i] == actual[i + offset]);
	}

	private static DatagramPacket makePacket(final byte[] data, final SocketAddress socketAddr) {
		return new DatagramPacket(
				data,
				data.length,
				socketAddr
		);
	}

	private static DatagramPacket makePacket(final DatagramSocket socketAddr) throws SocketException {
		return new DatagramPacket(
				new byte[socketAddr.getReceiveBufferSize()],
				socketAddr.getReceiveBufferSize()
		);
	}

	private static void send(
			final SocketAddress socketAddr,
			final DatagramSocket socket,
			final byte[] request
	) throws IOException {
		final DatagramPacket packet = makePacket(request, socketAddr);
		socket.send(packet);
	}

	private static DatagramPacket receive(final DatagramSocket socket) throws IOException {
		final DatagramPacket received = makePacket(socket);
		socket.receive(received);
		return received;
	}

	@Override
	public void run(
			final String host,
			final int port,
			final String prefix,
			final int threads,
			final int requests
	) {
		final SocketAddress socketAddr = new InetSocketAddress(host, port);
		final ExecutorService service = Executors.newFixedThreadPool(threads);
		final Phaser waiter = new Phaser(1);
		for (int i = 1; i <= threads; i++) {
			final Runnable multiRequest = new MultiRequest(
					waiter,
					prefix,
					requests,
					i,
					socketAddr
			);
			service.submit(multiRequest);
		}
		waiter.arriveAndAwaitAdvance();
		closeService(service, Thread.currentThread());
	}

	private record MultiRequest(
			Phaser waiter,
			String prefix,
			int requests,
			int numberThread,
			SocketAddress socketAddr
	) implements Runnable {
		@Override
		public void run() {
			waiter.register();
			try (final DatagramSocket socket = new DatagramSocket()) {
				socket.setSoTimeout(SOCKET_TIMEOUT);
				for (int j = 1; j <= requests; j++) {
					singleRequest(
							prefix,
							numberThread,
							j,
							socketAddr,
							socket);
				}
			} catch (final IOException ignored) {
			} finally {
				waiter.arrive();
			}
		}
	}

	private static void singleRequest(
			final String prefix,
			final int numberThread,
			final int numberRequest,
			final SocketAddress socketAddr,
			final DatagramSocket socket
	) {
		final String message = makeCleanRequest(prefix, numberThread, numberRequest);
		final byte[] request = makeRequest(message);
		final byte[] expected = makeExpected(message);
		while (true) {
			try {
				send(socketAddr, socket, request);
				final DatagramPacket received = receive(socket);
				if (isEquals(expected, received.getData(), received.getOffset())) {
					break;
				}
			} catch (final IOException ignored) {
			}
		}
	}

	static {
		FORMAT_REQUEST = "%s%d_%d";
		FORMAT_EXPECT = HelloUDPServer.FORMAT_RESPONSE;
		SOCKET_TIMEOUT = 500;
		SERVICE_TIMEOUT = 1L;
		SERVICE_TIMEOUT_TIME_UNIT = TimeUnit.MINUTES;
	}

	public static void main(final String[] args) {
		if (Objects.isNull(args)) {
			System.err.println("Arguments is null object.");
			return;
		}

		if (args.length != 5) {
			System.err.println("Invalid number of arguments.");
			return;
		}

		if (Arrays.stream(args).anyMatch(Objects::isNull)) {
			System.err.println("Any of arguments is null.");
			return;
		}

		try {
			final String host = args[0];
			final int port = Integer.parseInt(args[1]);
			final String prefix = args[2];
			final int threads = Integer.parseInt(args[3]);
			final int requests = Integer.parseInt(args[4]);
			final HelloClient client = new HelloUDPClient();
			client.run(host, port, prefix, threads, requests);
		} catch (final NumberFormatException e) {
			System.err.println(e.getMessage());
		}
	}
}
