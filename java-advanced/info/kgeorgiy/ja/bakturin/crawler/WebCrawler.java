package info.kgeorgiy.ja.bakturin.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.Phaser;
import java.util.stream.IntStream;

public class WebCrawler implements Crawler {
	private final Factory queueDownload;
	private final Factory queueExtract;
	private final Downloader downloader;
	private final static int DEFAULT_DEPTH = 2;
	private final static int DEFAULT_DOWNLOADS = 10;
	private final static int DEFAULT_EXTRACTORS = Runtime.getRuntime().availableProcessors();
	private final static int DEFAULT_PER_HOST = 10;
	private final static String DEFAULT_PATH_NAME = "downloads";
	private final static double DEFAULT_TIME_SCALE = 1.0;

	public WebCrawler(final Downloader downloader, final int downloaders, final int extractors, final int perHost) {
		this.downloader = downloader;
		queueDownload = new Factory(downloaders);
		queueExtract = new Factory(extractors);
	}

	@Override
	public Result download(final String url, final int depth) {
		final Session session = new Session();
		final Phaser waiter = new Phaser(1);
		session.pending.add(url);
		session.cache.add(url);
		for (int i = 1; i <= depth; i++) {
			final Layer layer = new Layer(i != depth, waiter);
			for (final String currentURL : session.pending) {
				waiter.register();
				queueDownload.add(() -> taskDownload(currentURL, layer, session));
			}
			waiter.arriveAndAwaitAdvance();
			session.pending = layer.local;
		}
		return new Result(new ArrayList<>(session.result), session.errors);
	}

	private void taskDownload(final String url, final Layer layer, final Session session) {
		try {
			final Document document = downloader.download(url);
			session.result.add(url);
			if (layer.flag) {
				layer.waiter.register();
				queueExtract.add(() -> taskExtract(document, layer, session));
			}
		} catch (final IOException err) {
			session.errors.put(url, err);
		} finally {
			layer.waiter.arriveAndDeregister();
		}
	}

	private void taskExtract(final Document document, final Layer layer, final Session session) {
		try {
			final List<String> extracted = document.extractLinks();
			for (final String s : extracted) {
				if (session.cache.add(s)) {
					layer.local.add(s);
				}
			}
		} catch (final IOException ignored) {
		} finally {
			layer.waiter.arriveAndDeregister();
		}
	}

	@Override
	public void close() {
		queueDownload.close();
		queueExtract.close();
	}

	public static void main(final String[] args) {
		if (Objects.isNull(args)) {
			System.err.println("Arguments is null object.");
			return;
		}

		if (!(args.length > 0 && args.length <= 5)) {
			System.err.println("Invalid number of arguments.");
			return;
		}

		if (Arrays.stream(args).anyMatch(Objects::isNull)) {
			System.err.println("Some of arguments is invalid");
			return;
		}

		final int[] arguments = new int[]{
				DEFAULT_DEPTH,
				DEFAULT_DOWNLOADS,
				DEFAULT_EXTRACTORS,
				DEFAULT_PER_HOST
		};


		try {
			IntStream.range(1, args.length).forEach((i) -> arguments[i] = Integer.parseInt(args[i]));
		} catch (final NumberFormatException e) {
			System.err.println(e.getMessage());
			return;
		}

		try {
			final Downloader mainDownloader = new CachingDownloader(DEFAULT_TIME_SCALE, Paths.get(DEFAULT_PATH_NAME));
			try (Crawler crawler = new WebCrawler(mainDownloader, arguments[1], arguments[2], arguments[3])) {
				final Result result = crawler.download(args[0], arguments[0]);
				System.out.println("Successfully downloaded:");
				result.getDownloaded().forEach(System.out::println);
				System.out.println("Failed downloaded:");
				result.getErrors().forEach((key, value) -> System.out.println("Link" + " " + key + " " + "error" + " " + value.getMessage()));
			}
		} catch (final IOException e) {
			System.err.println("Path of" + " " + DEFAULT_PATH_NAME + " " + "can't be reached");
		}
	}
}
