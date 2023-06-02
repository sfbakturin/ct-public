package info.kgeorgiy.ja.bakturin.i18n;

import info.kgeorgiy.ja.bakturin.i18n.format.CategoryImpl;
import info.kgeorgiy.ja.bakturin.i18n.format.Subcategory;
import info.kgeorgiy.ja.bakturin.i18n.format.SubcategoryImpl;
import info.kgeorgiy.ja.bakturin.i18n.format.Writer;
import info.kgeorgiy.ja.bakturin.i18n.util.File;
import info.kgeorgiy.ja.bakturin.i18n.util.Init;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public final class TextStatistics {
	public final static Set<Locale> LOCALES_INPUT;
	public final static Set<Locale> LOCALES_OUTPUT;
	public final static Charset SC = StandardCharsets.UTF_8;
	public static final List<Subcategory> SUMMARY;

	static {
		LOCALES_INPUT = new TreeSet<>(Comparator.comparing(Locale::toString));
		LOCALES_INPUT.addAll(Arrays.asList(Locale.getAvailableLocales()));
	}

	static {
		LOCALES_OUTPUT = new TreeSet<>(Comparator.comparing(Locale::toString));
		LOCALES_OUTPUT
				.addAll(LOCALES_INPUT
						.stream()
						.filter(loc -> loc.toLanguageTag().startsWith("en") || loc.toLanguageTag().startsWith("ru"))
						.toList()
				);
	}

	static {
		SUMMARY = List.of(SubcategoryImpl.OCCURRENCES);
	}

	public static void main(final String[] args) {
		try {
			final var user = Init.parse(args);
			final var text = File.readText(user.getPathInput());
			final var parser = new TextParser(user.getLocaleInput(), user.getLocaleOutput());
			final var stats = Arrays.stream(CategoryImpl.values()).map(cat -> cat.getCategory(parser, text)).toList();
			final var writer = new Writer(user.getLocaleOutput());
			final var parsed = new StringBuilder();
			parsed.append(writer.header(user.getPathInput().toString()));
			parsed.append(writer.category("summary"));
			for (final var item : stats) {
				for (final Subcategory sub : SUMMARY) {
					parsed.append(sub.format(item.left(), writer));
				}
			}
			for (final var item : stats) {
				parsed.append(writer.category(item.left().getId()));
				final var subStats = writer.format(item.left(), item.right());
				subStats.forEach(parsed::append);
			}
			File.writeText(parsed.toString(), user.getPathOutput());
		} catch (final IOException e) {
			System.err.println(e.getMessage());
			System.exit(1);
		}
	}
}
