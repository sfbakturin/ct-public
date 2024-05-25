package md2html;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Workshop {
	private final FileInputHelper input;
	private final FileOutputHelper output;
	private final List<String> strings;
	private final Map<Integer, List<String>> headers;
	private final Map<Integer, List<String>> paragraphs;
	private final Set<Integer> headersIndex, paragraphsIndex;
	private int index;

	public Workshop(final String i, final String o) {
		this.input = new FileInputHelper(i);
		this.output = new FileOutputHelper(o);
		this.strings = new ArrayList<>();
		this.headers = new LinkedHashMap<>();
		this.paragraphs = new LinkedHashMap<>();
		this.headersIndex = new LinkedHashSet<>();
		this.paragraphsIndex = new LinkedHashSet<>();
		this.index = -1;
		this.run();
	}

	private void run() {
		this.read();
		this.add();
		this.factory();
	}

	private void read() {
		while (this.input.hasNextLine()) {
			this.strings.add(this.input.nextLine());
		}
		this.input.close();
	}

	private void factory() {
		for (int i = 0; i < this.strings.size(); i++) {
			if (this.headers.containsKey(i)) {
				this.toMarkdown(i, this.headers);
				this.highlightHeader(i);
				this.output.write(this.headers.get(i));
			} else {
				if (this.paragraphs.containsKey(i)) {
					this.toMarkdown(i, this.paragraphs);
					this.highlightParagraph(i);
					this.output.write(this.paragraphs.get(i));
				}
			}
		}
		this.output.close();
	}

	private void add() {
		for (int i = 0; i < this.strings.size(); i++) {
			final String string = this.strings.get(i);
			final String[] split = string.split(" ");
			if (string.length() != 0) {
				final int checkExisting = this.checkExisting(this.strings, i);
				if (string.charAt(0) == '#') {
					if (this.checkHeader(split[0]) != -1) {
						this.addElement(checkExisting, string, true);
					} else {
						if (checkExisting == i) {
							this.addElement(checkExisting, string, false);
						} else {
							this.addElement(checkExisting, string, this.checkType() == -1);
						}
					}
				} else {
					if (checkExisting == i) {
						this.addElement(checkExisting, string, false);
					} else {
						this.addElement(checkExisting, string, this.checkType() == -1);
					}
				}
			} else {
				this.index = -1;
			}
		}
	}

	private void highlightHeader(final int o) {
		final List<String> list = this.headers.get(o);
		int k = this.checkHeader(list.get(0));
		String[] split = list.get(0).split(" ");
		final StringBuilder sb = new StringBuilder();
		sb.append("<h").append(k).append(">");
		for (int i = 1; i <= split.length - 2; i++) {
			sb.append(split[i]).append(" ");
		}
		sb.append(split[split.length - 1]);
		if (list.size() != 1) {
			list.set(0, sb.toString());
			sb.setLength(0);
			split = list.get(list.size() - 1).split(" ");
			for (int i = 0; i <= split.length - 2; i++) {
				sb.append(split[i]).append(" ");
			}
			sb.append(split[split.length - 1]);
			sb.append("</h").append(k).append(">");
			list.set(list.size() - 1, sb.toString());
		} else {
			sb.append("</h").append(k).append(">");
			list.set(0, sb.toString());
		}
		this.headers.put(o, list);
	}

	private void highlightParagraph(int o) {
		final List<String> list = this.paragraphs.get(o);
		String[] split = list.get(0).split(" ");
		final StringBuilder sb = new StringBuilder();
		sb.append("<p>");
		for (int i = 0; i <= split.length - 2; i++) {
			sb.append(split[i]).append(" ");
		}
		sb.append(split[split.length - 1]);
		if (list.size() != 1) {
			list.set(0, sb.toString());
			sb.setLength(0);
			split = list.get(list.size() - 1).split(" ");
			for (int i = 0; i <= split.length - 2; i++) {
				sb.append(split[i]).append(" ");
			}
			sb.append(split[split.length - 1]);
			sb.append("</p>");
			list.set(list.size() - 1, sb.toString());
		} else {
			sb.append("</p>");
			list.set(0, sb.toString());
		}
		this.paragraphs.put(o, list);
	}

	private void toMarkdown(final int i, Map<Integer, List<String>> headers) {
		Markdown markdown = new Markdown(headers.get(i));
		headers.put(i, markdown.getList());
	}

	private int checkHeader(final String string) {
		int count = 0;
		for (int i = 0; i < string.length(); i++) {
			if (string.charAt(i) != '#' && string.charAt(i) != ' ') {
				count = -1;
				break;
			} else {
				if (string.charAt(i) == ' ') {
					break;
				}
				count++;
			}
		}
		return count;
	}

	private int checkExisting(final List<String> strings, final int index) {
		if (index - 1 >= 0) {
			if (strings.get(index - 1).isEmpty()) {
				this.index = index;
				return index;
			} else {
				return index - 1;
			}
		} else {
			this.index = index;
			return index;
		}
	}

	private int checkType() {
		if (this.headersIndex.contains(this.index)) {
			return -1;
		} else {
			return 0;
		}
	}

	private void addElement(final int index, final String string, final boolean type) {
		if (type) {
			final List<String> temp;
			if (this.headersIndex.contains(this.index)) {
				temp = this.headers.get(this.index);
			} else {
				temp = new ArrayList<>();
			}
			temp.add(string);
			this.headers.put(this.index, temp);
			this.headersIndex.add(index);
		} else {
			final List<String> temp;
			if (this.paragraphsIndex.contains(this.index)) {
				temp = this.paragraphs.get(this.index);
			} else {
				temp = new ArrayList<>();
			}
			temp.add(string);
			this.paragraphs.put(this.index, temp);
			this.paragraphsIndex.add(index);
		}
	}
}
