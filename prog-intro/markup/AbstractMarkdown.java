package markup;

import java.util.List;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public abstract class AbstractMarkdown implements Markdown {
	public void toMarkdown(final StringBuilder sb, final String input, final List<Markdown> list) {
		sb.append(input);
		for (final Markdown item : list) {
			item.toMarkdown(sb);
		}
		sb.append(input);
	}

	public void toBBCode(final StringBuilder sb, final String startBB, final String endBB, final List<Markdown> list) {
		sb.append(startBB);
		for (final Markdown item : list) {
			item.toBBCode(sb);
		}
		sb.append(endBB);
	}
}
