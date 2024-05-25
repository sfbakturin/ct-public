package markup;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Text implements Markdown {
	private final String text;

	public Text(final String text) {
		this.text = text;
	}

	@Override
	public void toMarkdown(final StringBuilder sb) {
		sb.append(text);
	}

	@Override
	public void toBBCode(final StringBuilder sb) {
		sb.append(text);
	}
}
