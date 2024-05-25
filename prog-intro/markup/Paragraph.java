package markup;

import java.util.List;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Paragraph extends AbstractMarkdown implements Markdown {
	protected final List<Markdown> list;
	private String input = "";
	private String startBB = "";
	private String endBB = "";

	public Paragraph(final List<Markdown> list) {
		this.list = list;
	}

	public Paragraph(final List<Markdown> list, final String input, final String startBB, final String endBB) {
		this.list = list;
		this.startBB = startBB;
		this.endBB = endBB;
		this.input = input;
	}

	public void toMarkdown(final StringBuilder sb) {
		this.toMarkdown(sb, input, this.list);
	}

	public void toBBCode(final StringBuilder sb) {
		this.toBBCode(sb, this.startBB, this.endBB, this.list);
	}
}
