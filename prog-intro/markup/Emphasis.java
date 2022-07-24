package markup;

import java.util.List;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Emphasis extends Paragraph implements Markdown {
	public Emphasis(final List<Markdown> list) {
		super(list, "*", "[i]", "[/i]");
	}
}
