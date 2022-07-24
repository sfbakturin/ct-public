package markup;

import java.util.List;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Strong extends Paragraph implements Markdown {
	public Strong(final List<Markdown> list) {
		super(list, "__", "[b]", "[/b]");
	}
}
