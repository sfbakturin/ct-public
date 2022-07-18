package markup;

import java.util.List;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class Strikeout extends Paragraph implements Markdown {
    public Strikeout(final List<Markdown> list) {
        super(list, "~", "[s]", "[/s]");
    }
}
