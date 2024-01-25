package bakturin.lab4.dynamo.grammar;

import bakturin.lab4.dynamo.assets.rule.Rule;
import bakturin.lab4.dynamo.assets.term.TerminalNode;

import java.util.Map;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public record Result(String grammar, String start, Map<String, TerminalNode> terminals,
                     Map<String, Rule> rules) {
}
