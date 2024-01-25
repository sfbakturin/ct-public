package bakturin.lab3.py2c;

import bakturin.lab3.antlr.Py2CLexer;
import bakturin.lab3.antlr.Py2CParser;
import bakturin.lab3.py2c.assets.CStatement;
import bakturin.lab3.py2c.assets.CVariable;
import bakturin.lab3.py2c.exceptions.Py2CException;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.util.Map;
import java.util.Objects;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2C {
	private final static String LS = System.lineSeparator();

	private Py2C() {
		throw new AssertionError("No bakturin.lab3.py2c.Py2C instances for you!");
	}

	public static Py2CResult translate(final String code) {
		try {
			final Py2CLexer lex = new Py2CLexer(CharStreams.fromString(code));
			final CommonTokenStream tok = new CommonTokenStream(lex);
			final Py2CParser p = new Py2CParser(tok);
			final ParseTree t = p.python();
			final ParseTreeWalker w = new ParseTreeWalker();
			final Py2CTranslator translator = new Py2CTranslator();

			w.walk(translator, t);

			final StringBuilder cSrc = new StringBuilder();

			cSrc.append(Py2CStd.STD_INITIALIZE);
			cSrc.append(LS);
			translator.variables.get("main").forEach((s, v) -> cSrc.append(v.asDeclaration()).append(LS));
			cSrc.append(LS);

			translator.functions.get("main").fixTypes(translator.variables.get("main"));
			for (final Map.Entry<String, CStatement> entry : translator.functions.entrySet()) {
				if (!entry.getKey().equals("main")) {
					cSrc.append(entry.getValue().asStatement(translator.variables.get(entry.getKey())));
				}
			}
			cSrc.append(translator.functions.get("main").asStatement());

			return new Py2CResult(translator.variables, translator.functions, cSrc.toString());
		} catch (final Py2CException e) {
			return new Py2CResult(e);
		} catch (final Exception e) {
			return new Py2CResult(new Py2CException(e.getMessage()));
		}
	}

	public static class Py2CResult {
		private final Py2CException error;
		public final Map<String, Map<String, CVariable>> variables;
		public final Map<String, CStatement> functions;
		public final String code;

		public Py2CResult(final Py2CException error) {
			this.variables = null;
			this.functions = null;
			this.code = null;
			this.error = error;
		}

		public Py2CResult(final Map<String, Map<String, CVariable>> variables, final Map<String, CStatement> functions, final String code) {
			this.variables = variables;
			this.functions = functions;
			this.code = code;
			this.error = null;
		}

		public boolean isSuccess() {
			return Objects.isNull(this.error);
		}

		public boolean isFailed() {
			return !isSuccess();
		}

		public String errorMessage() {
			if (Objects.nonNull(this.error)) {
				return this.error.getMessage();
			} else {
				return null;
			}
		}

		public Class<? extends Py2CException> errorClass() {
			if (Objects.nonNull(this.error)) {
				return this.error.getClass();
			} else {
				return null;
			}
		}
	}
}
