package bakturin.lab3.py2c;

import bakturin.lab3.antlr.Py2CBaseListener;
import bakturin.lab3.antlr.Py2CParser;
import bakturin.lab3.py2c.assets.*;
import bakturin.lab3.py2c.assets.expr.*;
import bakturin.lab3.py2c.assets.stmt.*;
import bakturin.lab3.py2c.assets.variable.DummyVariable;
import bakturin.lab3.py2c.assets.variable.FloatVariable;
import bakturin.lab3.py2c.assets.variable.IntVariable;
import bakturin.lab3.py2c.assets.variable.StringVariable;
import bakturin.lab3.py2c.exceptions.Py2CUnsupportedBinaryOperatorException;
import bakturin.lab3.py2c.exceptions.Py2CUnsupportedComparisonOperatorException;
import bakturin.lab3.py2c.exceptions.Py2CUnsupportedVariableTypeException;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Py2CTranslator extends Py2CBaseListener {
	public final Map<String, Map<String, CVariable>> variables;
	public final Map<String, CStatement> functions;
	private final Deque<Deque<CExpression>> previousExprs;
	private final List<List<CStatement>> previousStmts;
	private String currentFunction;
	private Deque<CExpression> currentExpr;
	private List<CStatement> currentStmts;
	private final Map<String, CVariable> currentDummies;

	public Py2CTranslator() {
		this.currentExpr = new ArrayDeque<>();
		this.previousExprs = new ArrayDeque<>();
		this.variables = new HashMap<>();
		this.previousStmts = new ArrayList<>();
		this.currentStmts = new ArrayList<>();
		this.functions = new HashMap<>();
		this.currentDummies = new HashMap<>();
	}

	@Override
	public void enterPython(final Py2CParser.PythonContext ctx) {
		this.currentFunction = "main";
		this.variables.put(this.currentFunction, new HashMap<>());
	}

	@Override
	public void exitPython(final Py2CParser.PythonContext ctx) {
		final CStatement main = new CFunction(CType.INT, this.currentFunction, this.currentStmts);
		this.functions.put(this.currentFunction, main);
	}

	@Override
	public void enterDefStmt(final Py2CParser.DefStmtContext ctx) {
		this.currentFunction = ctx.target().getText();
		this.variables.put(this.currentFunction, new HashMap<>());
		this.previousStmts.add(this.currentStmts);
		this.currentStmts = new ArrayList<>();
		final List<TerminalNode> list = ctx.defArgs().IDENTIFIER();
		if (!list.isEmpty()) {
			final List<String> names = list.stream().map(ParseTree::getText).toList();
			final List<CVariable> dummies = names.stream().map(DummyVariable::new).collect(Collectors.toList());
			this.currentDummies.clear();
			for (int i = 0; i < names.size(); i++) {
				this.currentDummies.put(names.get(i), dummies.get(i));
			}
		} else {
			this.currentDummies.clear();
		}
	}

	@Override
	public void exitDefStmt(final Py2CParser.DefStmtContext ctx) {
		final List<TerminalNode> list = ctx.defArgs().IDENTIFIER();
		final CStatement f;
		if (!list.isEmpty()) {
			final List<String> varNames = list.stream().map(ParseTree::getText).toList();
			final List<CVariable> dummies = varNames.stream().map(DummyVariable::new).collect(Collectors.toList());
			f = new CFunction(CType.VOID, dummies, this.currentFunction, this.currentStmts);
		} else {
			f = new CFunction(CType.VOID, this.currentFunction, this.currentStmts);
		}
		if (!this.previousStmts.isEmpty()) {
			this.currentStmts = this.previousStmts.get(this.previousStmts.size() - 1);
		} else {
			this.currentStmts.clear();
		}
		this.previousStmts.remove(this.previousStmts.size() - 1);
		this.functions.put(this.currentFunction, f);
		this.currentFunction = "main";
	}

	@Override
	public void enterForStmt(final Py2CParser.ForStmtContext ctx) {
		final String variableName = ctx.target().getText();
		this.variables.get(this.currentFunction).put(variableName, new IntVariable(variableName));
		this.previousStmts.add(this.currentStmts);
		this.currentStmts = new ArrayList<>();
	}

	@Override
	public void enterIfStmt(final Py2CParser.IfStmtContext ctx) {
		this.previousStmts.add(this.currentStmts);
		this.currentStmts = new ArrayList<>();
	}

	@Override
	public void enterElifStmt(final Py2CParser.ElifStmtContext ctx) {
		this.previousStmts.add(this.currentStmts);
		this.currentStmts = new ArrayList<>();
	}

	@Override
	public void enterElseStmt(final Py2CParser.ElseStmtContext ctx) {
		this.previousStmts.add(this.currentStmts);
		this.currentStmts = new ArrayList<>();
	}

	@Override
	public void exitIfStmt(final Py2CParser.IfStmtContext ctx) {
		final CStatement ifStmt = new IfStatement(this.currentExpr.pop(), this.currentStmts);
		final List<CStatement> transfer = this.previousStmts.get(this.previousStmts.size() - 1);
		int insertIdx = transfer.size() - 1;
		while (insertIdx > 0 && (transfer.get(insertIdx) instanceof ElseStatement || transfer.get(insertIdx) instanceof ElifStatement)) {
			insertIdx--;
		}
		if (insertIdx == transfer.size() - 1) {
			transfer.add(ifStmt);
			this.previousStmts.remove(this.previousStmts.size() - 1);
			this.currentStmts = transfer;
		} else if (!(transfer.get(insertIdx) instanceof ElifStatement) && !(transfer.get(insertIdx) instanceof ElseStatement)) {
			final List<CStatement> head = new ArrayList<>(transfer.subList(0, insertIdx + 1));
			final List<CStatement> rest = new ArrayList<>(transfer.subList(insertIdx + 1, transfer.size()));
			head.add(ifStmt);
			head.addAll(rest);
			this.previousStmts.remove(this.previousStmts.size() - 1);
			this.currentStmts = head;
		} else {
			final List<CStatement> head = new ArrayList<>();
			head.add(ifStmt);
			head.addAll(transfer);
			this.previousStmts.remove(this.previousStmts.size() - 1);
			this.currentStmts = head;
		}
	}

	@Override
	public void exitElifStmt(final Py2CParser.ElifStmtContext ctx) {
		final CStatement elifStmt = new ElifStatement(this.currentExpr.pop(), this.currentStmts);
		this.currentStmts = this.previousStmts.get(this.previousStmts.size() - 2);
		this.currentStmts.add(elifStmt);
		this.currentStmts = this.previousStmts.get(this.previousStmts.size() - 1);
		this.previousStmts.remove(this.previousStmts.size() - 1);
	}

	@Override
	public void exitElseStmt(final Py2CParser.ElseStmtContext ctx) {
		final CStatement elseStmt = new ElseStatement(this.currentStmts);
		this.currentStmts = this.previousStmts.get(this.previousStmts.size() - 2);
		this.currentStmts.add(elseStmt);
		this.currentStmts = this.previousStmts.get(this.previousStmts.size() - 1);
		this.previousStmts.remove(this.previousStmts.size() - 1);
	}

	@Override
	public void enterWhileStmt(final Py2CParser.WhileStmtContext ctx) {
		this.previousStmts.add(this.currentStmts);
		this.currentStmts = new ArrayList<>();
	}

	@Override
	public void exitWhileStmt(final Py2CParser.WhileStmtContext ctx) {
		final CStatement whileStmt = new WhileStatement(this.currentExpr.pop(), this.currentStmts);
		if (!this.previousStmts.isEmpty()) {
			this.currentStmts = this.previousStmts.get(this.previousStmts.size() - 1);
		} else {
			this.currentStmts.clear();
		}
		this.previousStmts.remove(this.previousStmts.size() - 1);
		this.currentStmts.add(whileStmt);
	}

	@Override
	public void exitForStmt(final Py2CParser.ForStmtContext ctx) {
		final String variableName = ctx.target().getText();
		final CStatement forStmt;
		if (Objects.nonNull(ctx.range1Stmt())) {
			forStmt = new ForRangeStatement(variableName, new IntVariable(0), this.currentExpr.pop(), this.currentStmts);
		} else {
			final CExpression to = this.currentExpr.pop();
			final CExpression from = this.currentExpr.pop();
			forStmt = new ForRangeStatement(variableName, from, to, this.currentStmts);
		}
		if (!this.previousStmts.isEmpty()) {
			this.currentStmts = this.previousStmts.get(this.previousStmts.size() - 1);
		} else {
			this.currentStmts.clear();
		}
		this.previousStmts.remove(this.previousStmts.size() - 1);
		this.currentStmts.add(forStmt);
	}

	@Override
	public void exitCallStmt(final Py2CParser.CallStmtContext ctx) {
		final String functionName = ctx.IDENTIFIER().getText();
		final List<CExpression> args = new ArrayList<>();
		final int argN = ctx.callArgs().expression().size();
		for (int i = 0; i < argN; i++) {
			args.add(this.currentExpr.pop());
		}
		Collections.reverse(args);
		final CStatement callStmt;
		if (Py2CStd.STD_FUNCTIONS.containsKey(functionName)) {
			callStmt = new CallStatement(Py2CStd.STD_FUNCTIONS.getOrDefault(functionName, Py2CStd.makeDefaultFunction(functionName)), args);
		} else {
			callStmt = new CallStatement(Py2CStd.makeDefaultFunction(functionName), args, (CFunction) this.functions.get(functionName));
		}
		this.currentStmts.add(callStmt);
	}

	@Override
	public void exitAssignmentStmt(final Py2CParser.AssignmentStmtContext ctx) {
		final Map<String, CVariable> currentFunctionVars = this.variables.get(this.currentFunction);
		final String variableName = ctx.target().getText();
		final CExpression expression = this.currentExpr.pop();
		if (!currentFunctionVars.containsKey(variableName)) {
			switch (expression.getType()) {
				case INT -> currentFunctionVars.put(variableName, new IntVariable(variableName));
				case FLOAT -> currentFunctionVars.put(variableName, new FloatVariable(variableName));
				case STRING -> currentFunctionVars.put(variableName, new StringVariable(variableName, null));
				default -> throw new Py2CUnsupportedVariableTypeException();
			}
		}
		this.currentStmts.add(new AssignmentStatement(variableName, expression));
	}

	@Override
	public void exitExpressionComp(final Py2CParser.ExpressionCompContext ctx) {
		final CExpression right = this.currentExpr.pop();
		if (Objects.nonNull(ctx.S_COMP())) {
			final CExpression left = this.currentExpr.pop();
			switch (ctx.S_COMP().getText()) {
				case "!=" -> this.currentExpr.push(new NotEquals(left, right));
				case "==" -> this.currentExpr.push(new Equals(left, right));
				case "<=" -> this.currentExpr.push(new NotGreaterThan(left, right));
				case ">=" -> this.currentExpr.push(new NotLessThan(left, right));
				case "<" -> this.currentExpr.push(new LessThan(left, right));
				case ">" -> this.currentExpr.push(new GreaterThan(left, right));
				default -> throw new Py2CUnsupportedComparisonOperatorException();
			}
		} else {
			this.currentExpr.push(right);
		}
	}

	@Override
	public void enterExpressionBitOr(final Py2CParser.ExpressionBitOrContext ctx) {
		this.previousExprs.push(this.currentExpr);
		this.currentExpr = new ArrayDeque<>();
	}

	@Override
	public void exitExpressionBitOr(final Py2CParser.ExpressionBitOrContext ctx) {
		final int opN = ctx.expressionBitXor().size() - 1;
		CExpression left = this.currentExpr.pollLast();
		for (int i = 0; i < opN; i++) {
			final CExpression right = this.currentExpr.pollLast();
			left = new BitwiseOr(left, right);
		}
		if (!this.previousExprs.isEmpty()) {
			this.currentExpr = this.previousExprs.pop();
		} else {
			this.currentExpr.clear();
		}
		this.currentExpr.push(left);
	}

	@Override
	public void enterExpressionBitXor(final Py2CParser.ExpressionBitXorContext ctx) {
		this.previousExprs.push(this.currentExpr);
		this.currentExpr = new ArrayDeque<>();
	}

	@Override
	public void exitExpressionBitXor(final Py2CParser.ExpressionBitXorContext ctx) {
		final int opN = ctx.expressionBitAnd().size() - 1;
		CExpression left = this.currentExpr.pollLast();
		for (int i = 0; i < opN; i++) {
			final CExpression right = this.currentExpr.pollLast();
			left = new BitwiseXor(left, right);
		}
		if (!this.previousExprs.isEmpty()) {
			this.currentExpr = this.previousExprs.pop();
		} else {
			this.currentExpr.clear();
		}
		this.currentExpr.push(left);
	}

	@Override
	public void enterExpressionBitAnd(final Py2CParser.ExpressionBitAndContext ctx) {
		this.previousExprs.push(this.currentExpr);
		this.currentExpr = new ArrayDeque<>();
	}

	@Override
	public void exitExpressionBitAnd(final Py2CParser.ExpressionBitAndContext ctx) {
		final int opN = ctx.expressionAddSub().size() - 1;
		CExpression left = this.currentExpr.pollLast();
		for (int i = 0; i < opN; i++) {
			final CExpression right = this.currentExpr.pollLast();
			left = new BitwiseAnd(left, right);
		}
		if (!this.previousExprs.isEmpty()) {
			this.currentExpr = this.previousExprs.pop();
		} else {
			this.currentExpr.clear();
		}
		this.currentExpr.push(left);
	}

	@Override
	public void enterExpressionAddSub(final Py2CParser.ExpressionAddSubContext ctx) {
		this.previousExprs.push(this.currentExpr);
		this.currentExpr = new ArrayDeque<>();
	}

	@Override
	public void exitExpressionAddSub(final Py2CParser.ExpressionAddSubContext ctx) {
		final int opN = ctx.expressionMulDiv().size() - 1;
		CExpression left = this.currentExpr.pollLast();
		for (int i = 0; i < opN; i++) {
			final CExpression right = this.currentExpr.pollLast();
			final String op = ctx.sAddSub().get(i).getText();
			switch (op) {
				case "+" -> left = new Add(left, right);
				case "-" -> left = new Subtract(left, right);
				default -> throw new Py2CUnsupportedBinaryOperatorException(op);
			}
		}
		if (!this.previousExprs.isEmpty()) {
			this.currentExpr = this.previousExprs.pop();
		} else {
			this.currentExpr.clear();
		}
		this.currentExpr.push(left);
	}

	@Override
	public void enterExpressionMulDiv(final Py2CParser.ExpressionMulDivContext ctx) {
		this.previousExprs.push(this.currentExpr);
		this.currentExpr = new ArrayDeque<>();
	}

	@Override
	public void exitExpressionMulDiv(final Py2CParser.ExpressionMulDivContext ctx) {
		final int opN = ctx.expressionPow().size() - 1;
		CExpression left = this.currentExpr.pollLast();
		for (int i = 0; i < opN; i++) {
			final CExpression right = this.currentExpr.pollLast();
			final String op = ctx.sMulDiv().get(i).getText();
			switch (op) {
				case "*" -> left = new Multiply(left, right);
				case "/" -> left = new Divide(left, right);
				default -> throw new Py2CUnsupportedBinaryOperatorException(op);
			}
		}
		if (!this.previousExprs.isEmpty()) {
			this.currentExpr = this.previousExprs.pop();
		} else {
			this.currentExpr.clear();
		}
		this.currentExpr.push(left);
	}

	@Override
	public void exitExpressionPow(final Py2CParser.ExpressionPowContext ctx) {
		final CExpression left = this.currentExpr.pop();
		if (Objects.nonNull(ctx.S_POW())) {
			final CExpression right = this.currentExpr.pop();
			this.currentExpr.push(new Power(right, left));
		} else {
			this.currentExpr.push(left);
		}
	}

	@Override
	public void exitAtom(final Py2CParser.AtomContext ctx) {
		if (Objects.nonNull(ctx.NUMBER())) {
			final String txt = ctx.NUMBER().getText();
			if (txt.contains(".")) {
				final CVariable f = new FloatVariable(Float.parseFloat(txt));
				this.currentExpr.push(f);
			} else {
				final CVariable i = new IntVariable(Integer.parseInt(txt));
				this.currentExpr.push(i);
			}
		} else if (Objects.nonNull(ctx.IDENTIFIER())) {
			final String v = ctx.IDENTIFIER().getText();
			final CVariable varVar;
			if (this.variables.get(this.currentFunction).containsKey(v)) {
				varVar = this.variables.get(this.currentFunction).get(v);
			} else {
				if (this.currentDummies.containsKey(v)) {
					varVar = this.currentDummies.get(v);
				} else {
					varVar = this.variables.get("main").get(v);
				}
			}
			this.currentExpr.push(varVar);
		} else if (Objects.nonNull(ctx.STRING())) {
			final String s = ctx.STRING().getText();
			final CVariable strVar = new StringVariable(s.substring(1, s.length() - 1));
			this.currentExpr.push(strVar);
		} else if (Objects.nonNull(ctx.FALSE())) {
			final CVariable falseVar = new IntVariable(0);
			this.currentExpr.push(falseVar);
		} else if (Objects.nonNull(ctx.TRUE())) {
			final CVariable trueVar = new IntVariable(1);
			this.currentExpr.push(trueVar);
		}
	}
}
