package expression.exceptions;

import expression.*;
import expression.exceptions.custom.ParserException;
import expression.util.BaseParser;
import expression.util.CharSource;
import expression.parser.ExpressionType;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class CheckedParsersizing extends BaseParser {
	private final List<CommonExpression> atoms;
	private final List<ExpressionType> operators;
	private int atomsPointer, operatorsPointer;
	private final Set<Integer> indexesUsed;
	private static final Set<ExpressionType> BINARY_OPERATORS_SET;

	static {
		BINARY_OPERATORS_SET = new HashSet<>();
		BINARY_OPERATORS_SET.add(ExpressionType.ADD);
		BINARY_OPERATORS_SET.add(ExpressionType.SUBTRACT);
		BINARY_OPERATORS_SET.add(ExpressionType.MULTIPLY);
		BINARY_OPERATORS_SET.add(ExpressionType.DIVIDE);
	}

	public CheckedParsersizing(final CharSource source) {
		super(source);
		this.atoms = new ArrayList<>();
		this.operators = new ArrayList<>();
		this.atomsPointer = -1;
		this.operatorsPointer = -1;
		this.indexesUsed = new HashSet<>();
	}

	public CommonExpression parse() {
		this.fill();
		this.check();
		return this.returnLowestPriority();
	}

	private void check() throws ParserException {
		for (int i = 0; i < this.operators.size() - 1; i++) {
			if ((this.operators.get(i) == ExpressionType.CONST && this.operators.get(i + 1) == ExpressionType.CONST) || (BINARY_OPERATORS_SET.contains(this.operators.get(i)) && BINARY_OPERATORS_SET.contains(this.operators.get(i + 1)))) {
				throw new ParserException("Can't be CONST and CONST or OPERATOR and OPERATOR: " + i + ", " + (i + 1));
			}
		}
	}

	private CommonExpression returnLowestPriority() {
		CommonExpression result = this.returnHighestPriority();
		final ExpressionType currentOperator = this.next();
		switch (currentOperator) {
			case ADD, SUBTRACT, MIN, MAX -> {
				switch (currentOperator) {
					case ADD -> result = new CheckedAdd(result, this.returnHighestPriority());
					case SUBTRACT -> result = new CheckedSubtract(result, this.returnHighestPriority());
					case MIN -> result = new Min(result, this.returnHighestPriority());
					case MAX -> result = new Max(result, this.returnHighestPriority());
				}
				while (this.look() != ExpressionType.END) {
					switch (this.next()) {
						case ADD -> result = new CheckedAdd(result, this.returnHighestPriority());
						case SUBTRACT -> result = new CheckedSubtract(result, this.returnHighestPriority());
						case MIN -> result = new Min(result, this.returnHighestPriority());
						case MAX -> result = new Max(result, this.returnHighestPriority());
						case BRACKET_CLOSE -> {
							return result;
						}
					}
				}
			}
			case MULTIPLY, DIVIDE -> {
				switch (currentOperator) {
					case MULTIPLY -> result = new CheckedMultiply(result, this.returnAtom());
					case DIVIDE -> result = new CheckedDivide(result, this.returnAtom());
				}
				if (this.look() != ExpressionType.END) {
					switch (this.next()) {
						case MULTIPLY -> result = new CheckedMultiply(result, this.returnAtom());
						case DIVIDE -> result = new CheckedDivide(result, this.returnAtom());
						case SUBTRACT -> result = new CheckedSubtract(result, this.returnAtom());
						case ADD -> result = new CheckedAdd(result, this.returnAtom());
						case MIN -> result = new Min(result, this.returnAtom());
						case MAX -> result = new Max(result, this.returnAtom());
						case BRACKET_CLOSE -> {
							return result;
						}
					}
				}
			}
			case BRACKET_CLOSE -> {
				return result;
			}
		}
		if (this.look() == ExpressionType.BRACKET_CLOSE) {
			this.take();
		}
		return result;
	}

	private CommonExpression returnHighestPriority() {
		CommonExpression result = this.returnAtom();
		if (this.look() == ExpressionType.MULTIPLY || this.look() == ExpressionType.DIVIDE) {
			switch (this.next()) {
				case MULTIPLY -> result = new CheckedMultiply(result, this.returnAtom());
				case DIVIDE -> result = new CheckedDivide(result, this.returnAtom());
			}
			while (this.look() != ExpressionType.END) {
				switch (this.next()) {
					case MULTIPLY -> result = new CheckedMultiply(result, this.returnAtom());
					case DIVIDE -> result = new CheckedDivide(result, this.returnAtom());
					case BRACKET_CLOSE, ADD, SUBTRACT, MIN, MAX -> {
						this.back();
						return result;
					}
				}
			}
		}
		return result;
	}

	private CommonExpression returnAtom() throws ParserException {
		CommonExpression result = null;
		switch (this.next()) {
			case CONST, VARIABLE -> {
				if (this.indexesUsed.contains(this.atomsPointer)) {
					this.atomsPointer++;
				}
				if (this.atomsPointer == -1) {
					this.atomsPointer++;
				}
				this.indexesUsed.add(this.atomsPointer);
				result = this.atoms.get(this.atomsPointer);
			}
			case NEGATE -> {
				if (this.look() != ExpressionType.CONST) {
					result = new CheckedNegate(this.returnAtom());
				} else {
					final Const temp = (Const) this.grab();
					if (temp != null) {
						if (temp.evaluate(0) < 0) {
							result = new CheckedNegate(this.returnAtom());
						} else {
							result = this.returnAtom();
						}
					}
				}
			}
			case BRACKET_OPEN -> result = this.returnLowestPriority();
			case L0 -> result = new L0(this.returnAtom());
			case T0 -> result = new T0(this.returnAtom());
		}
		if (result != null) {
			return result;
		} else {
			throw new ParserException("Something is wrong, result can't be null");
		}
	}

	private ExpressionType next() {
		if (this.operatorsPointer + 1 < this.operators.size()) {
			return this.operators.get(++this.operatorsPointer);
		} else {
			return ExpressionType.END;
		}
	}

	private ExpressionType look() {
		if (this.operatorsPointer + 1 < this.operators.size()) {
			return this.operators.get(this.operatorsPointer + 1);
		} else {
			return ExpressionType.END;
		}
	}

	private CommonExpression grab() {
		if (this.atomsPointer + 1 < this.atoms.size()) {
			return this.atoms.get(this.atomsPointer + 1);
		} else {
			return this.atoms.get(this.atomsPointer);
		}
	}

	private void back() {
		this.operatorsPointer--;
	}

	private void fill() throws ParserException {
		while (!this.eof()) {
			if (this.take('+')) {
				this.operators.add(ExpressionType.ADD);
				continue;
			}
			if (this.take('-')) {
				if (this.operators.size() != 0) {
					if (this.operators.get(this.operators.size() - 1) == ExpressionType.CONST || this.operators.get(this.operators.size() - 1) == ExpressionType.VARIABLE || this.operators.get(this.operators.size() - 1) == ExpressionType.BRACKET_CLOSE) {
						this.operators.add(ExpressionType.SUBTRACT);
					} else {
						this.operators.add(ExpressionType.NEGATE);
					}
				} else {
					this.operators.add(ExpressionType.NEGATE);
				}
				continue;
			}
			if (this.take('*')) {
				this.operators.add(ExpressionType.MULTIPLY);
				continue;
			}
			if (this.take('/')) {
				this.operators.add(ExpressionType.DIVIDE);
				continue;
			}
			if (this.take('(')) {
				this.operators.add(ExpressionType.BRACKET_OPEN);
				continue;
			}
			if (this.take(')')) {
				this.operators.add(ExpressionType.BRACKET_CLOSE);
				continue;
			}
			if (this.take('t')) {
				if (this.expect("0")) {
					this.operators.add(ExpressionType.T0);
					if (this.lookNext() != ' ' && this.lookNext() != '(') {
						throw new ParserException("Must be ws after t0: " + this.lookNext());
					}
					continue;
				} else {
					throw new ParserException("Maybe you mean T0: " + this.get());
				}
			}
			if (this.take('l')) {
				if (this.expect("0")) {
					this.operators.add(ExpressionType.L0);
					if (this.lookNext() != ' ' && this.lookNext() != '(') {
						throw new ParserException("Must be ws after l0: " + this.lookNext());
					}
					continue;
				} else {
					throw new ParserException("Maybe you mean L0: " + this.get());
				}
			}
			if (this.take('m')) {
				if (this.expect("in")) {
					this.operators.add(ExpressionType.MIN);
					continue;
				} else {
					if (this.expect("ax")) {
						this.operators.add(ExpressionType.MAX);
						continue;
					} else {
						throw new ParserException("Maybe you mean MAX or MIN: " + this.get());
					}
				}
			}
			if (this.between('0')) {
				if (this.operators.size() != 0) {
					if (this.operators.get(this.operators.size() - 1) == ExpressionType.NEGATE) {
						this.atoms.add(this.returnConst(-1));
						this.operators.remove(this.operators.size() - 1);
					} else {
						this.atoms.add(this.returnConst(1));
					}
				} else {
					this.atoms.add(this.returnConst(1));
				}
				this.operators.add(ExpressionType.CONST);
				continue;
			}
			if (this.take(' ')) {
				continue;
			}
			final char c = this.take();
			if (this.checkChar(c)) {
				this.atoms.add(this.returnVariable(c));
				this.operators.add(ExpressionType.VARIABLE);
				continue;
			}
			throw new ParserException("Unexpected symbol, no such thing in my alphabet found: " + c);
		}
	}

	private CommonExpression returnVariable(final char c) {
		return new Variable(String.valueOf(c));
	}

	private CommonExpression returnConst(final int k) throws ParserException {
		final StringBuilder sb = new StringBuilder();

		if (this.take('0')) {
			return new Const(0);
		} else {
			if (this.between('1')) {
				sb.append(this.take());
			} else {
				throw new ParserException("Number can't be something with not a digits: " + this.returnChar());
			}
			while (this.between('0')) {
				sb.append(this.take());
			}
		}

		String s = sb.toString();
		if (k == -1) {
			s = "-" + s;
		}

		try {
			return new Const(Integer.parseInt(s));
		} catch (final NumberFormatException err) {
			throw new ParserException("Wrong number, it's overflowed");
		}
	}
}
