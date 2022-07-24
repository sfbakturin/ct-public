package expression.parser;

import expression.Add;
import expression.CommonExpression;
import expression.Const;
import expression.Divide;
import expression.L0;
import expression.Multiply;
import expression.Subtract;
import expression.T0;
import expression.Negate;
import expression.Variable;
import expression.util.BaseParser;
import expression.util.CharSource;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

public class Parsersizing extends BaseParser {
	private final List<CommonExpression> atoms;
	private final List<ExpressionType> operators;
	private int atomsPointer, operatorsPointer;
	private final Set<Integer> indexesUsed;

	public Parsersizing(final CharSource source) {
		super(source);
		this.atoms = new ArrayList<>();
		this.operators = new ArrayList<>();
		this.atomsPointer = -1;
		this.operatorsPointer = -1;
		this.indexesUsed = new HashSet<>();
	}

	public CommonExpression parse() {
		this.fill();
		return this.returnExpression();
	}

	private CommonExpression returnExpression() {
		return this.returnLowestPriority();
	}

	private CommonExpression returnLowestPriority() {
		CommonExpression result = this.returnHighestPriority();
		switch (this.next()) {
			case ADD -> {
				result = new Add(result, this.returnHighestPriority());
				while (this.look() != ExpressionType.END) {
					switch (this.next()) {
						case ADD -> result = new Add(result, this.returnHighestPriority());
						case SUBTRACT -> result = new Subtract(result, this.returnHighestPriority());
						case BRACKET_CLOSE -> {
							return result;
						}
					}
				}
			}
			case SUBTRACT -> {
				result = new Subtract(result, this.returnHighestPriority());
				while (this.look() != ExpressionType.END) {
					switch (this.next()) {
						case ADD -> result = new Add(result, this.returnHighestPriority());
						case SUBTRACT -> result = new Subtract(result, this.returnHighestPriority());
						case BRACKET_CLOSE -> {
							return result;
						}
					}
				}

			}
			case BRACKET_CLOSE -> {
				return result;
			}
			case MULTIPLY -> {
				result = new Multiply(result, this.returnAtom());
				if (this.look() != ExpressionType.END) {
					switch (this.next()) {
						case MULTIPLY -> result = new Multiply(result, this.returnAtom());
						case DIVIDE -> result = new Divide(result, this.returnAtom());
						case SUBTRACT -> result = new Subtract(result, this.returnAtom());
						case ADD -> result = new Add(result, this.returnAtom());
						case BRACKET_CLOSE -> {
							return result;
						}
					}
				}
			}
			case DIVIDE -> {
				result = new Divide(result, this.returnAtom());
				if (this.look() != ExpressionType.END) {
					switch (this.next()) {
						case MULTIPLY -> result = new Multiply(result, this.returnAtom());
						case DIVIDE -> result = new Divide(result, this.returnAtom());
						case SUBTRACT -> result = new Subtract(result, this.returnAtom());
						case ADD -> result = new Add(result, this.returnAtom());
						case BRACKET_CLOSE -> {
							return result;
						}
					}
				}
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
				case MULTIPLY -> {
					result = new Multiply(result, this.returnAtom());
					while (this.look() != ExpressionType.END) {
						switch (this.next()) {
							case MULTIPLY -> result = new Multiply(result, this.returnAtom());
							case DIVIDE -> result = new Divide(result, this.returnAtom());
							case BRACKET_CLOSE, ADD, SUBTRACT -> {
								this.back();
								return result;
							}
						}
					}
				}
				case DIVIDE -> {
					result = new Divide(result, this.returnAtom());
					while (this.look() != ExpressionType.END) {
						switch (this.next()) {
							case MULTIPLY -> result = new Multiply(result, this.returnAtom());
							case DIVIDE -> result = new Divide(result, this.returnAtom());
							case BRACKET_CLOSE, ADD, SUBTRACT -> {
								this.back();
								return result;
							}
						}
					}
				}
			}
		}
		return result;
	}

	private CommonExpression returnAtom() {
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
					result = new Negate(this.returnAtom());
				} else {
					final Const temp = (Const) this.grab();
					if (temp != null) {
						if (temp.evaluate(0) < 0) {
							result = new Negate(this.returnAtom());
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
			throw new AssertionError("Unexpected situation: return null");
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
		--this.operatorsPointer;
	}

	private void fill() {
		while (!this.eof()) {
			if (this.take('+')) {
				this.operators.add(ExpressionType.ADD);
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
				this.take();
				this.operators.add(ExpressionType.T0);
				continue;
			}
			if (this.take('l')) {
				this.take();
				this.operators.add(ExpressionType.L0);
				continue;
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
			final char c = this.take();
			if (c == 'x' || c == 'y' || c == 'z') {
				this.atoms.add(this.returnVariable(c));
				this.operators.add(ExpressionType.VARIABLE);
				continue;
			}
			if (c == ' ') {
				continue;
			}
			throw new AssertionError("No such atom, variable or  operator was found!");
		}
	}

	private CommonExpression returnVariable(final char c) {
		return new Variable(String.valueOf(c));
	}

	private CommonExpression returnConst(final int k) {
		final StringBuilder sb = new StringBuilder();

		if (this.take('0')) {
			return new Const(0);
		} else {
			if (this.between('1')) {
				sb.append(this.take());
			} else {
				throw new AssertionError("Cannot applied with 00 or 0.[1-9].");
			}
			while (this.between('0')) {
				sb.append(this.take());
			}
		}

		final long a = k * Long.parseLong(sb.toString());
		return new Const((int) a);
	}
}
