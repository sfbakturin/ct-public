package expression.generic.assets.parser;

import expression.exceptions.custom.ParserException;
import expression.generic.assets.math.*;
import expression.generic.assets.operation.GenericOperationType;
import expression.util.BaseParser;
import expression.util.CharSource;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class GenericCheckedParsersizing<T> extends BaseParser {
    private final List<GenericExpression<T>> atoms;
    private final List<GenericExpressionType> operators;
    private int atomsPointer, operatorsPointer;
    private final Set<Integer> indexesUsed;
    private final GenericOperationType<T> mode;
    private static final Set<GenericExpressionType> GENERIC_BINARY_OPERATORS_SET;

    static {
        GENERIC_BINARY_OPERATORS_SET = new HashSet<>();
        GENERIC_BINARY_OPERATORS_SET.add(GenericExpressionType.GENERIC_ADD);
        GENERIC_BINARY_OPERATORS_SET.add(GenericExpressionType.GENERIC_SUBTRACT);
        GENERIC_BINARY_OPERATORS_SET.add(GenericExpressionType.GENERIC_MULTIPLY);
        GENERIC_BINARY_OPERATORS_SET.add(GenericExpressionType.GENERIC_DIVIDE);
    }

    public GenericCheckedParsersizing(final CharSource source, final GenericOperationType<T> mode) {
        super(source);
        this.atoms = new ArrayList<>();
        this.operators = new ArrayList<>();
        this.atomsPointer = -1;
        this.operatorsPointer = -1;
        this.indexesUsed = new HashSet<>();
        this.mode = mode;
    }

    public GenericExpression<T> parse() {
        this.fill();
        this.check();
        return this.returnLowestPriority();
    }

    private void check() throws ParserException {
        for (int i = 0; i < this.operators.size() - 1; i++) {
            if ((this.operators.get(i) == GenericExpressionType.GENERIC_CONST && this.operators.get(i + 1) == GenericExpressionType.GENERIC_CONST)
                    || (GENERIC_BINARY_OPERATORS_SET.contains(this.operators.get(i)) && GENERIC_BINARY_OPERATORS_SET.contains(this.operators.get(i + 1)))) {
                throw new ParserException("Can't be CONST and CONST or OPERATOR and OPERATOR: " + i + ", " + (i + 1));
            }
        }
    }

    private GenericExpression<T> returnLowestPriority() {
        GenericExpression<T> result = this.returnHighestPriority();
        final GenericExpressionType currentOperator = this.next();
        switch (currentOperator) {
            case GENERIC_ADD, GENERIC_SUBTRACT, GENERIC_MIN, GENERIC_MAX -> {
                switch (currentOperator) {
                    case GENERIC_ADD -> result = new GenericAdd<>(result, this.returnHighestPriority(), this.mode);
                    case GENERIC_SUBTRACT -> result = new GenericSubtract<>(result, this.returnHighestPriority(), this.mode);
                    case GENERIC_MIN -> result = new GenericMin<>(result, this.returnHighestPriority(), this.mode);
                    case GENERIC_MAX -> result = new GenericMax<>(result, this.returnHighestPriority(), this.mode);
                }
                while (this.look() != GenericExpressionType.GENERIC_END) {
                    switch (this.next()) {
                        case GENERIC_ADD -> result = new GenericAdd<>(result, this.returnHighestPriority(), this.mode);
                        case GENERIC_SUBTRACT -> result = new GenericSubtract<>(result, this.returnHighestPriority(), this.mode);
                        case GENERIC_MIN -> result = new GenericMin<>(result, this.returnHighestPriority(), this.mode);
                        case GENERIC_MAX -> result = new GenericMax<>(result, this.returnHighestPriority(), this.mode);
                        case GENERIC_BRACKET_CLOSE -> {
                            return result;
                        }
                    }
                }
            }
            case GENERIC_MULTIPLY, GENERIC_DIVIDE -> {
                switch (currentOperator) {
                    case GENERIC_MULTIPLY -> result = new GenericMultiply<>(result, this.returnAtom(), this.mode);
                    case GENERIC_DIVIDE -> result = new GenericDivide<>(result, this.returnAtom(), this.mode);
                }
                if (this.look() != GenericExpressionType.GENERIC_END) {
                    switch (this.next()) {
                        case GENERIC_MULTIPLY -> result = new GenericMultiply<>(result, this.returnAtom(), this.mode);
                        case GENERIC_DIVIDE -> result = new GenericDivide<>(result, this.returnAtom(), this.mode);
                        case GENERIC_SUBTRACT -> result = new GenericSubtract<>(result, this.returnAtom(), this.mode);
                        case GENERIC_ADD -> result = new GenericAdd<>(result, this.returnAtom(), this.mode);
                        case GENERIC_MIN -> result = new GenericMin<>(result, this.returnAtom(), this.mode);
                        case GENERIC_MAX -> result = new GenericMax<>(result, this.returnAtom(), this.mode);
                        case GENERIC_BRACKET_CLOSE -> {
                            return result;
                        }
                    }
                }
            }
            case GENERIC_BRACKET_CLOSE -> {
                return result;
            }
        }
        if (this.look() == GenericExpressionType.GENERIC_BRACKET_CLOSE) {
            this.take();
        }
        return result;
    }

    private GenericExpression<T> returnHighestPriority() {
        GenericExpression<T> result = this.returnAtom();
        if (this.look() == GenericExpressionType.GENERIC_MULTIPLY || this.look() == GenericExpressionType.GENERIC_DIVIDE) {
            switch (this.next()) {
                case GENERIC_MULTIPLY -> result = new GenericMultiply<>(result, this.returnAtom(), this.mode);
                case GENERIC_DIVIDE -> result = new GenericDivide<>(result, this.returnAtom(), this.mode);
            }
            while (this.look() != GenericExpressionType.GENERIC_END) {
                switch (this.next()) {
                    case GENERIC_MULTIPLY -> result = new GenericMultiply<>(result, this.returnAtom(), this.mode);
                    case GENERIC_DIVIDE -> result = new GenericDivide<>(result, this.returnAtom(), this.mode);
                    case GENERIC_BRACKET_CLOSE, GENERIC_ADD, GENERIC_SUBTRACT, GENERIC_MIN, GENERIC_MAX -> {
                        this.back();
                        return result;
                    }
                }
            }
        }
        return result;
    }

    private GenericExpression<T> returnAtom() throws ParserException {
        GenericExpression<T> result = null;
        switch (this.next()) {
            case GENERIC_CONST, GENERIC_VARIABLE -> {
                if (this.indexesUsed.contains(this.atomsPointer)) {
                    this.atomsPointer++;
                }
                if (this.atomsPointer == -1) {
                    this.atomsPointer++;
                }
                this.indexesUsed.add(this.atomsPointer);
                result = this.atoms.get(this.atomsPointer);
            }
            case GENERIC_NEGATE -> {
                if (this.look() != GenericExpressionType.GENERIC_CONST) {
                    result = new GenericNegate<>(this.returnAtom(), this.mode);
                } else {
                    final GenericConst temp = (GenericConst) this.grab();
                    final int argument = Integer.parseInt(temp.evaluate());
                    if (argument < 0) {
                        result = new GenericNegate<>(this.returnAtom(), this.mode);
                    } else {
                        result = this.returnAtom();
                    }
                }
            }
            case GENERIC_BRACKET_OPEN -> result = this.returnLowestPriority();
            case GENERIC_COUNT -> result = new GenericCount<>(this.returnAtom(), this.mode);
        }
        if (result != null) {
            return result;
        } else {
            throw new ParserException("Something is wrong, result can't be null");
        }
    }

    private GenericExpressionType next() {
        if (this.operatorsPointer + 1 < this.operators.size()) {
            return this.operators.get(++this.operatorsPointer);
        } else {
            return GenericExpressionType.GENERIC_END;
        }
    }

    private GenericExpressionType look() {
        if (this.operatorsPointer + 1 < this.operators.size()) {
            return this.operators.get(this.operatorsPointer + 1);
        } else {
            return GenericExpressionType.GENERIC_END;
        }
    }

    private GenericExpression<T> grab() {
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
                this.operators.add(GenericExpressionType.GENERIC_ADD);
                continue;
            }
            if (this.take('-')) {
                if (this.operators.size() != 0) {
                    if (this.operators.get(this.operators.size() - 1) == GenericExpressionType.GENERIC_CONST || this.operators.get(this.operators.size() - 1) == GenericExpressionType.GENERIC_VARIABLE || this.operators.get(this.operators.size() - 1) == GenericExpressionType.GENERIC_BRACKET_CLOSE) {
                        this.operators.add(GenericExpressionType.GENERIC_SUBTRACT);
                    } else {
                        this.operators.add(GenericExpressionType.GENERIC_NEGATE);
                    }
                } else {
                    this.operators.add(GenericExpressionType.GENERIC_NEGATE);
                }
                continue;
            }
            if (this.take('*')) {
                this.operators.add(GenericExpressionType.GENERIC_MULTIPLY);
                continue;
            }
            if (this.take('/')) {
                this.operators.add(GenericExpressionType.GENERIC_DIVIDE);
                continue;
            }
            if (this.take('(')) {
                this.operators.add(GenericExpressionType.GENERIC_BRACKET_OPEN);
                continue;
            }
            if (this.take(')')) {
                this.operators.add(GenericExpressionType.GENERIC_BRACKET_CLOSE);
                continue;
            }
            if (this.take('c')) {
                if (this.expect("ount")) {
                    this.operators.add(GenericExpressionType.GENERIC_COUNT);
                    if (this.lookNext() != ' ' && this.lookNext() != '(') {
                        throw new ParserException("Must be ws after count: " + this.lookNext());
                    }
                    continue;
                } else {
                    throw new ParserException("Maybe you mean COUNT: " + this.get());
                }
            }
            if (this.take('m')) {
                if (this.expect("in")) {
                    this.operators.add(GenericExpressionType.GENERIC_MIN);
                    continue;
                } else {
                    if (this.expect("ax")) {
                        this.operators.add(GenericExpressionType.GENERIC_MAX);
                        continue;
                    } else {
                        throw new ParserException("Maybe you mean MAX or MIN: " + this.get());
                    }
                }
            }
            if (this.between('0')) {
                if (this.operators.size() != 0) {
                    if (this.operators.get(this.operators.size() - 1) == GenericExpressionType.GENERIC_NEGATE) {
                        this.atoms.add(this.returnConst(-1));
                        this.operators.remove(this.operators.size() - 1);
                    } else {
                        this.atoms.add(this.returnConst(1));
                    }
                } else {
                    this.atoms.add(this.returnConst(1));
                }
                this.operators.add(GenericExpressionType.GENERIC_CONST);
                continue;
            }
            if (this.take(' ')) {
                continue;
            }
            final char c = this.take();
            if (this.checkChar(c)) {
                this.atoms.add(this.returnVariable(c));
                this.operators.add(GenericExpressionType.GENERIC_VARIABLE);
                continue;
            }
            throw new ParserException("Unexpected symbol, no such thing in my alphabet found: " + c);
        }
    }

    private GenericVariable<T> returnVariable(final char c) {
        return new GenericVariable<>(String.valueOf(c));
    }

    private GenericExpression<T> returnConst(final int k) throws ParserException {
        final StringBuilder sb = new StringBuilder();

        if (this.take('0')) {
            return this.mode.createConst("0");
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

        return this.mode.createConst(s);
    }
}

