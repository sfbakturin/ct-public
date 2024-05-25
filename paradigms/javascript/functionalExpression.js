"use strict";

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

const cnst = (a) => () => {
	return a;
}

const pi = cnst(Math.PI);
const e = cnst(Math.E);

function compare(a, b) {
	if (a < b) {
		return -1;
	}
	if (a > b) {
		return 1;
	}
	return 0;
}

const unaryOperator = (f) => (m) => (x, y, z) => {
	return f(m(x, y, z));
}

const binaryOperator = (f) => (l, r) => (x, y, z) => {
	return f(l(x, y, z), r(x, y, z));
}

const ternaryOperator = (f) => (l, m, r) => (x, y, z) => {
	return f(l(x, y, z), m(x, y, z), r(x, y, z));
}

const negate = unaryOperator((a) => -a);
const abs = unaryOperator((a) => Math.abs(a));
const add = binaryOperator((a, b) => a + b);
const subtract = binaryOperator((a, b) => a - b);
const multiply = binaryOperator((a, b) => a * b);
const divide = binaryOperator((a, b) => a / b);

const iff = ternaryOperator((a, b, c) => a >= 0 ? b : c);
const avg3 = ternaryOperator((a, b, c) => (a + b + c) / 3);

const med5 = (...args) => (x, y, z) => {
	const b = args.sort((a, b) => compare(a(x, y, z), b(x, y, z)));
	return b[2](x, y, z);
}

const variable = (a) => (...args) => {
	const b = {"x": 0, "y": 1, "z": 2};
	return args[b[a]];
}

const parse = (a) => {
	const stack = [];
	const b = a.split(" ");
	const operationsBinary = {"+": add, "-": subtract, "*": multiply, "/": divide};
	const operationsUnary = {"negate": negate, "abs": abs};
	const operationsTernary = {"iff": iff, "avg3": avg3};
	const operationsVariables = {"x": variable, "y": variable, "z": variable};
	const operationsConstants = {"pi": pi, "e": e};
	for (const v of b) {
		if (v === "med5") {
			const a5 = stack.pop();
			const a4 = stack.pop();
			const a3 = stack.pop();
			const a2 = stack.pop();
			stack.push(med5(stack.pop(), a2, a3, a4, a5));
			continue;
		}
		if (v in operationsTernary) {
			const r = stack.pop();
			const m = stack.pop();
			stack.push(operationsTernary[v](stack.pop(), m, r));
			continue;
		}
		if (v in operationsBinary) {
			const r = stack.pop();
			stack.push(operationsBinary[v](stack.pop(), r));
			continue;
		}
		if (v in operationsUnary) {
			stack.push(operationsUnary[v](stack.pop()));
			continue;
		}
		if (v in operationsVariables) {
			stack.push(operationsVariables[v](v));
			continue;
		}
		if (v in operationsConstants) {
			stack.push(operationsConstants[v]);
			continue;
		}
		if (v !== "") {
			stack.push(cnst(parseInt(v)));
		}
	}
	return stack.pop();
}
