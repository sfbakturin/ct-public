"use strict";

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

const operationsBinary = {"+": Add, "-": Subtract, "*": Multiply, "/": Divide};
const operationsUnary = {"negate": Negate, "sinh": Sinh, "cosh": Cosh};
const operationsVariables = {"x": new Variable("x"), "y": new Variable("y"), "z": new Variable("z")};

const alphabet = ["x", "y", "z", "+", "-", "*", "/", '(', ')', "n", "e", "g", "a", "t", "s", "i", "n", "h", "c", "o"];
const characterMapper = {
	"+": "+",
	"-": "-",
	"*": "*",
	"/": "/",
	"n": "negate",
	"s": "sinh",
	"c": "cosh"
}
const bracketsMapper = {
	"(": 1,
	")": -1
}

const operatorsPriority = {
	"(": -1024,
	"+": -512,
	"-": -512,
	"*": -256,
	"/": -256,
	"negate": -128,
	"sinh": -128,
	"cosh": -128
}

const DiffBinary = {
	add: function (l, r, a) {
		return new Add(l.diff(a), r.diff(a));
	},
	sub: function (l, r, a) {
		return new Subtract(l.diff(a), r.diff(a));
	},
	mul: function (l, r, a) {
		return new Add(new Multiply(l.diff(a), r), new Multiply(l, r.diff(a)));
	},
	div: function (l, r, a) {
		return new Divide(new Subtract(new Multiply(l.diff(a), r), new Multiply(l, r.diff(a))), new Multiply(r, r));
	}
}

const DiffUnary = {
	neg: function (m, a) {
		return new Multiply(new Const(-1), m.diff(a));
	},
	sinh: function (m, a) {
		return new Multiply(m.diff(a), new Cosh(m));
	},
	cosh: function (m, a) {
		return new Multiply(m.diff(a), new Sinh(m));
	}
}

const BinaryPrototype = (f) => (a, b) => (x, y, z) => {
	return f(a.evaluate(x, y, z), b.evaluate(x, y, z));
}

function Const(input) {
	this.value = input;
}

function Variable(input) {
	this.value = input;
}

function Add(left, right) {
	this.l = left;
	this.r = right;
}

function Subtract(left, right) {
	this.l = left;
	this.r = right;
}

function Multiply(left, right) {
	this.l = left;
	this.r = right;
}

function Divide(left, right) {
	this.l = left;
	this.r = right;
}

function Negate(input) {
	this.value = input;
}

function Min3(a, b, c) {
	this.l = a;
	this.m = b;
	this.r = c;
}

function Max5(a, b, c, d, e) {
	this.a1 = a;
	this.a2 = b;
	this.a3 = c;
	this.a4 = d;
	this.a5 = e;
}

function Sinh(a) {
	this.value = a;
}

function Cosh(a) {
	this.value = a;
}

Const.prototype.evaluate = function () {
	return this.value;
}

Variable.prototype.evaluate = function (x, y, z) {
	const map = {"x": x, "y": y, "z": z};
	return map[this.value];
}

Add.prototype.evaluate = function (x, y, z) {
	return BinaryPrototype((a, b) => a + b)(this.l, this.r)(x, y, z);
}

Subtract.prototype.evaluate = function (x, y, z) {
	return BinaryPrototype((a, b) => a - b)(this.l, this.r)(x, y, z);
}

Multiply.prototype.evaluate = function (x, y, z) {
	return BinaryPrototype((a, b) => a * b)(this.l, this.r)(x, y, z);
}

Divide.prototype.evaluate = function (x, y, z) {
	return BinaryPrototype((a, b) => a / b)(this.l, this.r)(x, y, z);
}

Negate.prototype.evaluate = function (x, y, z) {
	return (-1) * this.value.evaluate(x, y, z);
}

Min3.prototype.evaluate = function (x, y, z) {
	return Math.min(this.l.evaluate(x, y, z), this.r.evaluate(x, y, z), this.m.evaluate(x, y, z));
}

Max5.prototype.evaluate = function (x, y, z) {
	return Math.max(this.a1.evaluate(x, y, z), this.a2.evaluate(x, y, z), this.a3.evaluate(x, y, z), this.a4.evaluate(x, y, z), this.a5.evaluate(x, y, z));
}

Sinh.prototype.evaluate = function (x, y, z) {
	return Math.sinh(this.value.evaluate(x, y, z));
}

Cosh.prototype.evaluate = function (x, y, z) {
	return Math.cosh(this.value.evaluate(x, y, z));
}

Const.prototype.toString = function () {
	return this.value.toString();
}

Variable.prototype.toString = function () {
	return this.value;
}

Add.prototype.toString = function () {
	return binaryOperatorToString(this.l, this.r, "+");
}

Subtract.prototype.toString = function () {
	return binaryOperatorToString(this.l, this.r, "-");
}

Multiply.prototype.toString = function () {
	return binaryOperatorToString(this.l, this.r, "*");
}

Divide.prototype.toString = function () {
	return binaryOperatorToString(this.l, this.r, "/");
}

Negate.prototype.toString = function () {
	return unaryOperatorToString(this.value, "negate");
}

Min3.prototype.toString = function () {
	return this.l.toString() + " " + this.m.toString() + " " + this.r.toString() + " min3";
}

Max5.prototype.toString = function () {
	return this.a1.toString() + " " + this.a2.toString() + " " + this.a3.toString() + " " + this.a4.toString() + " " + this.a5.toString() + " max5"
}

Sinh.prototype.toString = function () {
	return unaryOperatorToString(this.value, "sinh");
}

Cosh.prototype.toString = function () {
	return unaryOperatorToString(this.value, "cosh");
}

function unaryOperatorToString(v, f) {
	return v.toString() + " " + f;
}

Sinh.prototype.prefix = function () {
	return unaryOperatorToStringPrefix(this.value, "sinh");
}

Cosh.prototype.prefix = function () {
	return unaryOperatorToStringPrefix(this.value, "cosh");
}

Negate.prototype.prefix = function () {
	return unaryOperatorToStringPrefix(this.value, "negate");
}

function unaryOperatorToStringPrefix(v, f) {
	return "(" + f + " " + v.prefix() + ")";
}

Const.prototype.diff = function () {
	return new Const(0);
}

Variable.prototype.diff = function (a) {
	return a === this.value ? new Const(1) : new Const(0);
}

Add.prototype.diff = function (a) {
	return DiffBinary.add(this.l, this.r, a);
}

Subtract.prototype.diff = function (a) {
	return DiffBinary.sub(this.l, this.r, a);
}

Multiply.prototype.diff = function (a) {
	return DiffBinary.mul(this.l, this.r, a);
}

Divide.prototype.diff = function (a) {
	return DiffBinary.div(this.l, this.r, a);
}

Negate.prototype.diff = function (a) {
	return new DiffUnary.neg(this.value, a);
}

Min3.prototype.diff = function (a) {
	return new Min3(this.l.diff(a), this.m.diff(a), this.r.diff(a));
}

Max5.prototype.diff = function (a) {
	return new Max5(this.a1.diff(a), this.a2.diff(a), this.a3.diff(a), this.a4.diff(a), this.a5.diff(a));
}

Sinh.prototype.diff = function (a) {
	return DiffUnary.sinh(this.value, a);
}

Cosh.prototype.diff = function (a) {
	return DiffUnary.cosh(this.value, a);
}

Const.prototype.prefix = function () {
	return this.value.toString();
}

Variable.prototype.prefix = function () {
	return this.value;
}

Add.prototype.prefix = function () {
	return binaryOperatorToStringPrefix(this.l, this.r, "+");
}

Subtract.prototype.prefix = function () {
	return binaryOperatorToStringPrefix(this.l, this.r, "-");
}

Multiply.prototype.prefix = function () {
	return binaryOperatorToStringPrefix(this.l, this.r, "*");
}

Divide.prototype.prefix = function () {
	return binaryOperatorToStringPrefix(this.l, this.r, "/");
}

function binaryOperatorToString(l, r, f) {
	return l.toString() + " " + r.toString() + " " + f;
}

function binaryOperatorToStringPrefix(l, r, f) {
	return "(" + f + " " + l.prefix() + " " + r.prefix() + ")";
}

const parse = (a) => {
	const stack = [];
	const b = a.split(" ");
	for (const v of b) {
		if (v === "" || v === " ") {
			continue;
		}
		if (v in operationsBinary) {
			if (stack.length < 2) {
				throw new Error("With binary operation stack cannot be length < 2");
			}
			const r = stack.pop();
			const l = stack.pop();
			stack.push(new operationsBinary[v](l, r));
			continue;
		}
		if (v in operationsUnary) {
			if (stack.length < 1) {
				throw new Error("With unary operation stack cannot be length < 1");
			}
			stack.push(new operationsUnary[v](stack.pop()));
			continue;
		}
		if (v in operationsVariables) {
			stack.push(operationsVariables[v]);
			continue;
		}
		if (v === "min3") {
			const r = stack.pop();
			const m = stack.pop();
			stack.push(new Min3(stack.pop(), m, r));
			continue;
		}
		if (v === "max5") {
			const a5 = stack.pop();
			const a4 = stack.pop();
			const a3 = stack.pop();
			const a2 = stack.pop();
			stack.push(new Max5(stack.pop(), a2, a3, a4, a5));
			continue;
		}
		const el = parseInt(v);
		if (!isNaN(el)) {
			stack.push(new Const(parseInt(v)));
		} else {
			throw new Error("No such op was found in my alphabet");
		}
	}
	if (stack.length !== 1) {
		throw new Error("Stack cannot be length not equals 1");
	}
	return stack.pop();
}

const parsePrefix = (a) => {
	function isDigit(a) {
		return (a >= "0" && a <= 9);
	}

	function space(a) {
		return " " + a + " ";
	}

	function stringSource(source) {
		let balance = 0;
		let output = "";
		let flagWhitespace = false;
		for (const c of source) {
			if (alphabet.includes(c) || isDigit(c)) {
				output += c;
				flagWhitespace = false;
				if (c in bracketsMapper) {
					balance += bracketsMapper[c];
				}
				if (balance < 0) {
					throw new Error("No balance with ()");
				}
			} else {
				if (!(/\s/.test(c))) {
					throw new Error("No such symbol was found in my alphabet");
				} else {
					if (!flagWhitespace) {
						output += " ";
					}
					flagWhitespace = !flagWhitespace;
				}
			}
		}
		if (balance !== 0) {
			throw new Error("No balance with ()");
		}
		return output;
	}

	const string = stringSource(a);
	let stack = [];
	let output = "";
	let index = 0;
	let flag = false;

	function getNext() {
		return string.charAt(index + 1);
	}

	function getPrev() {
		return output.charAt(output.length - 1);
	}

	function test(expected) {
		let j = index;
		let k = 0;
		for (; j <= index + expected.length - 1; j++) {
			if (string.charAt(j) !== expected.charAt(k)) {
				throw new Error("No such op is supported!");
			} else {
				k++;
			}
		}
		flag = false;
		j--;
		if (j + 1 < string.length && string.charAt(j + 1) !== " " && string.charAt(j + 1) !== "(") {
			throw new Error("Whitespace problems.");
		}
		index = j;
		while (stack.length > 0 && operatorsPriority[stack[stack.length - 1]] >= operatorsPriority[expected]) {
			output += space(stack.pop());
		}
		stack.push(expected);
	}

	while (index < string.length) {
		const v = string.charAt(index);
		if (isDigit(v)) {
			if (flag) {
				throw new Error("Must be some op after (");
			}
			if (output.length > 0 && !isDigit(getPrev()) && getPrev() !== "-") {
				output += " ";
			}
			output += v;
		} else if (v in operationsVariables) {
			if (flag) {
				throw new Error("Must be some op after (");
			}
			output += space(v);
		} else if (v === "(") {
			flag = true;
			stack.push(v);
		} else if (v === ")") {
			while (stack.length > 0 && stack[stack.length - 1] !== "(") {
				output += space(stack.pop());
			}
			stack.pop();
		} else if (v in characterMapper) {
			if (index + 1 < string.length && v === "-" && getNext() !== " " && getNext() !== "(") {
				output += v;
				index++;
				continue;
			}
			test(characterMapper[v]);
		} else {
			output += v;
		}
		index++;
	}
	while (stack.length > 0) {
		output += " " + stack.pop();
	}
	return parse(output);
}
