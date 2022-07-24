
%
% @author Saveliy Bakturin
% <p>
% Don't write off, if you don't wanna be banned!
%

lookup(K, [(K, V) | _], V).
lookup(K, [_ | T], V) :- lookup(K, T, V).

nonvar(V, T) :- var(V).
nonvar(V, T) :- nonvar(V), call(T).

concat([], B, B).
concat([H | T], B, [H | R]) :- concat(T, B, R).

variable(NAME, variable(NAME)).
const(VALUE, const(VALUE)).

operation(NAME, LEFT, RIGHT).
operation(NAME, MIDDLE).

evaluate(variable(NAME), MAP, RESULT) :-
	atom_chars(NAME, CHARS),
	element(1, CHARS, STR),
	lookup(STR, [('X', 'x'), ('Y', 'y'), ('Z', 'z'), ('x', 'x'), ('y', 'y'), ('z', 'z')], X),
	lookup(X, MAP, RESULT), !.
evaluate(const(VALUE), _, VALUE).
evaluate(operation(NAME, LEFT, RIGHT), MAP, RESULT) :-
	evaluate(LEFT, MAP, LEFT_RESULT),
	evaluate(RIGHT, MAP, RIGHT_RESULT),
	evaluate(NAME, LEFT_RESULT, RIGHT_RESULT, RESULT), !.
evaluate(operation(NAME, MIDDLE), MAP, RESULT) :-
	evaluate(MIDDLE, MAP, MIDDLE_RESULT),
	evaluate(NAME, MIDDLE_RESULT, RESULT), !.

evaluate(op_add, LEFT, RIGHT, RESULT) :-
	RESULT is LEFT + RIGHT.
evaluate(op_subtract, LEFT, RIGHT, RESULT) :-
	RESULT is LEFT - RIGHT.
evaluate(op_multiply, LEFT, RIGHT, RESULT) :-
	RESULT is LEFT * RIGHT.
evaluate(op_divide, LEFT, RIGHT, RESULT) :-
	RESULT is LEFT / RIGHT.
evaluate(op_negate, MIDDLE, RESULT) :-
	RESULT is (-1) * MIDDLE.

expr_term(variable(NAME), NAME) :-
	atom(NAME).
expr_term(const(VALUE), VALUE) :-
	number(VALUE).
expr_term(operation(OPERATION, MIDDLE), RESULT) :-
	R =.. [OPERATION, MIDDLE_RESULT],
	expr_term(MIDDLE, MIDDLE_RESULT).
expr_term(operation(OPERATION, LEFT, RIGHT), RESULT) :-
	R =.. [OPERATION, LEFT_RESULT, RIGHT_RESULT],
	expr_term(LEFT, LEFT_RESULT),
	expr_term(RIGHT, RIGHT_RESULT).

expr_text(EXPRESSION, STRING) :-
	ground(EXPRESSION),
	expr_term(EXPRESSION, TEXT),
	text_term(STRING, TEXT).
expr_text(EXPRESSION, STRING) :-
	atom(STRING),
	text_term(STRING, TEXT),
	expr_term(EXPRESSION, TEXT).

:- load_library('alice.tuprolog.lib.DCGLibrary').

expr_parse_letters([]) --> [].
expr_parse_letters([HEAD | TAIL]) -->
	{member(HEAD, ['x', 'y', 'z', 'X', 'Y', 'Z'])},
	[HEAD],
	expr_parse_letters(TAIL).

expr_parse_suffix(variable(NAME)) -->
	{nonvar(NAME, atom_chars(NAME, CHARS))},
	expr_parse_letters(CHARS),
	{CHARS = [_ | _], atom_chars(NAME, CHARS)}.

expr_parse_suffix(const(VALUE)) -->
	{nonvar(VALUE, number_chars(VALUE, CHARS))},
	expr_parse_digits(CHARS),
	{((CHARS = [HEAD, MIDDLE | TAIL], HEAD == '-'); (CHARS = [HEAD | TAIL], HEAD \= '-')), number_chars(VALUE, CHARS)}.

expr_parse_digits([]) --> [].
expr_parse_digits([HEAD | TAIL]) -->
	{member(HEAD, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', '-'])},
	[HEAD],
	expr_parse_digits(TAIL).

expr_parse_operation(op_add) --> ['+'].
expr_parse_operation(op_subtract) --> ['-'].
expr_parse_operation(op_multiply) --> ['*'].
expr_parse_operation(op_divide) --> ['/'].
expr_parse_operation(op_negate) --> ['n', 'e', 'g', 'a', 't', 'e'].

expr_parse_suffix(operation(NAME, LEFT, RIGHT)) -->
	['('],
	expr_parse_suffix(LEFT),
	[' '],
	expr_parse_suffix(RIGHT),
	[' '],
	expr_parse_operation(NAME),
	[')'].

expr_parse_suffix(operation(NAME, MIDDLE)) -->
	['('],
	expr_parse_suffix(MIDDLE),
	[' '],
	expr_parse_operation(NAME),
	[')'].

suffix_str(EXPRESSION, STRING) :-
	ground(EXPRESSION),
	phrase(expr_parse_suffix(EXPRESSION), TEXT),
	atom_chars(STRING, TEXT), !.
suffix_str(EXPRESSION, STRING) :-
	atom(STRING),
	atom_chars(STRING, TEXT),
	clear(TEXT, RESULT, 0),
	length(RESULT, R),
	(element(R, RESULT, E), E == ' ' -> copy(RESULT, 1, R, [], PARSED), phrase(expr_parse_suffix(EXPRESSION), PARSED), !; phrase(expr_parse_suffix(EXPRESSION), RESULT), !).

clear([], [], _) :- !.
clear(['(' | TAIL], RESULT, _) :-
	F is 0,
	clear(TAIL, TEMPORARY, F),
	concat(['('], TEMPORARY, RESULT), !.
clear([' ' | TAIL], RESULT, FLAG) :-
	FLAG == 0,
	clear(TAIL, RESULT, FLAG), !;
	FLAG == 1,
	F is 0,
	clear(TAIL, TEMPORARY, F),
	concat([' '], TEMPORARY, RESULT), !.
clear([OP | TAIL], RESULT, _) :-
	member(OP, ['+', '-', '*', '/', 'e']),
	F is 0,
	clear(TAIL, TEMPORARY, F),
	concat([OP], TEMPORARY, RESULT), !.
clear([')' | TAIL], RESULT, _) :-
	F is 1,
	clear(TAIL, TEMPORARY, F),
	concat([')'], TEMPORARY, RESULT), !.
clear([HEAD | TAIL], RESULT, _) :-
	F is 1,
	clear(TAIL, TEMPORARY, F),
	concat([HEAD], TEMPORARY, RESULT), !.

copy(ELEMENTS, INDEX, LENGTH, ARRAY, RESULT) :-
	(INDEX == LENGTH -> RESULT = ARRAY; element(INDEX, ELEMENTS, ELEM), I is INDEX + 1, concat(ARRAY, [ELEM], T), copy(ELEMENTS, I, LENGTH, T, RESULT)).
