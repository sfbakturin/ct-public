
%
% @author Saveliy Bakturin
% <p>
% Don't write off, if you don't wanna be banned!
%

checkIfDivide(NUMBER, DIVISOR) :- (0 is mod(NUMBER, DIVISOR)).
checkIfNotDivide(NUMBER, DIVISOR) :- (\+ checkIfDivide(NUMBER, DIVISOR)).

prime(NUMBER, 1) :- NUMBER \= 1.
prime(NUMBER, DIVISOR) :- (checkIfNotDivide(NUMBER, DIVISOR)), (DIVISORNEW is (DIVISOR - 1)), prime(NUMBER, DIVISORNEW).
prime(N) :- DIV is (round(sqrt(N))), prime(N, DIV).

composite(N) :- \+ prime(N).

concat([], B, B).
concat([H | T], B, [H | R]) :- concat(T, B, R).

factorization(NUMBER, DIVISOR, ARRAY, RESULT) :- ((checkIfDivide(NUMBER, DIVISOR)) -> ((N1 is (NUMBER // DIVISOR)), concat(ARRAY, [DIVISOR], T), factorization(N1, DIVISOR, T, RESULT)); ((NUMBER > 1) -> ((D1 is (DIVISOR + 1)), factorization(NUMBER, D1, ARRAY, RESULT)); RESULT = ARRAY)).
factorization(NUMBER, RETURN) :- factorization(NUMBER, 2, [], T), RETURN = T.

prime_divisors(N, Divisors) :- factorization(N, R), ((\+ is_list(Divisors)) -> (Divisors = R); true), R == Divisors.

nth_prime(N, PRIME, CURRENT, NUM) :- (NUM == N -> PRIME = CURRENT; C1 is CURRENT + 1, (prime(C1) -> N1 is NUM + 1, nth_prime(N, PRIME, C1, N1); nth_prime(N, PRIME, C1, NUM))).
nth_prime(N, P) :- nth_prime(N, P1, 2, 1), P = P1.
