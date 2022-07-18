
%
% @author Saveliy Bakturin
%
% Don't write off, if you don't wanna be banned!
%

%нода:	0 - КЛЮЧ,
%		1 - ЗНАЧЕНИЕ,
%		2 - ЛЕВОЕ ПОДДЕРЕВО,
%		3 - ПРАВОЕ ПОДДЕРЕВО.

node(KEY, VALUE, [KEY, VALUE, [], []]).
node(KEY, VALUE, LEFT, RIGHT, [KEY, VALUE, LEFT, RIGHT]).

map_get_key([A, B, C, D], A).
map_get_value([A, B, C, D], B).
map_get_left_child([A, B, C, D], C).
map_get_right_child([A, B, C, D], D).

pair_get_head((H, T), RESULT) :- RESULT = H.
pair_get_tail((H, T), RESULT) :- RESULT = T.

map_build(ELEMENTS, TREE, ARRAY, LEFT, RIGHT) :-
	(\+ (LEFT > RIGHT) ->
		(M is (LEFT + RIGHT) // 2,
		ML is M - 1,
		MR is M + 1,
		element(MR, ELEMENTS, PAIR),
		pair_get_head(PAIR, KEY),
		pair_get_tail(PAIR, VALUE),
		map_build(ELEMENTS, TREE_LEFT, [], LEFT, ML),
		map_build(ELEMENTS, TREE_RIGHT, [], MR, RIGHT),
		node(KEY, VALUE, TREE_LEFT, TREE_RIGHT, TREE));
	TREE = ARRAY).

map_build(ListMap, TreeMap) :-
	length(ListMap, LENGTH),
	RIGHT_EDGE is LENGTH - 1,
	map_build(ListMap, TreeMap, [], 0, RIGHT_EDGE), !.

map_get_variable(TREE, KEY, RESULT) :-
	(TREE \= [], map_get_key(TREE, TK), TK > KEY ->
		map_get_left_child(TREE, LEFT),
		map_get_variable(LEFT, KEY, RESULT);
	 TREE \= [], map_get_key(TREE, TK), TK < KEY ->
	 	map_get_right_child(TREE, RIGHT),
	 	map_get_variable(RIGHT, KEY, RESULT);
	 TREE \= [], map_get_key(TREE, TK), TK == KEY ->
	 	map_get_value(TREE, RESULT)).

map_get(TreeMap, Key, Value) :-
	var(Value),
	map_get_variable(TreeMap, Key, Value), !.

map_replacer(TREE, KEY, VALUE, NEW_TREE) :-
	(map_get_key(TREE, TK), TK > KEY ->
		map_get_key(TREE, COPY_KEY),
		map_get_value(TREE, COPY_VALUE),
		map_get_left_child(TREE, COPY_LEFT),
		map_get_right_child(TREE, COPY_RIGHT),
		map_replacer(COPY_LEFT, KEY, VALUE, EDITED_LEFT),
		node(COPY_KEY, COPY_VALUE, EDITED_LEFT, COPY_RIGHT, NEW_TREE), !;
	 map_get_key(TREE, TK), TK < KEY ->
	 	map_get_key(TREE, COPY_KEY),
		map_get_value(TREE, COPY_VALUE),
		map_get_left_child(TREE, COPY_LEFT),
		map_get_right_child(TREE, COPY_RIGHT),
		map_replacer(COPY_RIGHT, KEY, VALUE, EDITED_RIGHT),
		node(COPY_KEY, COPY_VALUE, COPY_LEFT, EDITED_RIGHT, NEW_TREE), !;
	 map_get_key(TREE, TK), TK == KEY ->
	 	map_get_key(TREE, COPY_KEY),
		map_get_left_child(TREE, COPY_LEFT),
		map_get_right_child(TREE, COPY_RIGHT),
		node(COPY_KEY, VALUE, COPY_LEFT, COPY_RIGHT, NEW_TREE), !).
%map_check(TREE, KEY, RESULT) :-
%	(TREE \= [], map_get_key(TREE, TK), TK > KEY ->
%		map_get_left_child(TREE, LEFT),
%		map_check(LEFT, KEY, RESULT);
%	 TREE \= [], map_get_key(TREE, TK), TK < KEY ->
%	 	map_get_right_child(TREE, RIGHT),
%	 	map_check(RIGHT, KEY, RESULT);
%	 TREE \= [], map_get_key(TREE, TK), TK == KEY ->
%	 	RESULT = true).
map_replace(Map, Key, Value, Result) :-
	map_get(Map, Key, Flag),
	map_replacer(Map, Key, Value, Result), !;
	Result = Map.
