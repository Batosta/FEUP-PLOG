% Initial board
initialBoard([
[[empty, 0], [empty, 0], [empty, 0], [empty, 0]],
[[empty, 0], [black, 20], [white, 20], [empty, 0]],
[[empty, 0], [empty, 0], [empty, 0], [empty, 0]]
]).

% Mid game Board
midBoard([
[[empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0]],
[[empty, 0], [white, 4], [black, 1], [white, 4], [empty, 0], [empty, 0], [empty, 0]],
[[empty, 0], [white, 2], [black, 6], [black, 3], [white, 4], [empty, 0], [empty, 0]],
[[empty, 0], [black, 2], [black, 5], [white, 5], [black, 3], [white, 1], [empty, 0]],
[[empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0]]
]).

% Final game board
finalBoard([
[[empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0]],
[[empty, 0], [black, 1], [white, 3], [empty, 0], [empty, 0], [empty, 0], [empty, 0]],
[[empty, 0], [white, 2], [black, 4], [white, 6], [white, 2], [black, 5], [empty, 0]],
[[empty, 0], [empty, 0], [white, 1], [black, 2], [black, 3], [white, 1], [empty, 0]],
[[empty, 0], [empty, 0], [empty, 0], [white, 5], [black, 2], [black, 3], [empty, 0]],
[[empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0]]
]).

% Symbols meaning
symbol(empty, S) :- S='e'.
symbol(white, S) :- S='W'.
symbol(black, S) :- S='B'.


% Board printer
printBoard(X) :-
	printGame(X, 1).


% Head = [[empty, 0], [empty, 0], [empty, 0], [empty, 0]]
printGame([], 20).
printGame([H|T], N) :-
	N1 is N + 1,
	nl,
	write('| '),
	printLine(H), nl,
	arrayLength(H, K),
	separation(K),
	printGame(T, N1).

% Head = [empty, 0]
printLine([]).
printLine([H|T]) :-
	printPair(H),
	printLine(T).


% Head = empty
printPair([]).
printPair([H|T]) :-
	symbol(H, S),
	write(S),
	printNumber(T).

printNumber([]).
printNumber([H|T]) :-
	write(','),
	write(H),
	write(' | ').


% Calculate an array size
arrayLength([], 0).
arrayLength([H|T], LenResult) :-
	arrayLength(T, L),
	LenResult is L + 1.

% Table top
tableTop(0).
tableTop(N) :-
	write(' |  '),
	write(N),
	N1 is N - 1,
	tableTop(N1).

separation(0).
separation(N) :-
	write('-----'),
	N1 is N - 1,
	write(N1),
	separation(N1).