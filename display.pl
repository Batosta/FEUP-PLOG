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
symbol(white, S) :- S='w'.
symbol(black, S) :- S='b'.
letter(1, S) :- S='A'.
letter(2, S) :- S='B'.
letter(3, S) :- S='C'.
letter(4, S) :- S='D'.
letter(5, S) :- S='E'.
letter(6, S) :- S='F'.
letter(7, S) :- S='G'.
letter(8, S) :- S='H'.
letter(9, S) :- S='I'.
letter(10, S) :- S='J'.
letter(11, S) :- S='K'.
letter(12, S) :- S='L'.
letter(13, S) :- S='M'.
letter(14, S) :- S='N'.
letter(15, S) :- S='O'.
letter(16, S) :- S='P'.
letter(17, S) :- S='Q'.
letter(18, S) :- S='R'.
letter(19, S) :- S='S'.
letter(20, S) :- S='T'.
letter(21, S) :- S='U'.
letter(22, S) :- S='V'.
letter(23, S) :- S='W'.
letter(24, S) :- S='X'.
letter(25, S) :- S='Y'.
letter(26, S) :- S='Z'.


% Prints any board
display_game([H|T], Player) :-
	arrayLength(H, Columns),
	nl, write('|'), separation(Columns), write('|'), nl,
	write('|/////|   '), tableTop(Columns, 0), nl,
	write('|'), separation(Columns), write('|'),
	printBoard([H|T], Columns, 0, 1), nl, nl.


% Head - [[empty, 0], [empty, 0], [empty, 0], [empty, 0]]
% Prints the pieces & its number
% ([Head|Tail], Columns, Rows, N)
printBoard([], _, _, _).
printBoard([H|T], C, R, N) :-
	nl,
	N1 is N + 1,
	R1 is R + 1,
	letter(R1, S),
	write('|  '), write(S), write('  |  '), printLine(H), nl, 
	write('|'), separation(C), write('|'),
	printBoard(T, C, R1, N1).

% [empty, 0]
% Prints a full line
printLine([]).
printLine([H|T]) :-
	printPair(H),
	printLine(T).

% empty
% Prints a pair
printPair([]).
printPair([H|T]) :-
	symbol(H, S),
	write(S),
	printNumber(T).

% 0
% Prints the number in a pair
printNumber([]).
printNumber([H|T]) :-
	write(','), write(H), write(' | ').

% Top of the table
tableTop(C, C).
tableTop(C, X) :-
	write(X), write('  |  '),
	X1 is X + 1,
	tableTop(C, X1).


% Separation between 2 lines
separation(-1).
separation(C) :-
	write('------'),
	C1 is C - 1,
	separation(C1).

% Calculate an array size
arrayLength([], 0).
arrayLength([H|T], LenResult) :-
	arrayLength(T, L),
	LenResult is L + 1.