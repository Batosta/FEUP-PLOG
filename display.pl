% Initial board
initialBoard([
[[empty, 0], [empty, 0], [empty, 0], [empty, 0]],
[[empty, 0], [black, 20], [white, 20], [empty, 0]],
[[empty, 0], [empty, 0], [empty, 0], [empty, 0]]
]).

% Symbols meaning
symbol(white, S) :- S='O'.
symbol(black, S) :- S='X'.
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


play :-
	initialBoard(X),
<<<<<<< Updated upstream
	addColumnEnd(X, [H1|T1]),
	addRowEnd([H1|T1], 4, R1, [H2|T2]),
	arrayLength(H2, Col),
	addRowStart([H2|T2], Col, R2, [H3|T3]),
	addColumnStart([H3|T3], [H4|T4]),
	addColumnEnd([H4|T4], [H5|T5]),
	addColumnStart([H5|T5], [H6|T6]),
	addColumnEnd([H6|T6], [H7|T7]),
	arrayLength(H7, Col1),
	addRowStart([H7|T7], Col1, R3, P),
	display_game(P, 1, L).
=======
	addRowStart(X, 4, R, Z),
	addRowEnd(Z, 4, R1, Z1),
	display_game(X, 1, L).
>>>>>>> Stashed changes

% Prints any board
display_game([H|T], Player, R) :-

	arrayLength(H, Columns), arrayLength(T, Rows), R1 is Rows + 1,

<<<<<<< Updated upstream
=======
	%addRowStart([H|T], Columns, R),
>>>>>>> Stashed changes
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
printPair([empty|_]) :-
	write('    | ').
printPair([H|T]) :-
	symbol(H, S),
	write(S),
	printNumber(T).

% 0
% Prints the number in a pair
printNumber([], X).
printNumber([H|T]) :-
	(H > 9 -> write(','), write(H), write('| '); write(','), write(H), write(' | ')).


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



%Ex: initialBoard(X), addRowEnd(X, 4, R, Z), display_game(Z, 1, L).
% Adds a full row in the end
addRowEnd(P, Col, R, Z) :-
	createEmptyRow([], Col, R),
	append(P, [R], Z).

% Adds a full row in the start
addRowStart(P, Col, R, Z) :-
	createEmptyRow([], Col, R),
	append([R], P, Z).

% Appends Col x [empty, 0]
createEmptyRow(P, 0, P).
createEmptyRow(P, Col, R):-
	Col1 is Col - 1,
	append(P, [[empty, 0]], Z),
<<<<<<< Updated upstream
	createEmptyRow(Z, Col1, R).

% Adds a full column to the end.

addColumnEnd([], []).
addColumnEnd([H|T], [H1|T1]):-
	append(H, [[empty,0]], H1),
	addColumnEnd(T, T1).


% Adds a full column to the beginning.

addColumnStart([], []).
addColumnStart([H|T], [H1|T1]):-
	append([[empty,0]], H, H1),
	addColumnStart(T, T1).
=======
	createEmptyRow(Z, Col1, R).
>>>>>>> Stashed changes
