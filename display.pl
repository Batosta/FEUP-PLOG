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
	addColumnEnd(X, [H1|T1]),
	addRowEnd([H1|T1], R1, [H2|T2]),
	addRowStart([H2|T2], R2, [H3|T3]),
	addColumnStart([H3|T3], [H4|T4]),
	addColumnEnd([H4|T4], [H5|T5]),
	addColumnStart([H5|T5], [H6|T6]),
	addColumnEnd([H6|T6], [H7|T7]),
	addRowStart([H7|T7], R3, P),
	updatePiece(P, [], 3, 3, 5, black, F),
	movePiece(F, [], 2, 5, 5, black, F1),
	updatePiece(F1, [], 3, 4, 3, white, F2),
	movePiece(F2, [], 1, 5, 3, white, F3),
	checkPiece(F3, 0, 0, Empty),
	( Empty = 2 ->
		updatePiece(F3, [], 3, 3, 2, black, F4),
		movePiece(F4, [], 0, 0, 2, black, F5),
		boardResize(F5, 0, 0, F6),
		display_game(F6, 1, L)
	;	write('Cant move to that position, not empty!'),
		display_game(F3, 1, L)	
	).

% Prints any board
display_game([H|T], Player, R) :-

	arrayLength(H, Columns), arrayLength([H|T], Rows),

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
addRowEnd([H|T], R, Z) :-
	arrayLength(H, Col),
	createEmptyRow([], Col, R),
	append([H|T], [R], Z).

% Adds a full row in the start
addRowStart([H|T], R, Z) :-
	arrayLength(H, Col),
	createEmptyRow([], Col, R),
	append([R], [H|T], Z).

% Appends Col x [empty, 0]
createEmptyRow(P, 0, P).
createEmptyRow(P, Col, R):-
	Col1 is Col - 1,
	append(P, [[empty, 0]], Z),
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

% Checks the board needs to be resized
boardResize([H|T], IndC, IndR, [H4|T4]) :-
	arrayLength(H, Columns), 
	arrayLength([H|T], Rows),
	Col is Columns - 1,
	Row is Rows - 1,
	((IndC =:= 0 ; IndR =:= 0 ; IndC =:= Col ; IndR =:= Row) -> 
		addColumnEnd([H|T], [H1|T1]),
		addRowEnd([H1|T1], R1, [H2|T2]),
		addRowStart([H2|T2], R2, [H3|T3]),
		addColumnStart([H3|T3], [H4|T4]); 
		append([], [H|T], [H4|T4])).


% Updates the pieces that were played too
updatePiece([H|T], New, 0, IndCol, Number, Player, Final):-
	updatePieceAux(H, IndCol, Number, Z),
	replace(H, IndCol, [Player,Z], R),
	append([R], T, Q),
	append(New, Q, Final).
updatePiece([H|T], New, IndRow, IndCol, Number, Player, Final):-
	IndRow1 is IndRow - 1,
	append(New, [H], NewT),
	updatePiece(T, NewT, IndRow1, IndCol, Number, Player, Final).

updatePieceAux([[H1|T1]|T], 0, Number, Z):-
	Z is T1 - Number.
updatePieceAux([H|T], IndCol, Number, Z):-
	IndCol1 is IndCol - 1,
	updatePieceAux(T, IndCol1, Number, Z).


% Updates the tile to where the piece was played
movePiece([H|T], New, 0, IndCol, Number, Player, Final):-
	replace(H, IndCol, [Player,Number], R),
	append([R], T, Q),
	append(New, Q, Final).
movePiece([H|T], New, IndRow, IndCol, Number, Player, Final):-
	IndRow1 is IndRow-1,
	append(New, [H], NewT),
	movePiece(T, NewT, IndRow1, IndCol, Number, Player, Final).

% Replaces an element in a list into another, returning the resulting board
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

% Checks the pieces color/empty
checkPiece([H|T], 0, Col, Piece):-
	checkPieceAux(H, Col, Piece).
checkPiece([H|T], Row, Col, Piece):-
	Row1 is Row-1,
	checkPiece(T, Row1, Col, Piece).

checkPieceAux([[H|[H1|[]]]|T], 0, Piece):-
	(
		H = white -> Piece is 0
	;	( H = black -> Piece is 1
		;	Piece is 2
		)
	).
checkPieceAux([H|T], Col, Piece):-
	Col1 is Col-1,
	checkPieceAux(T, Col1, Piece).

testBoard([
[[black, 0], [black, 0], [empty, 0]],
[[empty, 0], [black, 20], [empty, 0]],
[[empty, 0], [empty, 0], [empty, 0]]
]).

% Checks the adjacent tiles to a tile (returning 0 means there arent adjacent pieces)
checkAdjacents([H|T], IndR, IndC, Flag) :-
A is IndR + 1,
B is IndR,
C is IndR - 1,
D is IndC + 1,
E is IndC,
F is IndC - 1,
checkPiece([H|T], C, F, P1),
(P1 =\= 2 -> Flag is 1;
	checkPiece([H|T], C, E, P2),
	(P2 =\= 2 -> Flag is 1;
		checkPiece([H|T], C, D, P3),	
		(P3 =\= 2 -> Flag is 1;
			checkPiece([H|T], B, D, P4),
			(P4 =\= 2 -> Flag is 1;
				checkPiece([H|T], A, D, P5),
				(P5 =\= 2 -> Flag is 1;
					checkPiece([H|T], A, E, P6),
					(P6 =\= 2 -> Flag is 1;
						checkPiece([H|T], A, F, P7),
						(P7 =\= 2 -> Flag is 1;
							checkPiece([H|T], B, F, P8),
							(P8 =\= 2 -> Flag is 1;
							Flag is 0)
						)
					)
				)
			)
		)
	)
).
