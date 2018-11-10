% Initial board
initialBoard([
[[empty, 0], [empty, 0], [empty, 0], [empty, 0]],
[[empty, 0], [black, 20], [white, 20], [empty, 0]],
[[empty, 0], [empty, 0], [empty, 0], [empty, 0]]
]).

testBoard([
[[empty, 0], [empty, 0], [empty, 0], [empty, 0], [empty, 0]],
[[white, 0], [white, 0], [white, 0], [white, 0], [black, 0]],
[[empty, 0], [empty, 0], [white, 0], [empty, 0], [black, 0]],
[[empty, 0], [empty, 0], [empty, 0], [white, 0], [black, 0]],
[[empty, 0], [empty, 0], [empty, 0], [empty, 0], [white, 0]]
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

% To play the game. The recursive function that does, basically, everything
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
	updatePiece(P, [], 3, 3, 19, black, F),
	movePiece(F, [], 2, 5, 7, black, F1),
	display_game(F1).
	%updatePiece(F1, [], 3, 4, 3, white, F2),
	%movePiece(F2, [], 1, 5, 3, white, F3),
	%checkPiece(F3, 0, 0, Empty),
	%(Empty = 2 ->
	%	updatePiece(F3, [], 3, 3, 2, black, F4),
	%	movePiece(F4, [], 0, 0, 2, black, F5),
	%	boardResize(F5, 0, 0, F6),
	%	display_game(F6)
	%;	write('Cant move to that position, not empty!'),
	%	display_game(F3)	
	%).


play2 :-
	initialBoard(X),
	display_game(X),
	mainRecursive(X, Y, 0).


% Prints any board
display_game([H|T]) :-

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



% Checks the pieces color/empty (2: empty; 1: black; 0: white)
checkPiece([H|T], Row, Col, Piece) :-
	checkInsideBoard([H|T], Col, Row, Flag),
	(Flag =:= 0 -> checkPiece1([H|T], Row, Col, Piece);
		Piece is 2
	).

checkPiece1([H|T], 0, Col, Piece):-
	checkPieceAux(H, Col, Piece).
checkPiece1([H|T], Row, Col, Piece):-
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


% Checks the number of pieces in a certain tile
checkNumber([H|T], 0, Col, Num):-
	checkNumberAux(H, Col, Num).
checkNumber([H|T], Row, Col, Num):-
	Row1 is Row-1,
	checkNumber(T, Row1, Col, Num).

checkNumberAux([[H|[H1|[]]]|T], 0, Num):-
	Num is H1.
checkNumberAux([H|T], Col, Num):-
	Col1 is Col-1,
	checkNumberAux(T, Col1, Num).



% Checks the adjacent tiles to a tile (1: there are adjacent pieces; 0: there are not)
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



% Checks the play to check its L (1: it is an L move; 0: it is not)
checkLPlay([H|T], C1, R1, C2, R2, Flag) :-
	A is C1 - 2,
	B is C1 - 1,
	C is C1 + 1,
	D is C1 + 2,	
	E is R1 - 2,
	F is R1 - 1,
	G is R1 + 1,
	I is R1 + 2,
	checkInsideBoard([H|T], B, E, F1),
	((C2 =:= B , R2 =:= E , F1 =:= 0) -> Flag is 1;
		checkInsideBoard([H|T], C, E, F2),
		((C2 =:= C , R2 =:= E , F2 =:= 0) -> Flag is 1;
			checkInsideBoard([H|T], D, F, F3),
			((C2 =:= D , R2 =:= F , F3 =:= 0) -> Flag is 1;
				checkInsideBoard([H|T], D, G, F4),
				((C2 =:= D , R2 =:= G , F4 =:= 0) -> Flag is 1;
					checkInsideBoard([H|T], C, I, F5),
					((C2 =:= C , R2 =:= I , F5 =:= 0) -> Flag is 1;
						checkInsideBoard([H|T], B, I, F6),
						((C2 =:= B , R2 =:= I , F6 =:= 0) -> Flag is 1;
							checkInsideBoard([H|T], A, G, F7),
							((C2 =:= A , R2 =:= G , F7 =:= 0) -> Flag is 1;
								checkInsideBoard([H|T], A, F, F8),
								((C2 =:= A , R2 =:= F , F8 =:= 0) -> Flag is 1;
								Flag is 0)
							)
						)
					)
				)
			)
		)
	).


% Checks the cases where a piece is outside the board (1: is not inside the board; 0: is inside)
checkInsideBoard([H|T], IndC, IndR, Flag) :-
	arrayLength(H, Col), 
	arrayLength([H|T], Row),
	C is Col - 1,
	R is Row - 1,
	((IndC @> C ; IndR @> R) -> Flag is 1;
		((IndC @< 0 ; IndR @< 0) -> Flag is 1;
		Flag is 0)
	).

% Checks all the conditions to see whether a piece is valid to be played (1: with errors; 0: can proceed)
checkStackConditions([H|T], Col, Row, Flag) :-
	checkInsideBoard([H|T], Col, Row, F1),
	(F1 =:= 1 -> write('The tile must belong to the board'), nl, Flag is 1;
		checkPiece([H|T], Row, Col, F2),
		(F2 =:= 2 -> write('The tile must not be empty'), nl, Flag is 1;
			Flag is 0
		)
	).

% Checks all the conditions to see whether a tile is valid to be played to (1: with errors; 0: can proceed)
checkTileConditions([H|T], C1, R1, C2, R2, Flag) :-
	checkInsideBoard([H|T], C2, R2, F1),
	(F1 =:= 1 -> write('The tile must belong to the board'), nl, Flag is 1;
		checkPiece([H|T], R2, C2, F2),
		(F2 =\= 2 -> write('The tile must be empty'), nl, Flag is 1;
			checkLPlay([H|T], C1, R1, C2, R2, F3),
			(F3 =:= 0 -> write('The play must be in an L shape'), nl, Flag is 1;
				checkAdjacents([H|T], R2, C2, F4),
				(F4 =:= 0 -> write('The tile must have adjacent pieces'), nl, Flag is 1;
					Flag is 0
				)
			)
		)
	).

% Checks whether the number of pieces to be moved is valid
checkNPConditions([H|T], Col, Row, Number, Flag) :-
	checkNumber([H|T], Col, Row, N),
	MaxNum is N - 1,
	((Number @> MaxNum ; Number @< 1) -> write('The number of pieces to be moved from this tile must be between 1 and '), write(MaxNum), nl,
		Flag is 1;
		Flag is 0
	).

makeMoveB([H|T], C1, R1, C2, R2, NP, Z):-
	updatePiece([H|T], [], R1, C1, NP, black, Z1),
	movePiece(Z1, [], R2, C2, NP, black, Z2),
	boardResize(Z2, C2, R2, Z).

makeMoveW([H|T], C1, R1, C2, R2, NP, Z):-
	updatePiece([H|T], [], R1, C1, NP, white, Z1),
	movePiece(Z1, [], R2, C2, NP, white, Z2),
	boardResize(Z2, C2, R2, Z).


% Recursive function that takes care of all the plays
mainRecursive([H|T], [H1|T1], Counter) :-
	Result is Counter mod 2,
	(Result = 0 -> write('X turn to play!'), nl
	;write('O turn to play!'), nl
	),
	write('Insert the coordinates of the stack you wish to move:'), nl,
	write('Number of the column: '), read(C1),
	write('Number of the row: '), read(R1),
	checkStackConditions([H|T], C1, R1, F1),

	checkPiece([H|T], R1, C1, Piece),
	(((Result = 0, Piece \= 1) ; (Result = 1, Piece \= 0)) -> write('You cant play that stack!'), nl, mainRecursive([H|T], [H1|T1], Counter)
	; write('You chose your stack correctly!'), nl
	),

	nl,
	write('Insert the coordinates of the tile you wish to move your stack to:'), nl,
	write('Number of the column: '), read(C2),
	write('Number of the row: '), read(R2),
	checkTileConditions([H|T], C1, R1, C2, R2, F2),

	nl, 
	write('Choose the number of pieces you wish to move:'), read(NP),
	checkNPConditions([H|T], C1, R1, NP, F3),

	(Result = 0 -> makeMoveB([H|T], C1, R1, C2, R2, NP, [H1|T1])
	; makeMoveW([H|T], C1, R1, C2, R2, NP, [H1|T1])
	), 
	display_game([H1|T1]),
	Counter1 is Counter+1,
	mainRecursive([H1|T1], New, Counter1).


%WIN CONDITION

%tabuleiro, 20, 20, 0, 0, Win
checkWin([H|T], MaxRow, MaxCol, Row, Col, Win):-
	checkPiece([H|T], Row, Col, F1),
	(F1 = 2 -> Col1 is Col+1, checkWin([H|T], MaxRow, MaxCol, Row, Col1, Win)
	;	(F1 = 1 -> check([H|T], Row, Col, black, Winaux)
		; check([H|T])	
		)
	)

	
%Returns 1 for black win, 2 for white win
check(X, Row, Col, Player, Win):-
	checkHorizontal(X, Row, Col, Player, Win1),
	checkVertical(X, Row, Col, Player, Win2),
	checkDiagonal(X, Row, Col, Player, Win3),
	((Win1 = 1; Win2 = 1 ; Win3 = 1) -> Win is 1
	; ((Win1 = 2; Win2 = 2; Win3 = 2) -> Win is 2 ; Win is 0)
	). 

%Each one returns 1 for black, returns 2 for white

checkHorizontal(X, Row, Col, Player, Win):-
	A is Col+1,
	B is Col+2,
	C is Col+3,
	checkInsideBoard(X, A, Row, F1),
	checkInsideBoard(X, B, Row, F2),
	checkInsideBoard(X, C, Row, F3),

	checkPiece(X, Row, A, P1),
	checkPiece(X, Row, B, P2),
	checkPiece(X, Row, C, P3),
	(Player = black -> 
		(((F1 = 0, P1 = 1), (F2 = 0, P2 = 1), (F3 = 0, P3 = 1)) -> 
			Win is 1
		;	Win is 0
		)
	;	(((F1 = 0, P1 = 0), (F2 = 0, P2 = 0), (F3 = 0, P3 = 0)) -> 
			Win is 2
		;	Win is 0
		)
	).

checkVertical(X, Row, Col, Player, Win):-
	A is Row+1,
	B is Row+2,
	C is Row+3,
	checkInsideBoard(X, Col, A, F1),
	checkInsideBoard(X, Col, B, F2),
	checkInsideBoard(X, Col, C, F3),

	checkPiece(X, A, Col, P1),
	checkPiece(X, B, Col, P2),
	checkPiece(X, C, Col, P3),
	(Player = black -> 
		(((F1 = 0, P1 = 1), (F2 = 0, P2 = 1), (F3 = 0, P3 = 1)) -> 
			Win is 1
		;	Win is 0
		)
	;	(((F1 = 0, P1 = 0), (F2 = 0, P2 = 0), (F3 = 0, P3 = 0)) -> 
			Win is 2
		;	Win is 0
		)
	).

checkDiagonal(X, Row, Col, Player, Win):-
	R1 is Row+1,
	C1 is Col+1,
	
	R2 is R1+1,
	C2 is C1+1,

	R3 is R2+1,
	C3 is C2+1,

	checkInsideBoard(X, R1, C1, F1),
	checkInsideBoard(X, R2, C2, F2),
	checkInsideBoard(X, R3, C3, F3),

	checkPiece(X, R1, C1, P1),
	checkPiece(X, R2, C2, P2),
	checkPiece(X, R3, C3, P3),
	(Player = black -> 
		(((F1 = 0, P1 = 1), (F2 = 0, P2 = 1), (F3 = 0, P3 = 1)) -> 
			Win is 1
		;	Win is 0
		)
	;	(((F1 = 0, P1 = 0), (F2 = 0, P2 = 0), (F3 = 0, P3 = 0)) -> 
			Win is 2
		;	Win is 0
		)
	).