
% Calculate an array size
arrayLength([], 0).
arrayLength([_|T], LenResult) :-
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
addRowEnd([H|T], Z) :-
	arrayLength(H, Col),
	createEmptyRow([], Col, R),
	append([H|T], [R], Z).

% Adds a full row in the start
addRowStart([H|T], Z) :-
	arrayLength(H, Col),
	createEmptyRow([], Col, R),
	append([R], [H|T], Z).

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

checkPiece1([H|_], 0, Col, Piece):-
	checkPieceAux(H, Col, Piece).
checkPiece1([_|T], Row, Col, Piece):-
	Row1 is Row-1,
	checkPiece(T, Row1, Col, Piece).


checkPieceAux([[H|[_|[]]]|_], 0, Piece):-
	(
		H = white -> Piece is 0
	;	( H = black -> Piece is 1
		;	Piece is 2
		)
	).
checkPieceAux([_|T], Col, Piece):-
	Col1 is Col-1,
	checkPieceAux(T, Col1, Piece).


% Checks the number of pieces in a certain tile
checkNumber([H|_], 0, Col, Num):-
	checkNumberAux(H, Col, Num).
checkNumber([_|T], Row, Col, Num):-
	Row1 is Row-1,
	checkNumber(T, Row1, Col, Num).

checkNumberAux([[_|[H1|[]]]|_], 0, Num):-
	Num is H1.
checkNumberAux([_|T], Col, Num):-
	Col1 is Col-1,
	checkNumberAux(T, Col1, Num).


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

winningMessage:-
    write('__________________________________________'),nl,nl,
    write('| Congratulations you Just won the game! |'),nl,
    write('__________________________________________'),nl.

choseStack(Board, C, R, Player):-
	write('To play'), nl,
	write('Column : '), read(C),
	write('Row    : '), read(R).

choseWhereToMove(Board, C, R, Player):-
	write('Where to play'), nl,
	write('Column : '), read(C),
	write('Row    : '), read(R).

turn(Player):-
	(Player = 1 ->
    	write('__________________________________________'),nl,nl,
   		write('|        X Player turn to play!           |'),nl,
    	write('__________________________________________'),nl
	;   write('__________________________________________'),nl,nl,
    	write('|        O Player turn to play!            |'),nl,
    	write('__________________________________________'),nl
	).

choseNumberPieces(Np):-
	write('Number of pieces : '), read(Np).

checkLengths([H|T], MaxRow, MaxCol):-
	arrayLength([H|T], MaxRow), 
	arrayLength(H, MaxCol).