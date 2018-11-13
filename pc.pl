playPLPC :-
	initialBoard(X),
	%display_game(X),
	mainRecursivePLPC(X, 1, 0, 0).

chooseRandomPieceNumber(X, Player, ColSize, RowSize, Col, Row, Number) :-
	random(0, ColSize, C1),
	random(0, RowSize, R1),
	checkPiece(X, R1, C1, F1),
	(F1 =\= Player ->
		chooseRandomPieceNumber(X, Player, ColSize, RowSize, Col, Row, Number);
		checkNumber(X, R1, C1, MaxNum),
		(MaxNum =:= 1 ->
			chooseRandomPieceNumber(X, Player, ColSize, RowSize, Col, Row, Number);
			random(1, MaxNum, Number),
			Col is C1,
			Row is R1
		)
	).

chooseRandomLMove(X, Player, ColSize, RowSize, PieceC, PieceR, TileC, TileR) :-
	random(0, ColSize, C2),
	random(0, RowSize, R2),
	checkPiece(X, R2, C2, F1),
	(F1 =\= 2 ->
		chooseRandomLMove(X, Player, ColSize, RowSize, PieceC, PieceR, TileC, TileR);
		checkLPlay(X, PieceC, PieceR, C2, R2, F2),
		(F2 =\= 1 ->
			chooseRandomLMove(X, Player, ColSize, RowSize, PieceC, PieceR, TileC, TileR);
			checkAdjacents(X, R2, C2, F3),
			(F3 =\= 1 ->
				chooseRandomLMove(X, Player, ColSize, RowSize, PieceC, PieceR, TileC, TileR);
				TileC is C2,
				TileR is R2
			)
		)
	).

pcMove(X, Player, C1, R1, C2, R2, Np) :-
	checkLengths(X, RowSize, ColSize),
	chooseRandomPieceNumber(X, Player, ColSize, RowSize, C1, R1, Np),
	chooseRandomLMove(X, Player, ColSize, RowSize, C1, R1, C2, R2).



% (1: black; 0: white)
mainRecursivePLPC(_, _, 1, _):-
	winningMessage.
mainRecursivePLPC(_, _, _, 1):-
	noPiecesMessage.
mainRecursivePLPC(Board, Counter, _, _) :-
	write('\33\[2J'),
	display_game(Board),
	Player is Counter mod 2,

	(Player \= 0 -> 
		turn(1), nl,
		choseStack(Board, C1, R1, Player),
		choseWhereToMove(Board, C1, R1, C2, R2, Player),
		choseNumberPieces(Board, C1, R1, Np, Counter);

		turn(0), nl,
		pcMove(Board, Player, C1, R1, C2, R2, Np)
	),

	(Player \= 0 -> 
		makeMoveB(Board, C1, R1, C2, R2, Np, Board1); 
		makeMoveW(Board, C1, R1, C2, R2, Np, Board1)
	), 

	checkLengths(Board1, MaxRow, MaxCol),
	checkWin(Board1, Player, MaxRow, MaxCol, 0, 0, WinAux),
	checkIfPossible(Board1, Player, MaxRow, MaxCol, 0, 0, LoseAux),

	display_game(Board1),

	Counter1 is Counter + 1,
	mainRecursivePLPC(Board1, Counter1, WinAux, LoseAux).

%procura no mapa uma posição com uma peça empty e vê se tem Max peças na diagonal, vertical, horizontal, caso tenha, ve se existe em L uma peça white que possa jogar nesta posiçao 

checkGreedy(_,_,_,_,_,0,_,_):-
	%meter aqui a random do pretjinho

checkGreedy(_,MaxRow, MaxCol, MaxRow, MaxCol, Max, JogR, JogC):-
	Max1 is Max - 1,
	checkGreedy(X, MaxRow, MaxCol, 0, 0, Max1, JogR, JogC).

checkGreedy(X, MaxRow, MaxCol, R, C, Max, JogR, JogC):-
	checkPiece(X, R, C, Flag),
	C1 is C+1,
	(Flag = 2 -> 
		checkMax(X, R, C, Max, JogR, JogC); 
		(C =:= MaxCol -> R1 is R+1, checkGreedy(X, MaxRow, MaxCol, R1, 0, Max, JogR, JogC) ; checkGreedy(X, MaxRow, MaxCol, R, C1, Max, JogR, JogC))
	).

checkMax(X, R, C, Mm JogR, JogC):-