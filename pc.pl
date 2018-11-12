%chooseLPlay([H|T], Player, Col, Row)

chooseRandomPiece(X, ColSize, RowSize, Col, Row) :-
	random(0, ColSize, C1),
	random(0, RowSize, R1),
	checkPiece(X, R1, C1, P1),
	(P1 =:= 0 ->
		Col is C1,
		Row is R1;
		chooseRandomPiece(X, ColSize, RowSize, Col, Row)
	).



createRandomLMove(X, Player, Col, Row) :-
	A is Col - 2,
	B is Col - 1,
	C is Col + 1,
	D is Col + 2,
	E is Row - 2,
	F is Row - 1,
	G is Row + 1,
	I is Row + 2,
	random(1, 9, Rand).		



% createRandomLMove
% dou random a uma L
% vejo se esse L tem adjacents
	% createRandomAdjacent
	% createRandomLMove