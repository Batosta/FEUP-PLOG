% Returns in Dist the distance between the two points with coordinates (R1, C1)-(R2, C2)
distancePoints(R1, C1, R2, C2, Dist) :-
	
	DiffRow is abs(R1 - R2),
	DiffCol is abs(C1 - C2),
	DiffRow2 is DiffRow * DiffRow,
	DiffCol2 is DiffCol * DiffCol,
	Dist is DiffRow2 + DiffCol2.


% Returns the number of houses in the puzzle
getNumberHouses([], NumberHouses, NumberHouses).
getNumberHouses([H|T], NumberHouses, Aux) :-

	getNumberHousesAux(H, NewAux, Aux),
	getNumberHouses(T, NumberHouses, NewAux), !.

getNumberHousesAux([], LineNumber, LineNumber).
getNumberHousesAux([H|T], LineNumber, Aux) :-
	
	H = 1,
		Aux1 is Aux + 1,
		getNumberHousesAux(T, LineNumber, Aux1);
		getNumberHousesAux(T, LineNumber, Aux).


% Goes through the whole board saving the coordinates of the houses
getHousesCoords([], _, XHouseCoords, XHouseCoords, YHouseCoords, YHouseCoords).
getHousesCoords([H|T], Row, XHouseCoords, XAux, YHouseCoords, YAux) :-
	
	getHousesCoordsAux(H, Row, 0, XHouseCoordsAux, [], YHouseCoordsAux, []),
	append(XAux, XHouseCoordsAux, XAux1),
	append(YAux, YHouseCoordsAux, YAux1),
	Row1 is Row + 1,
	getHousesCoords(T, Row1, XHouseCoords, XAux1, YHouseCoords, YAux1), !.

% Goes through a whole row saving the coordinates of the houses in that row
getHousesCoordsAux([], _, _, XHouseCoords, XHouseCoords, YHouseCoords, YHouseCoords).
getHousesCoordsAux([H|T], Row, Col, XHouseCoords, XAux, YHouseCoords, YAux) :-
	
	H = 1,
		Col1 is Col + 1,
		append(XAux, [Col], XAux1),
		append(YAux, [Row], YAux1),
		getHousesCoordsAux(T, Row, Col1, XHouseCoords, XAux1, YHouseCoords, YAux1);
		Col1 is Col + 1,
		getHousesCoordsAux(T, Row, Col1, XHouseCoords, XAux, YHouseCoords, YAux).


separatePairs(_, FinalPairsA, FinalPairsA, FinalPairsB, FinalPairsB, PairsLength, PairsLength).
separatePairs(AllPairs, FinalPairsA, AuxA, FinalPairsB, AuxB, Num, PairsLength) :-

	Num1 is Num + PairsLength,
	nth0(Num, AllPairs, Elem1),
	nth0(Num1, AllPairs, Elem2),

	append(AuxA, [Elem1], AuxA1),
	append(AuxB, [Elem2], AuxB1),
	NumAux is Num + 1,
	separatePairs(AllPairs, FinalPairsA, AuxA1, FinalPairsB, AuxB1, NumAux, PairsLength).


showResults([], _, _, _).
showResults([HA|TA], [HB|TB], [HDist|TDist], Number) :-
	
	write('Pair '), write(Number), write(': '),
	write('H'), write(HA), write(' connects to '),
	write('H'), write(HB), write(' with a distance of '),
	write(HDist), nl,

	Number1 is Number + 1,
	showResults(TA, TB, TDist, Number1).