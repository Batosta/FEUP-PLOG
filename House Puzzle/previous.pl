% 1 means house, 0 means blank space
board([
	[1, 0, 1, 1],
	[0, 0, 1, 1],
	[0, 0, 1, 0],
	[1, 0, 0, 1]
]).

houseCoords([[0,0],[0,2],[0,3],[1,2],[1,3],[2,2],[3,0],[3,3]]).

board1([
	[1, 0, 1, 0, 1, 0],
	[1, 0, 0, 0, 0, 1],
	[1, 0, 0, 1, 0, 0],
	[0, 0, 0, 0, 0, 0],
	[1, 0, 1, 0, 0, 1],
	[1, 0, 0, 0, 1, 0]
]).


% Returns in Dist the distance between the two points with coordinates (R1, C1)-(R2, C2)
distancePoints(R1, C1, R2, C2, Dist) :-
	
	DiffRow is abs(R1 - R2),
	DiffCol is abs(C1 - C2),
	DiffRow2 is DiffRow * DiffRow,
	DiffCol2 is DiffCol * DiffCol,
	Dist is DiffRow2 + DiffCol2.



% Returns in Dist the distance between the two points with coordinates in the lists [H1|T1], [H2|T2]
distanceLists([H1|T1], [H2|T2], Dist) :-
	
	DiffRow is abs(H1 - H2),
	distanceListsAux(T1, C1),
	distanceListsAux(T2, C2),
	DiffCol is abs(C1 - C2),
	DiffRow2 is DiffRow * DiffRow,
	DiffCol2 is DiffCol * DiffCol,
	Dist is DiffRow2 + DiffCol2.
distanceListsAux([H1|_], Col) :-

	Col is H1.



% Goes through the whole board saving the coordinates of the houses
getHousesCoords([], _, HouseCoords, HouseCoords).
getHousesCoords([H|T], Row, HouseCoords, Aux) :-
	
	getHousesCoordsAux(H, Row, 0, HouseCoordsAux, []),
	append(Aux, HouseCoordsAux, Aux1),
	Row1 is Row + 1,
	getHousesCoords(T, Row1, HouseCoords, Aux1), !.
% Goes through a whole row saving the coordinates of the houses in that row
getHousesCoordsAux([], _, _, HouseCoords, HouseCoords).
getHousesCoordsAux([H|T], Row, Col, HouseCoords, Aux) :-
	
	H = 1,
		Col1 is Col + 1,
		append(Aux, [[Row, Col]], Aux1),
		getHousesCoordsAux(T, Row, Col1, HouseCoords, Aux1);
		Col1 is Col + 1,
		getHousesCoordsAux(T, Row, Col1, HouseCoords, Aux).



% Returns a TRIANGULAR matrix that has every distance from one point to another
getDistancesArray([], [], AllDistances, AllDistances).
getDistancesArray([_|T], [H1|T1], AllDistances, Aux) :-
	getDistancesArrayAux(T, H1, AllDistanceAux, []),
	AllDistanceAux = [],
		getDistancesArray(T, T1, AllDistances, Aux), !;
		getDistancesArrayAux(T, H1, AllDistanceAux, []),
		append(Aux, [AllDistanceAux], Aux1),
		getDistancesArray(T, T1, AllDistances, Aux1), !.
getDistancesArrayAux([], _, AllDistanceAux, AllDistanceAux).
getDistancesArrayAux([H|T], H1, AllDistanceAux, Aux) :-
	distanceLists(H, H1, Dist),
	append(Aux, [Dist], Aux1),
	getDistancesArrayAux(T, H1, AllDistanceAux, Aux1).



% Returns a REGULAR matrix that has every distance from one point to another
getDistancesArray1(_, [], AllDistances, AllDistances).
getDistancesArray1([H|T], [H1|T1], AllDistances, Aux) :-
	
	getDistancesArray1Aux([H|T], H1, AllDistanceAux, []),
	append(Aux, [AllDistanceAux], Aux1),
	getDistancesArray1([H|T], T1, AllDistances, Aux1).
getDistancesArray1Aux([], _, AllDistanceAux, AllDistanceAux).
getDistancesArray1Aux([H|T], H1, AllDistanceAux, Aux) :-
	
	distanceLists(H, H1, Dist),
	Dist \= 0,
		append(Aux, [Dist], Aux1),
		getDistancesArray1Aux(T, H1, AllDistanceAux, Aux1);
		getDistancesArray1Aux(T, H1, AllDistanceAux, Aux).


% Returns a list that has every distance from one point to another
getDistancesArray2(_, [], AllDistances, AllDistances).
getDistancesArray2([H|T], [H1|T1], AllDistances, Aux) :-

	getDistancesArray1Aux([H|T], H1, AllDistanceAux, []),
	append(Aux, AllDistanceAux, Aux1),
	getDistancesArray1([H|T], T1, AllDistances, Aux1).


% Finds the maximum value in a list
findMaxList([], Max, Max).
findMaxList([H|T], Max, Aux) :-
	
	H @> Aux,
		Aux1 is H,
		findMaxList(T, Max, Aux1);
		findMaxList(T, Max, Aux).

% Finds the minimum value in a list
findMinList([], Min, Min).
findMinList([H|T], Min, Aux) :-
	
	H @< Aux,
		Aux1 is H,
		findMinList(T, Min, Aux1);
		findMinList(T, Min, Aux).



% Finds the maximum value in a list of lists
findMaxListLists([], Max, Max).
findMaxListLists([H|T], Max, Aux) :-
	
	findMaxList(H, Value, -1),
	Value @> Aux,
		findMaxListLists(T, Max, Value);
		findMaxListLists(T, Max, Aux).

% Finds the minimum value in a list of lists
findMinListLists([], Min, Min).
findMinListLists([H|T], Min, Aux) :-

	findMinList(H, Value, 10000),
	Value @< Aux,
		findMinListLists(T, Min, Value);
		findMinListLists(T, Min, Aux).


getDiffDistArray([], AllDiffDist, AllDiffDist).
getDiffDistArray([H|T], AllDiffDist, SoFarDiffDist) :-
	
	getDiffDistArrayAux(H, NewSoFarDiffDist, []),
	%getDiffDistArrayAux(NewSoFarDiffDist, NewSoFarDiffDist2, SoFarDiffDist),
	write(NewSoFarDiffDist), nl,
	append(SoFarDiffDist, NewSoFarDiffDist, SoFarDiffDist1),
	getDiffDistArray(T, AllDiffDist, SoFarDiffDist1).


getDiffDistArrayAux([], NewSoFarDiffDist, NewSoFarDiffDist).
getDiffDistArrayAux([H|T], NewSoFarDiffDist, Aux) :-
	
	member(H, Aux),
	getDiffDistArrayAux(T, NewSoFarDiffDist, Aux);
	append(Aux, [H], Aux1),
	getDiffDistArrayAux(T, NewSoFarDiffDist, Aux1).