logicMain(X, D1, D2) :-
	
	getHousesCoords(X, 0, HouseCoords, []),
	getDistancesArray1(HouseCoords, HouseCoords, AllDistances, []), nl, nl,
	write('HouseCoords: '), write(HouseCoords), nl, nl,
	write('AllDistances: '), write(AllDistances), nl, nl,

	findMaxListLists(AllDistances, Max, -1),
	findMinListLists(AllDistances, Min, 10000),
	write('Max: '), write(Max), write('       Min: '), write(Min), nl, nl,

	domain([D1, D2], Min, Max),
	all_different([D1, D2]).