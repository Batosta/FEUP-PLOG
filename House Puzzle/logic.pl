logicMain(X) :-
	
	getNumberHouses(X, NumberHouses, 0),

	getHousesCoords(X, 0, XHousesCoords, [], YHousesCoords, []),

	PairsLength is div(NumberHouses, 2),
	length(PairsA, PairsLength),
	length(PairsB, PairsLength),
	domain(PairsA, 1, NumberHouses),
	domain(PairsB, 1, NumberHouses),

	solver(XHousesCoords, YHousesCoords, PairsA, PairsB, Distances),
	nvalue(2, Distances),

	append(PairsA, PairsB, AllPairs),
	all_distinct(AllPairs),
	labeling([ffc], AllPairs),

	separatePairs(AllPairs, FinalPairsA, [], FinalPairsB, [], 0, PairsLength),

	write('Results: '), nl, nl,
	showResults(FinalPairsA, FinalPairsB, Distances, 0), nl, nl.



solver(_, _, [], [], []).
solver(XHousesCoords, YHousesCoords, [HA|TA], [HB|TB], [HL|TL]) :-

	element(HA, XHousesCoords, XA),
	element(HA, YHousesCoords, YA),
	element(HB, XHousesCoords, XB),
	element(HB, YHousesCoords, YB),

	HL #= ((XA - XB)*(XA - XB) + (YA - YB)*(YA - YB)),

	solver(XHousesCoords, YHousesCoords, TA, TB, TL).