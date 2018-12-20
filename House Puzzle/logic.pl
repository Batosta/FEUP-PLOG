logicMain(X) :-
	
	getNumberHouses(X, NumberHouses, 0),
	write('NumberHouses: '), write(NumberHouses), nl, nl,

	getHousesCoords(X, 0, XHousesCoords, [], YHousesCoords, []),
	write('XHousesCoords: '), write(XHousesCoords), nl,
	write('YHousesCoords: '), write(YHousesCoords), nl, nl,

	PairsLength is div(NumberHouses, 2),
	length(PairsA, PairsLength),
	length(PairsB, PairsLength),
	domain(PairsA, 1, NumberHouses),
	domain(PairsB, 1, NumberHouses),

	append(PairsA, PairsB, AllPairs),
	all_distinct(AllPairs),

	write('PairsA: '), write(PairsA), nl,
	write('PairsB: '), write(PairsB), nl,

	solver(XHousesCoords, YHousesCoords, PairsA, PairsB, Distances),
	nvalue(2, Distances),

	
	labeling([ffc], AllPairs),
	write(AllPairs).

	%juntar o PairsA e o PairsB e dar lhe labeling - fim logicMain



solver(_, _, [], [], []).
solver(XHousesCoords, YHousesCoords, [HA|TA], [HB|TB], [HL|TL]) :-

	element(HA, XHousesCoords, XA),
	element(HA, YHousesCoords, YA),
	element(HB, XHousesCoords, XB),
	element(HB, YHousesCoords, YB),

	HL #= ((XA - XB)*(XA - XB) + (YA - YB)*(YA - YB)),

	solver(XHousesCoords, YHousesCoords, TA, TB, TL).