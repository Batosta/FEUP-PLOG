printBoard([H|T]) :-
	length(H, Col),
	nl, write('      '),
	separation1(Col), !, nl,
	display_board([H|T], Col, 0),
	write('      '),
	separation1(Col), !, nl, nl,
	logicMain([H|T]).

% Displays the whole puzzle
display_board([], _, _).
display_board([H|T], Col, Number) :-

	write('     |  '),
	printRow(H, Number, Aux),
	write('  |'), nl,

	write('     |'),
	separation2(Col),
	write('|'), nl,

	display_board(T, Col, Aux).


% Prints a single row
printRow([], Number, Number).
printRow([H|T], Number, Aux) :-
	H = 1,
		printHouse(Number),
		Number1 is Number + 1,
		printRow(T, Number1, Aux);
		
		write('    '),
		printRow(T, Number, Aux).


printHouse(Number) :-
	Number @< 10,
		write(' H'),
		write(Number),
		write(' ');

		write(' H'),
		write(Number).

separation1(-1).
separation1(C) :-
	write('----'),
	C1 is C - 1,
	separation1(C1).

% Prints the separation between 2 rows
separation2(-1).
separation2(C) :-
	write('    '),
	C1 is C - 1,
	separation2(C1).