% 1 means house, 0 means blank space
board([
[1, 0, 1, 1],
[0, 0, 1, 1],
[0, 0, 1, 0],
[1, 0, 0, 1]
]).


printBoard([H|T]) :-
	length(H, Col),
	nl, write('      '),
	separation1(Col), nl,
	display_board([H|T], Col),
	write('      '),
	separation1(Col), nl, nl.

% Displays the whole puzzle
display_board([], _).
display_board([H|T], Col) :-
	write('     | '),
	printRow(H),
	write('  |'), nl,

	write('     |'),
	separation2(Col),
	write('|'), nl,

	display_board(T, Col).


% Prints a single row
printRow([]).
printRow([H|T]) :-
	H = 1,
	write(' H '),
	printRow(T);
	write('   '),
	printRow(T).


separation1(-1).
separation1(C) :-
	write('---'),
	C1 is C - 1,
	separation1(C1).

% Prints the separation between 2 rows
separation2(-1).
separation2(C) :-
	write('   '),
	C1 is C - 1,
	separation2(C1).