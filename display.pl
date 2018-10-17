/*6x6 just for initial test*/
initialBoard([
[empty, empty, empty, empty, empty, empty], 
[empty, empty, empty, empty, empty, empty], 
[empty, empty, black, white, empty, empty], 
[empty, empty, empty, empty, empty, empty], 
[empty, empty, empty, empty, empty, empty], 
[empty, empty, empty, empty, empty, empty] 
]).

symbol(empty, S) :- S='e'.
symbol(white, S) :- S='W'.
symbol(black, S) :- S='B'.

printMatrixInd(X) :-
	nl,
	write('   | 1 | 2 | 3 | 4 | 5 | 6 |\n'),
	write('---|---|---|---|---|---|---|\n'),
	printGameBoard(X, 1).

printGameBoard([], 1).
printGameBoard([Head|Tail], N) :-
	write('  '),
	N1 is N + 1,
	write(' | '),
	printLine(Head),
	write('\n---|---|---|---|---|---|---|\n'),
	printGameBoard(Tail, N1).

printLine([]).
printLine([Head|Tail]) :-
	symbol(Head, S),
	write(S),
	write(' | '),
	printLine(Tail).