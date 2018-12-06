% Shows credits + the board given
menu(X) :-
	write('______________________________________________'), nl,
	write('                                              '), nl,
	write('                 HOUSE PUZZLE                 '), nl,
	write('                                              '), nl,
	write('                                              '), nl,
	write('                                              '), nl,
	write('          Joao Nuno Rodrigues Ferreira        '), nl,
	write('      Joao Miguel Vaz Tello da Gama Amaral    '), nl,
	write('______________________________________________'), nl, nl,
	write('Board:'), nl,
	printBoard(X).