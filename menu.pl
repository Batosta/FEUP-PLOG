firstMenu :-
	printFirstMenu.

printFirstMenu :-
	write('______________________________________________'), nl,
	write('                                              '), nl,
	write('                  KNIGHT LINE                 '), nl,
	write('                                              '), nl,
	write('                                              '), nl,
	write('                                              '), nl,
	write('          Joao Nuno Rodrigues Ferreira        '), nl,
	write('      Joao Miguel Vaz Tello da Gama Amaral    '), nl,
	write('______________________________________________'), nl, nl,
	mainMenu.

mainMenu :-
	write('1. Player vs Player'), nl,
	write('2. Player vs Computer'), nl,
	write('3. Player vs Computer(Smart)'), nl,
	write('0. Exit'), nl, nl,
	write('Please insert a valid option: '),
	read(Input),
	handleInput(Input).

handleInput(0) :-
	write('Exiting').

handleInput(1) :-
	playPLPL.

handleInput(2) :-
	playPLPC.

handleInput(3) :-
	playPLPC2.

handleInput(_Other) :-
	write('Invalid input.'), nl,
	write('Please insert a valid option: '),
	read(Input),
	handleInput(Input).