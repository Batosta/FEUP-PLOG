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
	write('3. Computer vs Computer'), nl,
	write('0. Exit'), nl, nl,
	write('Please insert a valid option: '),
	read(Input),
	handleInput(Input).

handleInput(0) :-
	write('Exiting').

handleInput(1) :-
	write('player vs player'), nl,
	mainMenu.

handleInput(2) :-
	write('player vs pc'), nl,
	mainMenu.

handleInput(3) :-
	write('pc vs pc'), nl,
	mainMenu.	

handleInput(InvalidInput) :-
	write('Invalid input.'), nl,
	write('______________________________________________'), nl, nl,
	mainMenu.