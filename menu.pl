firstMenu :-
	printFirstMenu,
	mainMenu.

printFirstMenu :-
	write('______________________________________________'), nl,
	write('                                              '), nl,
	write('               >>\.	                         '), nl,
	write('              /_  )`.                         '), nl,
	write('             /  _)`^)`.   _.---. _            '), nl,
	write('    		   (_,´ \  `^-)""      `.\           '), nl,
	write('                  |              | \          '), nl,
	write('                  \              / |          '), nl,
	write('                 / \  /.___.´\  (\ (_         '), nl,
	write('                < ,"||     \ |`. \`-´         '), nl,
	write('                 \\ ()      )|  )/            '), nl,
	write('                 |_>|>     /_] //             '), nl,
	write('                   /_]        /_]             '), nl,
	write('                                              '), nl,
	write('          João Nuno Rodrigues Ferreira        '), nl,
	write('      João Miguel Vaz Tello da Gama Amaral    '), nl,
	write('______________________________________________'), nl,
	mainMenu.

mainMenu :-
	write('1. Player vs Player'), nl,
	write('2. Player vs Computer'), nl,
	write('3. Computer vs Computer'), nl,
	write('0. Exit'), nl, nl,
	write('Please, insert your option: '),
	read(input),
	handleInput(input).

handleInput(0) :-
	write('exit').

handleInput(1) :-
	write('player vs player').

handleInput(2) :-
	write('player vs pc').

handleInput(3) :-
	write('pc vs pc').		

handleInput(invalidInput) :-
	write('Invalid input'),
	mainMenu. 
