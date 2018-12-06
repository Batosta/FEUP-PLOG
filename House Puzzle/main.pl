:-consult('menu.pl').
:-consult('display.pl').

% Corresponds to the main function
play(X) :-
	menu(X).