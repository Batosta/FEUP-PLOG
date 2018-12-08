:- use_module(library(clpfd)).

:-consult('menu.pl').
:-consult('display.pl').
:-consult('logic.pl').
:-consult('utilities.pl').

% Corresponds to the main function
play(X, D1, D2) :-
	menu(X, D1, D2).