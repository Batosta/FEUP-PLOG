:- use_module(library(clpfd)).
:- use_module(library(lists)).

:-consult('menu.pl').
:-consult('display.pl').
:-consult('logic.pl').
:-consult('utilities.pl').
:-consult('boards.pl').

% Corresponds to the main function
play :-
	board7(X),
	menu(X).