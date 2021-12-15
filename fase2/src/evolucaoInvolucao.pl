:- consult('baseConhecimento.pl').
:- consult('auxiliares.pl').
:- consult('invariantes.pl').

:- op( 900,xfy,'::' ).

%EVOLUCAO

% Extensao do predicado evolucao: Termo -> {V, F}
evolucao(Termo) :-
    solucoes(Invariantes,+Termo::Invariantes,Lista),
    insercao(Termo),
    teste(Lista).

%INVOLUCAO

% Extensao do predicado involucao: Termo -> {V, F}
involucao(Termo) :- Termo,
    solucoes(Invariantes,-Termo::Invariantes, Lista),
    remocao(Termo),
    teste(Lista).

% Extensao do predicado insercao: Termo -> {V, F}
insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo),!,fail.

% Extensao do predicado remocao: Termo -> {V, F}
remocao(Termo) :- retract(Termo).
remocao(Termo) :- assert(Termo), !, fail.

% Extensao do predicado teste: Lista -> {V, F}
teste([]).
teste([R|LR]) :- R, teste(LR).
