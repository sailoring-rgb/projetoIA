:- consult('baseConhecimento.pl').
:- consult('aux.pl').

%Funcionalidades

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado estafetasEncomendasCliente: Id,Lista,Resultado -> {V,F}
% Identificar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente

estafetasEncomendasCliente([IdEnc|Es]) :- 
	estafetasEncCliente(IdEnc,R),
	estafetasEncomendasCliente(Es).

estafetasEncCliente(IdEnc,L) :-
	solucoes(IdEstafeta,estafeta(IdEstafeta,IdEnc,_,_),L).

