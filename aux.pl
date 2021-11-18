:- consult('baseConhecimento.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%-------------------------------------Auxiliares para Funcionalidade 1-------------------------------------

% Conta o número de encomendas cujo transporte foi mais ecológico, ou seja, por bicicleta.
estafetaEncomendasEcologicas(IdEstaf,Conta) :-
	solucoes(IdEnc,estafeta(IdEstaf,IdEnc,_,_),Lista),
	encomendasPorBicicleta(Lista,Conta).

% Devolve o número de encomendas (duma lista) transportadas pelo meio de transporte bicicleta
encomendasPorBicicleta([],0).
encomendasPorBicicleta([IdEnc|T],Conta) :-
	nao(encomenda(IdEnc,_,_,_,_,'Bicicleta')),
	encomendasPorBicicleta(T,Conta).
encomendasPorBicicleta([IdEnc|T],Conta) :-
	encomenda(IdEnc,_,_,_,_,'Bicicleta'),
	encomendasPorBicicleta(T,Conta0),
	Conta is Conta0 + 1.

%-------------------------------------Auxiliares para Funcionalidade 2-------------------------------------

% Devolve os estafetas que entregaram determinada encomenda 
 
estafetasEncCliente(IdEnc,L) :-
	solucoes(IdEstafeta,estafeta(IdEstafeta,IdEnc,_,_),L).

%---------------------------------------------------Extras---------------------------------------------------

% Extensao do predicado pertence: Elemento,Lista -> {V,F}
pertence( X,[X|L] ).
pertence( X,[Y|L] ) :- X \= Y, pertence( X,L ).


% Extensao do meta-predicado nao: Questao -> {V,F}
nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).


% Extensao do meta-predicado membro: Elemento,Lista -> {V,F}
membro(X, [X|_]).
membro(X, [_|Xs]):-
	membro(X, Xs).


solucoes(X,Y,Z) :- findall(X,Y,Z).

% Comprimento de uma lista
comprimento(S,N) :- length(S,N).
