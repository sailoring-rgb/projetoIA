% Extensão do predicado encomendasEcologicasCount: IdEstaf,Count -> {V,F}
% Conta o número de encomendas cujo transporte foi mais ecológico, ou seja, por bicicleta.

encomendasEcologicasCount(IdEstaf,Count) :-
	solucoes(IdEnc,encomenda(IdEnc,_,IdEstaf,_,_,_,'Bicicleta'),Lista),
	length(List,Count).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pertence: Elemento,Lista -> {V,F}

pertence( X,[X|L] ).
pertence( X,[Y|L] ) :- X \= Y, pertence( X,L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado membro: Elemento,Lista -> {V,F}

membro(X, [X|_]).
membro(X, [_|Xs]):-
	membro(X, Xs).


solucoes(X,Y,Z) :- findall(X,Y,Z).
