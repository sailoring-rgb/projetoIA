:- consult('baseConhecimento.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%--------------------------------------Auxiliares Para o Caminho--------------------------------------

% Devolve o destino da encomenda, ou seja, a freguesia onde a mesma é entregue
destinoEncomenda(IdEnc,Destino) :-
    estafetaFezEncomenda(IdEstaf,IdEnc),
    estafeta(IdEstaf,Lista),
    membro((IdEnc,A,B,Destino),Lista).

% Devolve a velocidade a que uma encomenda foi entregue
% # Bicicleta - 10 km/h
% # Moto - 35 km/h
% # Carro - 25 km/h
velocidadeEntrega(IdEnc,Velocidade) :-
    encomenda(IdEnc,_,Peso,_,_,_,_),
    transporteEncomenda(IdEnc,Transporte),
    ((Transporte == 'Bicicleta' -> Velocidade is 10 - Peso * 0.7);
     (Transporte == 'Moto' -> Velocidade is 35 - Peso * 0.5);
     (Transporte == 'Carro' -> Velocidade is 25 - Peso * 0.1)).
/*
O método tempoEntrega está a bater mal porque existem dois caminhos desde a Green Distribuition até São Victor.
FALTA, POR ISSO, IMPLEMENTAR O MÉTODO PARA CALCULAR O CAMINHO MAIS CURTO (TALVEZ PELO ALGORITMO A*).
Nesse método, tem de ser devolvido não só o caminho mais curto, como a distância do caminho mais curto.
*/
% Devolve o tempo de entrega de uma encomenda
tempoEntrega(IdEnc,TempoTotal) :-
    destinoEncomenda(IdEnc,Destino),
    melhorCaminho(greenDistribuition,Destino,Caminho,DistanciaTotal),
    velocidadeEntrega(IdEnc,Velocidade),
    TempoTotal is DistanciaTotal / Velocidade.

%---------------------------------------------------Anexos---------------------------------------------------

estafetaFezEncomenda(IdEstaf, IdEnc) :- 
	encomendasDoEstafeta(IdEstaf,L),
	membro(IdEnc,L).

transporteEncomenda(IdEnc,Transporte) :-
	estafetaEncCliente(IdEnc,IdEstaf),
	estafeta(IdEstaf,L),
	procuraTransporteEncomenda(IdEnc,L,Transporte).

procuraTransporteEncomenda(IdEnc,[(IdEnc,_,Transporte,_)],Transporte).
procuraTransporteEncomenda(IdEnc,[(IdEnc,_,Transporte,_)|T],Transporte).
procuraTransporteEncomenda(IdEnc,[(_,_,_,_)|T],Transporte) :- procuraTransporteEncomenda(IdEnc,T,Transporte).

adjacente(X,Y,D,grafo(Es1,Es2)) :- member(aresta(X,Y,D),Es2).
adjacente(X,Y,D,grafo(Es1,Es2)) :- member(aresta(Y,X,D),Es2).

inverso(Xs,Ys) :- inverso(Xs,[],Ys).

inverso([],Xs,Xs).
inverso([X|Xs],Ys,Zs) :- inverso(Xs,[X|Ys],Zs).

seleciona(E,[E|Xs],Xs).
seleciona(E,[X|Xs],[X|Ys]) :- seleciona(E,Xs,Ys).