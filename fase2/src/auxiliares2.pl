:- consult('baseConhecimento.pl').
:- consult('auxiliares1.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%--------------------------------------Usadas em várias funcionalidades--------------------------------------

destinoEncomenda(IdEnc,Destino) :-
    estafetaFezEncomenda(IdEstaf,IdEnc),
    estafeta(IdEstaf,Lista),
    membro((IdEnc,A,B,Destino),Lista).

%Bicicleta - 10 km/h
%Moto - 35 km/h
%Carro - 25 km/h
velocidadeEntrega(IdEnc,Velocidade) :-
    encomenda(IdEnc,_,Peso,_,_,_,_),
    transporteEncomenda(IdEnc,Transporte),
    ((Transporte == 'Bicicleta' -> Velocidade is 10 - Peso * 0.7);
     (Transporte == 'Moto' -> Velocidade is 35 - Peso * 0.5);
     (Transporte == 'Carro' -> Velocidade is 25 - Peso * 0.1)).

/* Este método NÃO DEVOLVE o melhor caminho (de menor custo) */
caminho(Origem,Destino,Caminho,Km) :-
      g(G),
      caminhoAux(G,Origem,[Destino],Caminho,Km).

caminhoAux(_,A,[A|P1],[A|P1],0).
caminhoAux(G,A,[Y|P1],P,K1) :- 
    adjacente(X,Y,Ki,G),
    nao(membro(X,[Y|P1])),
    caminhoAux(G,A,[X,Y|P1],P,K),
    K1 is K + Ki.

/*
O método tempoEntrega está a bater mal porque existem dois caminhos desde a Green Distribuition até São Victor.
FALTA, POR ISSO, IMPLEMENTAR O MÉTODO PARA CALCULAR O CAMINHO MAIS CURTO (TALVEZ PELO ALGORITMO A*).
Nesse método, tem de ser devolvido não só o caminho mais curto, como a distância do caminho mais curto.
*/
tempoEntrega(IdEnc,TempoTotal) :-
    destinoEncomenda(IdEnc,Destino),
    melhorCaminho(greenDistribuition,Destino,Caminho,DistanciaTotal),
    velocidadeEntrega(IdEnc,Velocidade),
    TempoTotal is DistanciaTotal / Velocidade.

%---------------------------------------------------Anexos---------------------------------------------------

adjacente(X,Y,D,grafo(Es1,Es2)) :- member(aresta(X,Y,D),Es2).
adjacente(X,Y,D,grafo(Es1,Es2)) :- member(aresta(Y,X,D),Es2).