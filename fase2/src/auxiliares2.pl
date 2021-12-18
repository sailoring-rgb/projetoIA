:- consult('baseConhecimento.pl').
:- consult('auxiliares1.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%-------------------------------Pesquisa em Profundidade (DFS)-------------------------------

/*
                FUNCIONAL MAS TALVEZ FALTE:
Se o estafeta realizar mais do que uma entrega num percurso,
tem-se de ter em atenção o transporte que ele usa e a quantidade de peso que ele transporta.
*/
resolveDFS(Nodo,[Nodo|Caminho],Distancia) :-
    profundidade(Nodo,[Nodo],Caminho,Distancia).

profundidade(Nodo,_,[],0) :- goal(Nodo).
profundidade(Nodo,Historico,[ProxNodo|Caminho],DistanciaT) :-
    adjacente(Nodo,ProxNodo,Distancia1),
    nao(membro(ProxNodo,Historico)),
    profundidade(ProxNodo,[ProxNodo|Historico],Caminho,Distancia2),
    DistanciaT is Distancia1 + Distancia2.

%---------------------------------Pesquisa em Largura (BFS)---------------------------------

/*
                FUNCIONAL MAS TALVEZ FALTE:
Se o estafeta realizar mais do que uma entrega num percurso,
tem-se de ter em atenção o transporte que ele usa e a quantidade de peso que ele transporta.
*/
resolveBFS(Nodo,Caminho,Distancia) :-
    goal(NodoFinal),
    largura(NodoFinal,[[Nodo]],Caminho,Distancia).

largura(NodoFinal,[[NodoFinal|T]|_],Caminho,0) :- reverse([NodoFinal|T],Caminho).
largura(NodoFinal,[Lista|Outros],Caminho,DistanciaT) :-
    Lista = [A|_],
    findall([X|Lista],(NodoFinal \== A, adjacente(A,X,_),nao(membro(X,Lista))),Novos),
    adjacente(A,X,Distancia1),
    concatena(Outros,Novos,Todos),
    largura(NodoFinal,Todos,Caminho,Distancia2),
    DistanciaT is Distancia1 + Distancia2.

%------------------------------Pesquisa em Profundidade Limitada------------------------------

/*
                FUNCIONAL MAS TALVEZ FALTE:
Se o estafeta realizar mais do que uma entrega num percurso,
tem-se de ter em atenção o transporte que ele usa e a quantidade de peso que ele transporta.
*/
% # Limite - Número limite de nós a procurar.
resolveLimitada(Nodo,Caminho,Distancia,Limite) :-
    profundidadeLimitada(Nodo,[Nodo],Caminho,Distancia,Limite).

profundidadeLimitada(Nodo,_,[],0,_) :- goal(Nodo).
profundidadeLimitada(Nodo,Historico,[ProxNodo|Caminho],DistanciaT,Limite) :-
    Limite > 0,
    adjacente(Nodo,ProxNodo,Distancia1),
    nao(membro(ProxNodo,Historico)),
    Limite1 is Limite-1,
    profundidadeLimitada(ProxNodo,[ProxNodo|Historico],Caminho,Distancia2,Limite1),
    DistanciaT is Distancia1 + Distancia2.

%--------------------------------------Pesquisa Gulosa--------------------------------------

resolveGulosa(Nodo,Caminho/Custo) :-
    estima(Nodo,Estima),
    agulosa([[Nodo]/0/Estima],Invertido/Custo/_),
    inverso(Invertido,Caminho).

agulosa(Caminhos,Caminho) :-
    obter_melhor(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).
agulosa(Caminhos,Solucao) :-
    obter_melhor(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosa(MelhorCaminho,Expandidos),
    append(OutrosCaminhos,Expandidos,NovosCaminhos),
    agulosa(NovosCaminhos,Solucao).

expande_gulosa(Caminho,Expandidos) :- findall(NovoCaminho,adjacenteV2(Caminho,NovoCaminho), Expandidos).

%------------------------------------Pesquisa A Estrela------------------------------------

resolveAEstrela(Nodo,Caminho/Custo) :-
    estima(Nodo,Estima),
    aestrela([[Nodo]/0/Estima],InvCaminho/Custo/_),
    inverso(InvCaminho,Caminho).

aestrela(Caminhos,Caminho) :-
    obter_melhor(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).

aestrela(Caminhos,SolucaoCaminho) :-
    obter_melhor(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_aestrela(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    aestrela(NovoCaminhos,SolucaoCaminho). 

expande_aestrela(Caminho,ExpCaminhos) :-
    findall(NovoCaminho,adjacenteV2(Caminho,NovoCaminho),ExpCaminhos).

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
     (Transporte == 'Mota' -> Velocidade is 35 - Peso * 0.5);
     (Transporte == 'Carro' -> Velocidade is 25 - Peso * 0.1)).

/* NÃO FUNCIONAL */
calculaQuantidade([],0).
calculaQuantidade([X|Xs],Quantidade):- %getQtLixo('all',_,Q1,X),
							           calculaQuantidade(Xs,Q2),
							           Quantidade is Q1 + Q2.

% Devolve o tempo de entrega de uma encomenda, consoante o tipo de pesquisa adotado:
% # 1 - DFS
% # 2 - BFS
% # 3 - DFS Limitada
% # 4 - Gulosa
% # 5 - A*
tempoEntrega(IdEnc,TempoTotal,1) :-
    destinoEncomenda(IdEnc,Destino),
    resolveDFS(Destino,[Destino|Caminho],Distancia),
    velocidadeEntrega(IdEnc,Velocidade),
    DistanciaTotal is Distancia*2,
    TempoTotal is DistanciaTotal / Velocidade.

tempoEntrega(IdEnc,TempoTotal,2) :-
    destinoEncomenda(IdEnc,Destino),
    resolveBFS(Destino,Caminho,Distancia),
    velocidadeEntrega(IdEnc,Velocidade),
    DistanciaTotal is Distancia*2,
    TempoTotal is DistanciaTotal / Velocidade.

tempoEntrega(IdEnc,TempoTotal,3) :-
    destinoEncomenda(IdEnc,Destino),
    resolveLimitada(Destino,Caminho,Distancia,5),
    velocidadeEntrega(IdEnc,Velocidade),
    DistanciaTotal is Distancia*2,
    TempoTotal is DistanciaTotal / Velocidade.

tempoEntrega(IdEnc,TempoTotal,4) :-
    destinoEncomenda(IdEnc,Destino),
    resolveGulosa(Destino,Caminho/Distancia),
    velocidadeEntrega(IdEnc,Velocidade),
    DistanciaTotal is Distancia*2,
    TempoTotal is DistanciaTotal / Velocidade.

tempoEntrega(IdEnc,TempoTotal,5) :-
    destinoEncomenda(IdEnc,Destino),
    resolveAEstrela(Destino,Caminho/Distancia),
    velocidadeEntrega(IdEnc,Velocidade),
    DistanciaTotal is Distancia*2,
    TempoTotal is DistanciaTotal / Velocidade.

%--------------------------------------Auxiliares Funcionalidade 1--------------------------------------

caminho(A,B,P) :- caminho1(A,[B],P).

caminho1(A,[A|P1], [A|P1]).
caminho1(A,[Y|P1],P) :-
  adjacente(X,Y,_),
  nao(membro(X,[Y|P1])), 
  caminho1(A,[X,Y|P1],P).

todosOsCaminhosAux(Territorio,[P],L) :- allCaminhosTerritorio('Green Distribuition',P,Territorio,L).
todosOsCaminhosAux(Territorio,[P | T],L) :- 
    allCaminhosTerritorio('Green Distribuition',P,Territorio,R),
    todosOsCaminhosAux(Territorio,T,L1),
    concatena(R,L1,L).

allCaminhosTerritorio(A,B,T,L) :- findall(Caminho,(caminho(A,B,Caminho),membro(T,Caminho)),L).

%---------------------------------------------------Anexos---------------------------------------------------

adjacente(Nodo,ProxNodo,C) :- aresta(Nodo,ProxNodo,C).
adjacente(Nodo,ProxNodo,C) :- aresta(ProxNodo,Nodo,C).

adjacenteV2([Nodo|Caminho]/Custo1/_,[ProxNodo,Nodo|Caminho]/Custo2/Estima) :-
    adjacente(Nodo,ProxNodo,PassoCusto),
	\+member(ProxNodo,Caminho),
	Custo2 is Custo1 + PassoCusto,
	estima(ProxNodo,Estima).

obter_melhor([Caminho],Caminho) :- !.
obter_melhor([Caminho1/Custo1/Estima1,_/Custo2/Estima2|Caminhos],MelhorCaminho) :-
    Estima1 =< Estima2, !,
    obter_melhor([Caminho1/Custo1/Estima1|Caminhos],MelhorCaminho).
obter_melhor([_|Caminhos],MelhorCaminho) :-
    obter_melhor(Caminhos,MelhorCaminho).

inverso(Xs,Ys) :- inverso(Xs,[],Ys).

inverso([],Xs,Xs).
inverso([X|Xs],Ys,Zs) :- inverso(Xs,[X|Ys],Zs).

seleciona(E,[E|Xs],Xs).
seleciona(E,[X|Xs],[X|Ys]) :- seleciona(E,Xs,Ys).