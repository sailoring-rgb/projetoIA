:- consult('baseConhecimento2.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%-------------------------------Pesquisa em Profundidade (DFS)-------------------------------

% # Caminho: Green Distribuition -> Ponto de Entrega -> Green Distribuition
% # Distância: Custo do Circuito Inteiro.
resolveDFS(Nodo,Caminho,Distancia) :-
    profundidade(Nodo,[Nodo],CaminhoVolta,Dist),
    inverso(CaminhoVolta,CaminhoIda),
    append(CaminhoIda,[Nodo|CaminhoVolta],Caminho),
    Distancia is Dist*2.

profundidade(Nodo,_,[],0) :- goal(Nodo).
profundidade(Nodo,Historico,[ProxNodo|Caminho],DistanciaT) :-
    adjacente(Nodo,ProxNodo,Distancia1),
    nao(membro(ProxNodo,Historico)),
    profundidade(ProxNodo,[ProxNodo|Historico],Caminho,Distancia2),
    DistanciaT is Distancia1 + Distancia2.

%---------------------------------Pesquisa em Largura (BFS)---------------------------------

% # Caminho: Green Distribuition -> Ponto de Entrega -> Green Distribuition
% # Distância: Custo do Circuito Inteiro.
resolveBFS(Nodo,Caminho,Distancia) :-
    goal(NodoFinal),
    largura(NodoFinal,[[Nodo]],CaminhoAux,Dist),
    apagaCabeca(CaminhoAux,CaminhoVolta),
    inverso(CaminhoVolta,CaminhoIda),
    append(CaminhoIda,[Nodo|CaminhoVolta],Caminho),
    Distancia is Dist*2.

largura(NodoFinal,[[NodoFinal|T]|_],Caminho,0) :- inverso([NodoFinal|T],Caminho).
largura(NodoFinal,[Lista|Outros],Caminho,DistanciaT) :-
    Lista = [A|_],
    findall([X|Lista],(NodoFinal \== A, adjacente(A,X,_),nao(membro(X,Lista))),Novos),
    adjacente(A,X,Distancia1),
    concatena(Outros,Novos,Todos),
    largura(NodoFinal,Todos,Caminho,Distancia2),
    DistanciaT is Distancia1 + Distancia2.

%------------------------------Pesquisa em Profundidade Limitada------------------------------

% # Caminho: Green Distribuition -> Ponto de Entrega -> Green Distribuition
% # Distância: Custo do Circuito Inteiro.
% # Limite - Número limite de nós a procurar.
resolveLimitada(Nodo,Caminho,Distancia,Limite) :-
    profundidadeLimitada(Nodo,[Nodo],CaminhoAux,Dist,Limite),
    apagaCabeca(CaminhoAux,CaminhoVolta),
    inverso(CaminhoVolta,CaminhoIda),
    append(CaminhoIda,[Nodo|CaminhoVolta],Caminho),
    Distancia is Dist*2.

profundidadeLimitada(Nodo,_,[],0,_) :- goal(Nodo).
profundidadeLimitada(Nodo,Historico,[ProxNodo|Caminho],DistanciaT,Limite) :-
    Limite > 0,
    adjacente(Nodo,ProxNodo,Distancia1),
    nao(membro(ProxNodo,Historico)),
    Limite1 is Limite-1,
    profundidadeLimitada(ProxNodo,[ProxNodo|Historico],Caminho,Distancia2,Limite1),
    DistanciaT is Distancia1 + Distancia2.

%--------------------------------------Pesquisa Gulosa--------------------------------------

% # Caminho: Green Distribuition -> Ponto de Entrega -> Green Distribuition
% # Custo: Custo do Circuito Inteiro.
resolveGulosa(Nodo,Caminho/Custo) :-
    estima(Nodo,Estima),
    agulosa([[Nodo]/0/Estima],CaminhoIda/CustoIda/_),
    inverso(CaminhoIda,CaminhoAux),
    apagaCabeca(CaminhoAux,CaminhoVolta),
    append(CaminhoIda,CaminhoVolta,Caminho),
    Custo is CustoIda*2.

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

% # Caminho: Green Distribuition -> Ponto de Entrega -> Green Distribuition
% # Custo: Custo do Circuito Inteiro.
resolveAEstrela(Nodo,Caminho/Custo) :-
    estima(Nodo,Estima),
    aestrela([[Nodo]/0/Estima],CaminhoIda/CustoIda/_),
    inverso(CaminhoIda,CaminhoAux),
    apagaCabeca(CaminhoAux,CaminhoVolta),
    append(CaminhoIda,CaminhoVolta,Caminho),
    Custo is CustoIda*2.

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

%--------------------------------------Auxiliares Para o Circuito--------------------------------------
/*
% Devolve o destino da encomenda, ou seja, a freguesia onde a mesma é entregue
destinoEncomenda(IdEnc,Destino) :-
    estafetaFezEncomenda(IdEstaf,IdEnc),
    estafeta(IdEstaf,Lista),
    membro((IdEnc,A,B,Destino),Lista).
*/

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

% Devolve as encomendas de um determinado estafeta, com aquele ponto de entrega
encomendasEstafFreg(IdEstaf,PontoEntrega,Lista) :-
    encomendasDoEstafeta(IdEstaf,ListaEnc),
    encomendasEstafFreg2(PontoEntrega,ListaEnc,Lista).

encomendasEstafFreg2(_,[],[]).
encomendasEstafFreg2(PontoEntrega,[(IdEnc,_,_,Freg)],[IdEnc]) :- PontoEntrega == Freg.
encomendasEstafFreg2(PontoEntrega,[(IdEnc,_,_,Freg)|T],Lista) :-
    ((PontoEntrega == Freg,
     encomendasEstafFreg2(PontoEntrega,T,Lista0),
     adiciona(IdEnc,Lista0,Lista));
     encomendasEstafFreg2(PontoEntrega,T,Lista)).

distanciaCircuito([Freg],0).
distanciaCircuito([Freg,NextFreg],D) :- adjacente(Freg,NextFreg,D). 
distanciaCircuito([Freg,NextFreg | T],D) :-  
    adjacente(Freg,NextFreg,Dist),
    distanciaCircuito([NextFreg | T],D1),
    D is (Dist + D1)*2.
/*
% Devolve o tempo de entrega de uma encomenda, consoante o tipo de pesquisa adotado:
% # 1 - DFS
% # 2 - BFS
% # 3 - DFS Limitada
% # 4 - Gulosa
% # 5 - A*
tempoEntrega(IdEnc,Tempo,1) :-
    destinoEncomenda(IdEnc,Destino),
    resolveDFS(Destino,[Destino|Caminho],Distancia),
    velocidadeEntrega(IdEnc,Velocidade),
    Tempo is Distancia/Velocidade.

tempoEntrega(IdEnc,Tempo,2) :-
    destinoEncomenda(IdEnc,Destino),
    resolveBFS(Destino,Caminho,Distancia),
    velocidadeEntrega(IdEnc,Velocidade),
    Tempo is Distancia/Velocidade.

tempoEntrega(IdEnc,Tempo,3) :-
    destinoEncomenda(IdEnc,Destino),
    resolveLimitada(Destino,Caminho,Distancia,5),
    velocidadeEntrega(IdEnc,Velocidade),
    Tempo is Distancia/Velocidade.

tempoEntrega(IdEnc,Tempo,4) :-
    destinoEncomenda(IdEnc,Destino),
    resolveGulosa(Destino,Caminho/Distancia),
    velocidadeEntrega(IdEnc,Velocidade),
    Tempo is Distancia/Velocidade.

tempoEntrega(IdEnc,Tempo,5) :-
    destinoEncomenda(IdEnc,Destino),
    resolveAEstrela(Destino,Caminho/Distancia),
    velocidadeEntrega(IdEnc,Velocidade),
    Tempo is Distancia/Velocidade.
*/
%--------------------------------------Auxiliares Funcionalidade 1--------------------------------------

caminho(A,B,P) :- caminho1(A,B,[B],P).

caminho1(A,A,[A|P1],[A|P1]).
caminho1(A,B,Hist,P) :-
    adjacente(X,B,_),
    nao(membro(X,Hist)), 
    caminho1(A,X,[X|Hist],P).

todosOsCaminhosAux(Territorio,[P],L) :- allCaminhosTerritorio('Green Distribuition',P,Territorio,L).
todosOsCaminhosAux(Territorio,[P | T],L) :- 
    allCaminhosTerritorio('Green Distribuition',P,Territorio,R),
    todosOsCaminhosAux(Territorio,T,L1),
    concatena(R,L1,L).

allCaminhosTerritorio(A,B,T,L) :- findall(Caminho,(caminho(A,B,Caminho),membro(T,Caminho)),L).

%--------------------------------------Auxiliares Funcionalidade 2--------------------------------------

circuitosMaiorNumEntregasAux([C],MaxE,L) :- numEntregasCircuito(C,NumE), (NumE == MaxE -> adiciona(C,L1,L) ; L = []).
circuitosMaiorNumEntregasAux([C | T],MaxE,L) :- 
    numEntregasCircuito(C,NumE),
    circuitosMaiorNumEntregasAux(T,MaxE,L1),
    (NumE == MaxE -> inverso(C,CAux),
    apagaCabeca(CAux,CV),
    append(C,CV,Circuito),adiciona(Circuito,L1,L) ; L = L1).

maiorNumEntregasCircuito([C],Max) :- numEntregasCircuito(C,Max).
maiorNumEntregasCircuito([C | T],Max) :-
    numEntregasCircuito(C,NumE),
    maiorNumEntregasCircuito(T,Max1),
    (NumE > Max1 -> Max = NumE; Max = Max1).

numEntregasCircuito([],0).
numEntregasCircuito([Freg],N) :- findall(IdEstaf, estafeta(IdEstaf,_), Ets), contaEntregasFreguesia(Freg,Ets,N).
numEntregasCircuito([Freg | T],N) :- 
    findall(IdEstaf, estafeta(IdEstaf,_), Ets),
    contaEntregasFreguesia(Freg,Ets,Num),
    numEntregasCircuito(T,N1),
    N is Num + N1.

allCaminhos(A,L) :- findall(Caminho,(caminho(A,B,Caminho), A\=B),L).


%--------------------------------------Auxiliares Funcionalidade 4--------------------------------------

circuitoMaisRapidoAux([C],_,C).
circuitoMaisRapidoAux([C | T],D,Circuito) :- 
    distanciaCircuito(C,Dist),
    circuitoMaisRapidoAux(T,D,C1),
    (Dist == D -> Circuito = C; Circuito = C1).

distanciaCircuitoMaisRapido([C],D) :- distanciaCircuito(C,D).
distanciaCircuitoMaisRapido([C | T], D) :- 
    distanciaCircuito(C,Dist),
    distanciaCircuitoMaisRapido(T,D1),
    (Dist < D1 -> D = Dist; D = D1).


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

apagaCabeca([],[]).
apagaCabeca([X],[]).
apagaCabeca([H|T],T).