:- consult('baseConhecimento2.pl').
:- consult('auxiliares1.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%-------------------------------Pesquisa em Profundidade (DFS)-------------------------------

% # Caminho: Green Distribuition -> Ponto de Entrega -> Green Distribuition
% # Distância: Custo do Circuito Inteiro.
resolveDFS(Nodo,Caminho,Distancia,Quantidade) :-
    profundidade(Nodo,[Nodo],CaminhoVolta,Dist),
    numEntregasCircuito([Nodo|CaminhoVolta],_,Quantidade),
    inverso(CaminhoVolta,CaminhoIda),
    append(CaminhoIda,[Nodo|CaminhoVolta],Caminho),
    Distancia is Dist*2.

profundidade(Nodo,_,[],0) :- goal(Nodo).
profundidade(Nodo,Historico,[ProxNodo|Caminho],DistanciaT) :-
    g(G),adjacente(Nodo,ProxNodo,Distancia1,G),
    nao(membro(ProxNodo,Historico)),
    profundidade(ProxNodo,[ProxNodo|Historico],Caminho,Distancia2),
    DistanciaT is Distancia1 + Distancia2.

%---------------------------------Pesquisa em Largura (BFS)---------------------------------

% # Caminho: Green Distribuition -> Ponto de Entrega -> Green Distribuition
% # Distância: Custo do Circuito Inteiro.
resolveBFS(Nodo,Caminho,Distancia,Quantidade) :-
    goal(NodoFinal),
    largura(NodoFinal,[[Nodo]],CaminhoAux,Dist),
    numEntregasCircuito(CaminhoAux,_,Quantidade),
    apagaCabeca(CaminhoAux,CaminhoVolta),
    inverso(CaminhoVolta,CaminhoIda),
    append(CaminhoIda,[Nodo|CaminhoVolta],Caminho),
    Distancia is Dist*2.

largura(NodoFinal,[[NodoFinal|T]|_],Caminho,0) :- inverso([NodoFinal|T],Caminho).
largura(NodoFinal,[Lista|Outros],Caminho,DistanciaT) :-
    g(G),Lista = [A|_],
    findall([X|Lista],(NodoFinal \== A, adjacente(A,X,_,G),nao(membro(X,Lista))),Novos),
    adjacente(A,X,Distancia1,G),
    concatena(Outros,Novos,Todos),
    largura(NodoFinal,Todos,Caminho,Distancia2),
    DistanciaT is Distancia1 + Distancia2.

%------------------------------Pesquisa em Profundidade Limitada------------------------------

% # Caminho: Green Distribuition -> Ponto de Entrega -> Green Distribuition
% # Distância: Custo do Circuito Inteiro.
% # Limite - Número limite de nós a procurar.
resolveLimitada(Nodo,Caminho,Distancia,Quantidade,Limite) :-
    profundidadeLimitada(Nodo,[Nodo],CaminhoAux,Dist,Limite),
    numEntregasCircuito(CaminhoAux,_,Quantidade),
    apagaCabeca(CaminhoAux,CaminhoVolta),
    inverso(CaminhoVolta,CaminhoIda),
    append(CaminhoIda,[Nodo|CaminhoVolta],Caminho),
    Distancia is Dist*2.

profundidadeLimitada(Nodo,_,[],0,_) :- goal(Nodo).
profundidadeLimitada(Nodo,Historico,[ProxNodo|Caminho],DistanciaT,Limite) :-
    Limite > 0,g(G),
    adjacente(Nodo,ProxNodo,Distancia1,G),
    nao(membro(ProxNodo,Historico)),
    Limite1 is Limite-1,
    profundidadeLimitada(ProxNodo,[ProxNodo|Historico],Caminho,Distancia2,Limite1),
    DistanciaT is Distancia1 + Distancia2.

%--------------------------------------Pesquisa Gulosa--------------------------------------

% # Caminho: Green Distribuition -> Ponto de Entrega -> Green Distribuition
% # Custo: Custo do Circuito Inteiro.
resolveGulosa(Nodo,Caminho/Custo,Quantidade) :-
    estima(Nodo,Estima),
    agulosa([[Nodo]/0/Estima],CaminhoIda/CustoIda/_),
    numEntregasCircuito(CaminhoIda,_,Quantidade),
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
resolveAEstrela(Nodo,Caminho/Custo,Quantidade) :-
    estima(Nodo,Estima),
    aestrela([[Nodo]/0/Estima],CaminhoIda/CustoIda/_),
    numEntregasCircuito(CaminhoIda,_,Quantidade),
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

meioDeTransporteUsado(C,PesoTotal,Transporte) :-
    numEntregasCircuito(C,PesoTotal,_),
    ((PesoTotal =< 5 -> Transporte = 'Bicicleta');
     (PesoTotal > 5, PesoTotal =< 20 -> Transporte = 'Mota');
     (PesoTotal > 20, PesoTotal =< 100 -> Transporte = 'Carro')).

% Devolve a velocidade a que uma encomenda foi entregue
% # Bicicleta - 10 km/h
% # Moto - 35 km/h
% # Carro - 25 km/h
velocidadeEntrega(Transporte,Peso,Velocidade) :-
    ((Transporte == 'Bicicleta' -> Velocidade is 10 - Peso * 0.7);
     (Transporte == 'Mota' -> Velocidade is 35 - Peso * 0.5);
     (Transporte == 'Carro' -> Velocidade is 25 - Peso * 0.1)).

distanciaCircuito([Freg],0).
distanciaCircuito([Freg,NextFreg],D) :- g(G),adjacente(Freg,NextFreg,D,G). 
distanciaCircuito([Freg,NextFreg|T],D) :-
    g(G),adjacente(Freg,NextFreg,Dist,G),
    distanciaCircuito([NextFreg|T],D1),
    D is (Dist + D1)*2.

tempoCircuito(C,T) :- 
    distanciaCircuito(C,Distancia),
    meioDeTransporteUsado(C,PesoTotal,Transporte),
    velocidadeEntrega(Transporte,PesoTotal,Velocidade),
    T is Distancia/Velocidade.
    
numEntregasCircuito([],0,0).
numEntregasCircuito([Freg],PesoTotal,N) :-
    findall((IdEnc,Peso),encomenda(IdEnc,_,Peso,_,Freg),L),
    somaPesos(L,PesoTotal),
    comprimento(L,N).
numEntregasCircuito([Freg|T],PesoTotal,N) :-
    findall((IdEnc,Peso),encomenda(IdEnc,_,Peso,_,Freg),L),
    somaPesos(L,PesoTotal1),
    comprimento(L,N1),
    numEntregasCircuito(T,PesoTotal2,N2),
    PesoTotal is PesoTotal1 + PesoTotal2,
    N is N1 + N2.

allCaminhos(A,L) :- findall(Caminho,(caminho(A,B,Caminho),A\=B),L).

%--------------------------------------Auxiliares Funcionalidade 1--------------------------------------

caminho(A,B,P) :- g(G),caminho1(G,A,B,[B],P).

caminho1(G,A,A,[A|P1],[A|P1]).
caminho1(G,A,B,Hist,P) :-
    adjacente(X,B,_,G),
    nao(membro(X,Hist)), 
    caminho1(G,A,X,[X|Hist],P).

todosOsCaminhosAux(Territorio,[P],L) :- allCaminhosTerritorio('Green Distribuition',P,Territorio,L).
todosOsCaminhosAux(Territorio,[P|T],L) :- 
    allCaminhosTerritorio('Green Distribuition',P,Territorio,R),
    todosOsCaminhosAux(Territorio,T,L1),
    concatena(R,L1,L).

allCaminhosTerritorio(A,B,T,L) :- findall(Caminho,(caminho(A,B,Caminho),membro(T,Caminho)),L).

%--------------------------------------Auxiliares Funcionalidade 2--------------------------------------

circuitosMaiorNumEntregasAux([C],MaxE,L) :- numEntregasCircuito(C,_,NumE), (NumE == MaxE -> adiciona(C,L1,L) ; L = []).
circuitosMaiorNumEntregasAux([C|T],MaxE,L) :- 
    numEntregasCircuito(C,_,NumE),
    circuitosMaiorNumEntregasAux(T,MaxE,L1),
    (NumE == MaxE -> inverso(C,CAux),
    apagaCabeca(CAux,CV),
    append(C,CV,Circuito),adiciona(Circuito,L1,L) ; L = L1).

maiorNumEntregasCircuito([C],Max) :- numEntregasCircuito(C,_,Max).
maiorNumEntregasCircuito([C|T],Max) :-
    numEntregasCircuito(C,_,NumE),
    maiorNumEntregasCircuito(T,Max1),
    (NumE > Max1 -> Max = NumE; Max = Max1).

%--------------------------------------Auxiliares Funcionalidade 4--------------------------------------

circuitoMaisRapidoAux([],0,_).
circuitoMaisRapidoAux([C],D,C) :- distanciaCircuito(C,D).
circuitoMaisRapidoAux([C|T],Min,Circuito) :- 
    distanciaCircuito(C,Dist1),
    circuitoMaisRapidoAux(T,Dist2,C1),
    ((Dist1 < Dist2 -> Min = Dist1, Circuito = C);
     Min = Dist2, Circuito = C1).

%--------------------------------------Auxiliares Funcionalidade 5--------------------------------------

circuitoMaisEficienteAux([],0,_).
circuitoMaisEficienteAux([C],T,C) :- tempoCircuito(C,T).
circuitoMaisEficienteAux([C|T],Min,Circuito) :-
    tempoCircuito(C,T1),
    circuitoMaisEficienteAux(T,T2,C1),
    ((T1 < T2 -> Min = T1, Circuito = C);
     Min = T2, Circuito = C1).

%---------------------------------------------------Anexos---------------------------------------------------

adjacente(Nodo,ProxNodo,C,grafo(_,Es)) :- membro(aresta(Nodo,ProxNodo,C),Es).
adjacente(Nodo,ProxNodo,C,grafo(_,Es)) :- membro(aresta(ProxNodo,Nodo,C),Es).

adjacenteV2([Nodo|Caminho]/Custo1/_,[ProxNodo,Nodo|Caminho]/Custo2/Estima) :-
    g(G),adjacente(Nodo,ProxNodo,PassoCusto,G),
	nao(membro(ProxNodo,Caminho)),
	Custo2 is Custo1 + PassoCusto,
	estima(ProxNodo,Estima).

obter_melhor([Caminho],Caminho) :- !.
obter_melhor([Caminho1/Custo1/Estima1,_/Custo2/Estima2|Caminhos],MelhorCaminho) :-
    Estima1 =< Estima2, !,
    obter_melhor([Caminho1/Custo1/Estima1|Caminhos],MelhorCaminho).
obter_melhor([_|Caminhos],MelhorCaminho) :-
    obter_melhor(Caminhos,MelhorCaminho).

somaPesos([],0).
somaPesos([(_,P)],P).
somaPesos([(_,P)|T],N) :- somaPesos(T,N1), N is P + N1.

inverso(Xs,Ys) :- inverso(Xs,[],Ys).
inverso([],Xs,Xs).
inverso([X|Xs],Ys,Zs) :- inverso(Xs,[X|Ys],Zs).

seleciona(E,[E|Xs],Xs).
seleciona(E,[X|Xs],[X|Ys]) :- seleciona(E,Xs,Ys).

apagaCabeca([],[]).
apagaCabeca([X],[]).
apagaCabeca([H|T],T).