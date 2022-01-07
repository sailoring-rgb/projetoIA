:- consult('baseConhecimento2.pl').
:- consult('auxiliares1.pl').

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

resolveDFSTempo(IdEnc,Caminho,Tempo) :-
    encomenda(IdEnc,_,_,_,Nodo),
    profundidade(Nodo,[Nodo],CaminhoVolta,Dist),
    inverso(CaminhoVolta,CaminhoIda),
    append(CaminhoIda,[Nodo|CaminhoVolta],Caminho),
    tempoEntregaEncomenda(IdEnc,Dist,Tempo).

profundidade(Nodo,_,[],0) :- goal(Nodo).
profundidade(Nodo,Historico,[ProxNodo|Caminho],DistanciaT) :-
    g(G),adjacente(Nodo,ProxNodo,Distancia1,G),
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

resolveBFSTempo(IdEnc,Caminho,Tempo) :-
    encomenda(IdEnc,_,_,_,Nodo),
    goal(NodoFinal),
    largura(NodoFinal,[[Nodo]],CaminhoAux,Dist),
    apagaCabeca(CaminhoAux,CaminhoVolta),
    inverso(CaminhoVolta,CaminhoIda),
    append(CaminhoIda,[Nodo|CaminhoVolta],Caminho),
    tempoEntregaEncomenda(IdEnc,Dist,Tempo).

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
resolveLimitada(Nodo,Caminho,Distancia,Limite) :-
    profundidadeLimitada(Nodo,[Nodo],CaminhoAux,Dist,Limite),
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

resolveLimitadaTempo(IdEnc,Caminho,Tempo,Limite) :-
    encomenda(IdEnc,_,_,_,Nodo),
    profundidadeLimitada(Nodo,[Nodo],CaminhoAux,Dist,Limite),
    apagaCabeca(CaminhoAux,CaminhoVolta),
    inverso(CaminhoVolta,CaminhoIda),
    append(CaminhoIda,[Nodo|CaminhoVolta],Caminho),
    tempoEntregaEncomenda(IdEnc,Dist,Tempo).

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

resolveGulosaTempo(IdEnc,Caminho/Custo) :-
    encomenda(IdEnc,_,_,_,Nodo),
    estimaTempoEnc(IdEnc,Nodo,Estima),
    velocidadeEncomenda(IdEnc,V),
    agulosa_tempo(V,[[Nodo]/0/Estima],CaminhoIda/CustoIda/_),
    inverso(CaminhoIda,CaminhoAux),
    apagaCabeca(CaminhoAux,CaminhoVolta),
    append(CaminhoIda,CaminhoVolta,Caminho),
    Custo is CustoIda*2.

agulosa_tempo(_,Caminhos,Caminho) :-
    obter_melhor(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).
agulosa_tempo(V,Caminhos,Solucao) :-
    obter_melhor(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosa_tempo(V,MelhorCaminho,Expandidos),
    append(OutrosCaminhos,Expandidos,NovosCaminhos),
    agulosa_tempo(V,NovosCaminhos,Solucao).

expande_gulosa_tempo(V,Caminho,Expandidos) :- findall(NovoCaminho,adjacenteV3(V,Caminho,NovoCaminho),Expandidos).

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

resolveAEstrelaTempo(IdEnc,Caminho/Custo) :-
    encomenda(IdEnc,_,_,_,Nodo),
    estimaTempoEnc(IdEnc,Nodo,Estima),
    velocidadeEncomenda(IdEnc,V),
    aestrela_tempo(V,[[Nodo]/0/Estima],CaminhoIda/CustoIda/_),
    inverso(CaminhoIda,CaminhoAux),
    apagaCabeca(CaminhoAux,CaminhoVolta),
    append(CaminhoIda,CaminhoVolta,Caminho),
    Custo is CustoIda*2.

aestrela_tempo(_,Caminhos,Caminho) :-
    obter_melhor(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).
aestrela_tempo(V,Caminhos,SolucaoCaminho) :-
    obter_melhor(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_aestrela_tempo(V,MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    aestrela_tempo(V,NovoCaminhos,SolucaoCaminho). 

expande_aestrela_tempo(V,Caminho,ExpCaminhos) :-
    findall(NovoCaminho,adjacenteV3(V,Caminho,NovoCaminho),ExpCaminhos).

%-------------------------------------------Pesquisa (Distancia)---------------------------------------------
% Executa um dos algoritmos de pesquisa, dependendo do valor recebido.

% # 1 - DFS
% # 2 - BFS
% # 3 - Limitada em Profundidade
% # 4 - Gulosa
% # 5 - A*

estrategiaProcura(Nodo,Caminho,Distancia,1) :-
    resolveDFS(Nodo,Caminho,Distancia).

estrategiaProcura(Nodo,Caminho,Distancia,2) :-
    resolveBFS(Nodo,Caminho,Distancia).

estrategiaProcura(Nodo,Caminho,Distancia,3) :-
    resolveLimitada(Nodo,Caminho,Distancia,5).

estrategiaProcura(Nodo,Caminho,Distancia,4) :-
    resolveGulosa(Nodo,Caminho/Distancia).

estrategiaProcura(Nodo,Caminho,Distancia,5) :-
    resolveAEstrela(Nodo,Caminho/Distancia).

%-------------------------------------------Pesquisa (Tempo)---------------------------------------------
% Executa um dos algoritmos de pesquisa, dependendo do valor recebido.

% # 1 - DFS
% # 2 - BFS
% # 3 - Limitada em Profundidade
% # 4 - Gulosa
% # 5 - A*

estrategiaProcuraTempo(IdEnc,Caminho,Tempo,1) :-
    resolveDFSTempo(IdEnc,Caminho,Tempo).

estrategiaProcuraTempo(IdEnc,Caminho,Tempo,2) :-
    resolveBFSTempo(IdEnc,Caminho,Tempo).

estrategiaProcuraTempo(IdEnc,Caminho,Tempo,3) :-
    resolveLimitadaTempo(IdEnc,Caminho,Tempo,5).

estrategiaProcuraTempo(IdEnc,Caminho,Tempo,4) :-
    resolveGulosaTempo(IdEnc,Caminho/Tempo).

estrategiaProcuraTempo(IdEnc,Caminho,Tempo,5) :-
    resolveAEstrelaTempo(IdEnc,Caminho/Tempo).

%--------------------------------------Auxiliares Para o Circuito--------------------------------------

% Devolve o meio de transporte mais adequado a uma entrega, tendo em conta o peso da(s) encomenda(s)
meioDeTransporteUsado(PesoTotal,Transporte) :-
    ((PesoTotal =< 5 -> Transporte = 'Bicicleta');
     (PesoTotal > 5, PesoTotal =< 20 -> Transporte = 'Mota');
     (PesoTotal > 20, PesoTotal =< 100 -> Transporte = 'Carro')).

% Devolve a velocidade de uma entrega, consoante o seu transporte e o seu peso
% # Bicicleta - 10 km/h
% # Moto - 35 km/h
% # Carro - 25 km/h
velocidadeEntrega(Transporte,Peso,Velocidade) :-
    ((Transporte == 'Bicicleta' -> Velocidade is 10 - Peso * 0.7);
     (Transporte == 'Mota' -> Velocidade is 35 - Peso * 0.5);
     (Transporte == 'Carro' -> Velocidade is 25 - Peso * 0.1)).

% Devolve o tempo de entrega de uma encomenda, consoante a distância do circuito e a velocidade a que foi entregue
tempoEntregaEncomenda(IdEnc,D,T) :-
    velocidadeEncomenda(IdEnc,Velocidade),
    T is D/Velocidade.

% Devolve a velocidade a que uma encomenda poderá ser entregue, consoante o transporte e o peso
velocidadeEncomenda(IdEnc,Velocidade) :-
    encomenda(IdEnc,_,Peso,_,_),
    meioDeTransporteUsado(Peso,Transporte),
    velocidadeEntrega(Transporte,Peso,Velocidade).

% Devolve o número total de entregas feitas num circuito, juntamente com o peso total carregado
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

% Devolve todos os caminhos possíveis do sistema
allCaminhos(A,L) :- solucoes(Caminho,(caminho(A,B,Caminho),A\=B),R),
                     geraCircuitos(R,L).

%--------------------------------------Auxiliares Funcionalidade 1--------------------------------------

% Devolve o caminho acíclico P do nó A ao nó B 
caminho(A,B,P) :- g(G),caminho1(G,A,B,[B],P).

caminho1(G,A,A,[A|P1],[A|P1]).
caminho1(G,A,B,Hist,P) :-
    adjacente(X,B,_,G),
    nao(membro(X,Hist)), 
    caminho1(G,A,X,[X|Hist],P).

% Devolve todos os caminhos possíveis para chegar a um território
todosOsCaminhosAux(Territorio,[P],L) :- allCaminhosTerritorio('Green Distribuition',P,Territorio,L).
todosOsCaminhosAux(Territorio,[P|T],L) :- 
    allCaminhosTerritorio('Green Distribuition',P,Territorio,R),
    todosOsCaminhosAux(Territorio,T,L1),
    concatena(R,L1,L).

% Dado um caminho de ida, devolve o circuito correspondente (ida + volta)
geraCircuitos([],[]).
geraCircuitos([C],[L]) :- 
    inverso(C,CAux),
    apagaCabeca(CAux,CV),
    append(C,CV,L).
geraCircuitos([C|T],L) :- 
    inverso(C,CAux),
    apagaCabeca(CAux,CV),
    append(C,CV,Caminho),
    geraCircuitos(T,L1),
    adiciona(Caminho,L1,L).

% Devolve todos os caminhos possíveis entre dois pontos que cubram um determinado território
allCaminhosTerritorio(A,B,T,L) :- solucoes(Caminho,(caminho(A,B,Caminho),membro(T,Caminho)),R),
                                  geraCircuitos(R,L).

%--------------------------------------Auxiliares Funcionalidade 2--------------------------------------

% PESO
% Devolve o circuito com menor entregas feitas com um determinado peso
circuitoMaiorNumEntregasPorPesoAux(Peso,MaxE,[C],L) :- numeroEntregasPorPesoCircuito(Peso,C,E), (E == MaxE -> adiciona(C,L1,L) ; L = []).
circuitoMaiorNumEntregasPorPesoAux(Peso,MaxE,[C|T],L) :-
    numeroEntregasPorPesoCircuito(Peso,C,T1),
    circuitoMaiorNumEntregasPorPesoAux(Peso,MaxE,T,L1),
    (T1 == MaxE -> inverso(C,CAux),
    apagaCabeca(CAux,CV),
    append(C,CV,Circuito),adiciona(Circuito,L1,L) ; L = L1).

% Devolve o valor maximo de entregas com um determinado peso feitas num dos circuitos
maiorNumEntregasPorPeso(Peso,[C],Max) :- numeroEntregasPorPesoCircuito(Peso,C,Max).
maiorNumEntregasPorPeso(Peso,[C|T],Max) :-
    numeroEntregasPorPesoCircuito(Peso,C,NumE),
    maiorNumEntregasPorPeso(Peso,T,Max1),
    (NumE > Max1 -> Max = NumE; Max = Max1).

% Devolve o número de entregas feitas num circuito com um determinado peso
numeroEntregasPorPesoCircuito(Peso,[Freg],T) :- numeroEntregasPorPesoFreguesia(Peso,Freg,T).
numeroEntregasPorPesoCircuito(Peso,[Freg|T],Total) :-
    numeroEntregasPorPesoFreguesia(Peso,Freg,T1),
    numeroEntregasPorPesoCircuito(Peso,T,Total1),
    Total is Total1 + T1.

% Devolve o número de entregas com um determinado peso feitas numa freguesia
numeroEntregasPorPesoFreguesia(Peso,Freg,T) :- solucoes(IdEnc,encomenda(IdEnc,_,Peso,_,Freg),L),
                                               comprimento(L,T).

% VOLUME
% Devolve o circuito com menor entregas feitas com um determinado volume
circuitoMaiorNumEntregasPorVolumeAux(Volume,MaxE,[C],L) :- numeroEntregasPorVolumeCircuito(Vol,C,E), (E == MaxE -> adiciona(C,L1,L) ; L = []). 
circuitoMaiorNumEntregasPorVolumeAux(Volume,MaxE,[C|T],L) :-
    numeroEntregasPorVolumeCircuito(Vol,C,T1),
    circuitoMaiorNumEntregasPorVolumeAux(Vol,MaxE,T,L1),
    (T1 == MaxE -> inverso(C,CAux),
    apagaCabeca(CAux,CV),
    append(C,CV,Circuito),adiciona(Circuito,L1,L) ; L = L1).

% Devolve o valor maximo de entregas com um determinado volume feitas num dos circuitos
maiorNumEntregasPorVolume(Vol,[C],Max) :- numeroEntregasPorVolumeCircuito(Vol,C,Max).
maiorNumEntregasPorVolume(Vol,[C|T],Max) :-
    numeroEntregasPorVolumeCircuito(Vol,C,NumE),
    maiorNumEntregasPorVolume(Vol,T,Max1),
    (NumE > Max1 -> Max = NumE; Max = Max1).

% Devolve o número de entregas feitas num circuito com um determinado volume
numeroEntregasPorVolumeCircuito(Vol,[Freg],T) :- numeroEntregasPorVolumeFreguesia(Vol,Freg,T).
numeroEntregasPorVolumeCircuito(Vol,[Freg|T],Total) :-
    numeroEntregasPorVolumeFreguesia(Vol,Freg,T1),
    numeroEntregasPorVolumeCircuito(Vol,T,Total1),
    Total is Total1 + T1.

% Devolve o número de entregas com um determinado volume feitas numa freguesia
numeroEntregasPorVolumeFreguesia(Vol,Freg,T) :- solucoes(IdEnc,encomenda(IdEnc,_,_,Vol,Freg),L),
                                                comprimento(L,T).

%--------------------------------------Auxiliares Funcionalidade 4--------------------------------------

% Devolve o circuito mais rápido, conforme o algoritmo escolhido
circuitoMaisRapidoAux(Territorio,Caminho,Distancia,Alg) :-
      findall((C,D),estrategiaProcura(Territorio,C,D,Alg),Caminhos),
      circuitoMaisRapidoAux1(Caminhos,Distancia,Caminho).

% Devolve o circuito mais rápido de acordo com o critério distância 
circuitoMaisRapidoAux1([],0,_).
circuitoMaisRapidoAux1([(C,D)],D,C).
circuitoMaisRapidoAux1([(C,D)|T],Min,Circuito) :- 
    circuitoMaisRapidoAux1(T,D2,C1),
    ((D < D2 -> Min = D, Circuito = C);
     Min = D2, Circuito = C1).

%--------------------------------------Auxiliares Funcionalidade 5--------------------------------------

% Devolve o circuito mais eficiente, conforme o algoritmo escolhido
circuitoMaisEficienteAux(IdEnc,Caminho,Tempo,Alg) :-
      findall((C,T),estrategiaProcuraTempo(IdEnc,C,T,Alg),Caminhos),
      circuitoMaisEficienteAux1(Caminhos,Tempo,Caminho).

% Devolve o circuito mais eficiente de acordo com o critério tempo 
circuitoMaisEficienteAux1([],0,_).
circuitoMaisEficienteAux1([(C,T)],T,C).
circuitoMaisEficienteAux1([(C,T)|Cs],Min,Circuito) :-
    circuitoMaisEficienteAux1(Cs,T2,C1),
    ((T < T2 -> Min = T, Circuito = C);
     Min = T2, Circuito = C1).

%--------------------------------------Auxiliares Funcionalidade Extra--------------------------------------

% Devolve os circuitos com maior número de entregas associadas
circuitosMaiorNumEntregasAux([C],MaxE,L) :- numEntregasCircuito(C,_,NumE), (NumE == MaxE -> adiciona(C,L1,L) ; L = []).
circuitosMaiorNumEntregasAux([C|T],MaxE,L) :- 
    numEntregasCircuito(C,_,NumE),
    circuitosMaiorNumEntregasAux(T,MaxE,L1),
    (NumE == MaxE -> inverso(C,CAux),
    apagaCabeca(CAux,CV),
    append(C,CV,Circuito),adiciona(Circuito,L1,L) ; L = L1).

% Devolve o valor maximo de entregas feitas num dos circuitos
maiorNumEntregasCircuito([C],Max) :- numEntregasCircuito(C,_,Max).
maiorNumEntregasCircuito([C|T],Max) :-
    numEntregasCircuito(C,_,NumE),
    maiorNumEntregasCircuito(T,Max1),
    (NumE > Max1 -> Max = NumE; Max = Max1).

%---------------------------------------------------Anexos---------------------------------------------------

adjacente(Nodo,ProxNodo,C,grafo(_,Es)) :- membro(aresta(Nodo,ProxNodo,C),Es).
adjacente(Nodo,ProxNodo,C,grafo(_,Es)) :- membro(aresta(ProxNodo,Nodo,C),Es).

adjacenteV2([Nodo|Caminho]/Custo1/_,[ProxNodo,Nodo|Caminho]/Custo2/Estima) :-
    g(G),adjacente(Nodo,ProxNodo,PassoCusto,G),
	nao(membro(ProxNodo,Caminho)),
	Custo2 is Custo1 + PassoCusto,
	estima(ProxNodo,Estima).

adjacenteV3(V,[Nodo|Caminho]/Custo1/_,[ProxNodo,Nodo|Caminho]/Custo2/Estima) :-
    g(G),adjacente(Nodo,ProxNodo,Dist,G),
    PassoTempo is Dist/V,
    nao(membro(ProxNodo,Caminho)),
    Custo2 is Custo1 + PassoTempo,
    estima(ProxNodo,EstimaDist),
    Estima is EstimaDist/V.

obter_melhor([Caminho],Caminho) :- !.
obter_melhor([Caminho1/Custo1/Estima1,_/Custo2/Estima2|Caminhos],MelhorCaminho) :-
    Estima1 =< Estima2, !,
    obter_melhor([Caminho1/Custo1/Estima1|Caminhos],MelhorCaminho).
obter_melhor([_|Caminhos],MelhorCaminho) :-
    obter_melhor(Caminhos,MelhorCaminho).

estimaTempoEnc(IdEnc,ProxNodo,TE) :-
    estima(ProxNodo,Estima),
    tempoEntregaEncomenda(IdEnc,Estima,TE).

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