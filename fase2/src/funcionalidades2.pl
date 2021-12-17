:- consult('baseConhecimento.pl').
:- consult('auxiliares2.pl').
:- consult('invariantes.pl').
:- consult('evolucaoInvolucao.pl').

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).
:- set_prolog_flag(global, 10 000 000 000).

:- style_check(-singleton).

%-------------------------------Pesquisa em Profundidade (DFS)-------------------------------

/*
                FUNCIONAL MAS TALVEZ FALTE:
Se o estafeta realizar mais do que uma entrega num percurso,
tem-se de ter em atenção o transporte que ele usa e a quantidade de peso que ele transporta.
*/
resolveDFS(Nodo,[Nodo|Caminho],Distancia) :-
    g(Grafo),
    profundidade(Grafo,Nodo,[Nodo],Caminho,Distancia).

profundidade(Grafo,Nodo,_,[],0) :- goal(Nodo).
profundidade(Grafo,Nodo,Historico,[ProxNodo|Caminho],DistanciaT) :-
    adjacente(Nodo,ProxNodo,Distancia1,Grafo),
    nao(membro(ProxNodo,Historico)),
    profundidade(Grafo,ProxNodo,[ProxNodo|Historico],Caminho,Distancia2),
    DistanciaT is Distancia1 + Distancia2.

%---------------------------------Pesquisa em Largura (BFS)---------------------------------

/*
                FUNCIONAL MAS TALVEZ FALTE:
Se o estafeta realizar mais do que uma entrega num percurso,
tem-se de ter em atenção o transporte que ele usa e a quantidade de peso que ele transporta.
*/
resolveBFS(Nodo,Caminho,Distancia) :-
    goal(NodoFinal),
    g(Grafo),largura(Grafo,NodoFinal,[[Nodo]],Caminho,Distancia).

largura(Grafo,NodoFinal,[[NodoFinal|T]|_],Caminho,0) :- reverse([NodoFinal|T],Caminho).
largura(Grafo,NodoFinal,[Lista|Outros],Caminho,DistanciaT) :-
    Lista = [A|_],
    findall([X|Lista],(NodoFinal \== A, adjacente(A,X,_,Grafo),nao(membro(X,Lista))),Novos),
    adjacente(A,X,Distancia1,Grafo),
    concatena(Outros,Novos,Todos),
    largura(Grafo,NodoFinal,Todos,Caminho,Distancia2),
    DistanciaT is Distancia1 + Distancia2.

%------------------------------Pesquisa em Profundidade Limitada------------------------------

/*
                FUNCIONAL MAS TALVEZ FALTE:
Se o estafeta realizar mais do que uma entrega num percurso,
tem-se de ter em atenção o transporte que ele usa e a quantidade de peso que ele transporta.
*/
% # Limite - Número limite de nós a procurar.
resolveLimitada(Nodo,Caminho,Distancia,Limite) :-
    g(Grafo),
    profundidadeLimitada(Grafo,Nodo,[Nodo],Caminho,Distancia,Limite).

profundidadeLimitada(Grafo,Nodo,_,[],0,_) :- goal(Nodo).
profundidadeLimitada(Grafo,Nodo,Historico,[ProxNodo|Caminho],DistanciaT,Limite) :-
    Limite > 0,
    adjacente(Nodo,ProxNodo,Distancia1,Grafo),
    nao(membro(ProxNodo,Historico)),
    Limite1 is Limite-1,
    profundidadeLimitada(Grafo,ProxNodo,[ProxNodo|Historico],Caminho,Distancia2,Limite1),
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

expande_gulosa(Caminho,Expandidos) :- findall(NovoCaminho,adjacenteV2(Caminho,NovoCaminho), ExpCaminhos).

%--------------------------------------Pesquisa A Estrela--------------------------------------

resolveAEstrela(Nodo, Caminho/Custo) :-
    estima(Nodo, Estima),
    aestrela([[Nodo]/0/Estima], InvCaminho/Custo/_),
    inverso(InvCaminho, Caminho).

aestrela(Caminhos, Caminho) :-
    obtem_melhor(Caminhos, Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).

aestrela(Caminhos, SolucaoCaminho) :-
    obtem_melhor(Caminhos, MelhorCaminho),
    seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
    expande_aestrela(MelhorCaminho, ExpCaminhos),
    append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrela(NovoCaminhos, SolucaoCaminho). 

expande_aestrela(Caminho, ExpCaminhos) :-
    findall(NovoCaminho, adjacenteV2(Caminho,NovoCaminho), ExpCaminhos).