:- consult('baseConhecimento.pl').
:- consult('auxiliares1.pl').
:- consult('invariantes.pl').
:- consult('evolucaoInvolucao.pl').

%-------------------------------Pesquisa em Profundidade (DFS)-------------------------------

/*
Se o estafeta realizar mais do que uma entrega num percurso,
tem-se de ter em atenção o transporte que ele usa e a quantidade de peso que ele transporta.
*/
resolve_DFS(Nodo,[Nodo|Caminho],Distancia) :-
    profundidade(Grafo,Nodo,[Nodo],Caminho,Distancia).

profundidade(Grafo,Nodo,_,[],0) :- goal(Nodo).
profundidade(Grafo,Nodo,Historico,[ProxNodo|Caminho],DistanciaT) :-
    adjacente(Nodo,ProxNodo,Distancia1,Grafo),
    nao(membro(ProxNodo,Historico)),
    profundidade(ProxNodo,[ProxNodo|Historico],Caminho,Distancia2),
    DistanciaT is Distancia1 + Distancia2.