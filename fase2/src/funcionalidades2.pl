:- consult('baseConhecimento.pl').
:- consult('auxiliares2.pl').
:- consult('invariantes.pl').
:- consult('evolucaoInvolucao.pl').

:- set_prolog_stack(global, limit(100000000000)).

%-------------------------------Pesquisa em Profundidade (DFS)-------------------------------

/*
Se o estafeta realizar mais do que uma entrega num percurso,
tem-se de ter em atenção o transporte que ele usa e a quantidade de peso que ele transporta.
*/
resolve_DFS(Nodo,[Nodo|Caminho],Distancia) :-
    g(Grafo),
    profundidade(Grafo,Nodo,[Nodo],Caminho,Distancia).

profundidade(Grafo,Nodo,_,[],0) :- goal(Nodo).
profundidade(Grafo,Nodo,Historico,[ProxNodo|Caminho],DistanciaT) :-
    adjacente(Nodo,ProxNodo,Distancia1,Grafo),
    nao(membro(ProxNodo,Historico)),
    profundidade(ProxNodo,[ProxNodo|Historico],Caminho,Distancia2),
    DistanciaT is Distancia1 + Distancia2.

statistics_DFS() :-
    statistics(global_stack, [G1,L1]),
    time(resolve_DFS('Dume',_,D)),
    statistics(global_stack, [G2,L2]),
    Res is G2 - G1,
    write("Memory: "), 
    write(Res),write("\n"),
	write("Custo: "),write(D).