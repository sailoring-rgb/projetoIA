:- consult('baseConhecimento.pl').
:- consult('auxiliares2.pl').
:- consult('invariantes.pl').
:- consult('evolucaoInvolucao.pl').

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).
:- set_prolog_flag(global, 10 000 000 000).

:- style_check(-singleton).

%------------------------------------Funcionalidade 1------------------------------------
% Gerar os circuitos de entrega, caso existam, que cubram um determinado território. 

todosOsCaminhosTerritorio(Territorio,L) :- 
    solucoes(Ponto,pontoEntrega(P),Pts),
    todosOsCaminhosAux(Territorio,Pts,L).
    
%------------------------------------Funcionalidade 2------------------------------------
% Identificar quais os circuitos com maior número de entregas.

/* ??????????????????????????????????????????????????????????????????????????????????? */

%------------------------------------Funcionalidade 3------------------------------------
% Comparar circuitos de entrega tendo em conta os indicadores de produtividade (distância e quantidade).

% 1 - DFS
% 2 - BFS
% 3 - Gulosa
% 4 - A*

/* FALTA O INDICADOR DE PRODUTIVIDADE - QUANTIDADE !!! POR ISSO, AINDA NÃO FUNCIONAL */
produtividade(Nodo,Distancia,Quantidade,1) :- resolveDFS(Nodo,[Nodo|Caminho],Distancia).

produtividade(Nodo,Distancia,Quantidade,2) :- resolveBFS(Nodo,[Nodo|Caminho],Distancia).

produtividade(Nodo,Distancia,Quantidade,3) :- resolveGulosa(Nodo,Caminho/Distancia),
											  calculaQuantidade(Caminho,Quantidade),!.

produtividade(Nodo,Distancia,Quantidade,4) :- resolveAEstrela(Nodo,Caminho/Distancia),
											  calculaQuantidade(Caminho,Quantidade),!.

%------------------------------------Funcionalidade 4------------------------------------
% Escolher o circuito mais rápido (usando o critério da distância).

/* ??????????????????????????????????????????????????????????????????????????????????? */

%------------------------------------Funcionalidade 5------------------------------------
% Escolher o circuito mais ecológico (usando critério de tempo).

aestrela_tempo(Caminhos, Caminho) :-
    obtem_melhor_tempo(Caminhos, Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).

aestrela_tempo(Caminhos, SolucaoCaminho) :-
    obtem_melhor_tempo(Caminhos, MelhorCaminho),
    seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
    expande_aestrela_tempo(MelhorCaminho, ExpCaminhos),
    append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
        aestrela_tempo(NovoCaminhos, SolucaoCaminho).
    
obtem_melhor_tempo([Caminho], Caminho) :- !.
obtem_melhor_tempo([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
    Custo1 + Est1 =< Custo2 + Est2, !,
    obtem_melhor_tempo([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho). 
obtem_melhor_tempo([_|Caminhos], MelhorCaminho) :- 
    obtem_melhor_tempo(Caminhos, MelhorCaminho).
    
expande_aestrela_tempo(Caminho, ExpCaminhos) :-
    findall(NovoCaminho, adjacente_tempo(Caminho,NovoCaminho), ExpCaminhos).


/* USAR OS PREDICADOS tempoEntrega IMPLEMENTADOS EM AUXILIARES2.PL */