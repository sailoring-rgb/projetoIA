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

/* USAR OS PREDICADOS tempoEntrega IMPLEMENTADOS EM AUXILIARES2.PL */