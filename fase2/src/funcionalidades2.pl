:- consult('baseConhecimento.pl').
:- consult('auxiliares2.pl').
:- consult('invariantes.pl').
:- consult('evolucaoInvolucao.pl').

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).
:- set_prolog_flag(global, 10 000 000 000).

:- style_check(-singleton).

/*
%---------------------------------------Circuito de Entrega--------------------------------------

circuito(PontoEntrega,Distancia,Quantidade,Tempo,5) :-
    resolveAEstrela(PontoEntrega,Caminho/Distancia),
    velocidadeEntrega(IdEnc,Velocidade),
    Tempo is Distancia/Velocidade.
*/
%----------------------------------------Funcionalidade 1----------------------------------------
% Gerar os circuitos de entrega, caso existam, que cubram um determinado território. 

todosOsCaminhosTerritorio(Territorio,L) :- 
    solucoes(Ponto,pontoEntrega(Ponto),Pts),
    todosOsCaminhosAux(Territorio,Pts,L).

%----------------------------------------Funcionalidade 2----------------------------------------
% Identificar quais os circuitos com maior número de entregas.

circuitosMaiorNumEntregas(L) :- 
    allCaminhos('Green Distribuition',R),
    maiorNumEntregasCircuito(R,MaxE),
    circuitosMaiorNumEntregasAux(R,MaxE,L).

%----------------------------------------Funcionalidade 3----------------------------------------
% Comparar circuitos de entrega tendo em conta os indicadores de produtividade (distância e quantidade).

% # 1 - DFS
% # 2 - BFS
% # 3 - Limitada em Profundidade
% # 4 - Gulosa
% # 5 - A*
/* FALTA O INDICADOR DE PRODUTIVIDADE - QUANTIDADE !!! POR ISSO, AINDA NÃO FUNCIONAL */
produtividade(Nodo,Distancia,Quantidade,1) :- resolveDFS(Nodo,[Nodo|Caminho],Distancia).

produtividade(Nodo,Distancia,Quantidade,2) :- resolveBFS(Nodo,[Nodo|Caminho],Distancia).

produtividade(Nodo,Distancia,Quantidade,4) :- resolveGulosa(Nodo,Caminho/Distancia),
											  calculaQuantidade(Caminho,Quantidade),!.

produtividade(Nodo,Distancia,Quantidade,5) :- resolveAEstrela(Nodo,Caminho/Distancia),
											  calculaQuantidade(Caminho,Quantidade),!.

%----------------------------------------Funcionalidade 4----------------------------------------
% Escolher o circuito mais rápido (usando o critério da distância).



%----------------------------------------Funcionalidade 5----------------------------------------
% Escolher o circuito mais ecológico (usando critério de tempo).

/* USAR OS PREDICADOS tempoEntrega IMPLEMENTADOS EM AUXILIARES2.PL */