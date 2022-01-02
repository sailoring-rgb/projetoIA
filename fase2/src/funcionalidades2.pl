:- consult('baseConhecimento2.pl').
:- consult('auxiliares2.pl').

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).
:- set_prolog_flag(global, 10 000 000 000).

:- style_check(-singleton).

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
% Comparar circuitos de entrega tendo em conta os indicadores de produtividade (distância e tempo).

% # 1 - DFS
% # 2 - BFS
% # 3 - Limitada em Profundidade
% # 4 - Gulosa
% # 5 - A*

produtividade(IdEnc,Distancia,Tempo,1) :-
    encomenda(IdEnc,_,_,_,Freg),
    estrategiaProcura(Nodo,Caminho,Distancia,1),
    estrategiaProcuraTempo(IdEnc,Caminho,Tempo,1).

produtividade(IdEnc,Distancia,Tempo,2) :- 
    encomenda(IdEnc,_,_,_,Freg),
    estrategiaProcura(Freg,Caminho,Distancia,2),
    estrategiaProcuraTempo(IdEnc,Caminho,Tempo,2).

produtividade(IdEnc,Distancia,Tempo,3) :- 
    encomenda(IdEnc,_,_,_,Freg),
    estrategiaProcura(Freg,Caminho,Distancia,3),
    estrategiaProcuraTempo(IdEnc,Caminho,Tempo,3).

produtividade(IdEnc,Distancia,Tempo,4) :-
    encomenda(IdEnc,_,_,_,Freg),
    estrategiaProcura(Freg,Caminho,Distancia,4),
    estrategiaProcuraTempo(IdEnc,Caminho,Tempo,4).

produtividade(IdEnc,Distancia,Tempo,5) :-
    encomenda(IdEnc,_,_,_,Freg),
    estrategiaProcura(Freg,Caminho,Distancia,5),
    estrategiaProcuraTempo(IdEnc,Caminho,Tempo,5).

%----------------------------------------Funcionalidade 4----------------------------------------
% Escolher o circuito mais rápido (usando o critério da distância).

circuitoMaisRapido(IdEnc,Alg,C,D) :-
    encomenda(IdEnc,_,_,_,Freg),
    circuitoMaisRapidoAux(Freg,C,D,Alg).

%----------------------------------------Funcionalidade 5----------------------------------------
% Escolher o circuito mais ecológico (usando critério de tempo).

circuitoMaisEficiente(IdEnc,Alg,C,T) :-
    circuitoMaisEficienteAux(IdEnc,C,T,Alg).

%------------------------------------------Estatísticas------------------------------------------
% Analisar comparativamente as diferentes estratégias de procura.

% # 1 - DFS
% # 2 - BFS
% # 3 - Limitada em Profundidade
% # 4 - Gulosa
% # 5 - A*

obterEstatisticas(1) :-
    statistics(global_stack,[M1,L1]),
    time(resolveDFS('Lamas',Caminho,Distancia)),
    statistics(global_stack,[M2,L2]),
    Mem is M2 - M1,
    write("Memória usada: "),write(Mem),nl,
	write("Custo: "),write(Distancia).

obterEstatisticas(2) :-
    statistics(global_stack,[M1,L1]),
    time(resolveBFS('Lamas',Caminho,Distancia)),
    statistics(global_stack,[M2,L2]),
    Mem is M2 - M1,
    write("Memória usada: "),write(Mem),nl,
	write("Custo: "),write(Distancia).

obterEstatisticas(3) :-
    statistics(global_stack, [M1,L1]),
    time(resolveLimitada('Lamas',Caminho,Distancia,5)),
    statistics(global_stack, [M2,L2]),
    Mem is M2 - M1,
    write("Memória usada: "),write(Mem),nl,
	write("Custo: "),write(Distancia).

obterEstatisticas(4) :-
    statistics(global_stack, [M1,L1]),
    time(resolveGulosa('Lamas',C/Distancia)),
    statistics(global_stack, [M2,L2]),
    Mem is M2 - M1,
    write("Memória usada: "),write(Mem),nl,
	write("Custo: "),write(Distancia).

obterEstatisticas(5) :-
    statistics(global_stack, [M1,L1]),
    time(resolveAEstrela('Lamas',C/Distancia)),
    statistics(global_stack, [M2,L2]),
    Mem is M2 - M1,
    write("Memória usada: "),write(Mem),nl,
	write("Custo: "),write(Distancia).