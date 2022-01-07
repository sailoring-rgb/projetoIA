% MENU

:- consult('funcionalidades1.pl').
:- consult('funcionalidades2.pl').

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- style_check(-singleton).

%-----------------------------------------------------------------------------------------------------------------------------------------

main :-
    repeat,
    nl,
    write('-------------MENU-------------'), nl,
    write('1. Funcionalidades - Fase 1'),nl,
    write('2. Funcionalidades - Fase 2'),nl,
    write('0. Sair'), nl,
    write('------------------------------'), nl,nl,
    write('Escolha um: '),nl,
    read(X),
    (X = 0 -> !, fail; true),
    nl,
    fase(X),fail.

fase(1) :-
    repeat,
    nl,
    write('---------------------------------------------------------FASE1----------------------------------------------------------'),nl,
    write('1. Consultar o estafeta que utilizou mais vezes um meio de transporte mais ecológico.'),nl,
    write('2. Consultar os estafetas que entregaram determinada(s) encomenda(s) a um determinado cliente.'),nl,
    write('3. Consultar os clientes servidos por um determinado estafeta.'),nl,
    write('4. Calcular o valor faturado pela Green Distribution num determinado dia.'),nl,
    write('5. Consultar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution.'),nl,
    write('6. Calcular a classificação média de satisfação de cliente para um determinado estafeta.'),nl,
    write('7. Consultar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo.'),nl,
    write('8. Consultar o número total de entregas pelos estafetas, num determinado intervalo de tempo.'),nl,
    write('9. Calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo.'),nl,
    write('10. Calcular o peso total transportado por estafeta num determinado dia.'),nl,
    write('11. Consultar o cliente que fez mais encomendas.'),nl,
    write('12. Consultar os estefetas menos pontuais a fazer as suas entregas.'),nl,
    write('0. Sair'),nl,
    write('-----------------------------------------------------------------------------------------------------------------------'),nl,nl,
    write('Escolha um: '),nl,
    read(Y),
    (Y = 0 -> !, fail; true),
    nl,
    funcionalidade1(Y),fail.

fase(2) :-
    repeat,
    nl,
    write('---------------------------------------------------------FASE2----------------------------------------------------------'),nl,
    write('1. Gerar os circuitos de entrega, caso existam, que cubram um determinado território.'),nl,
    write('2. Representar os diversos pontos de entrega (freguesias) disponíveis em forma de grafo.'),nl,
    write('3. Identificar quais os circuitos com maior número de entregas (por volume e peso).'),nl,
    write('4. Comparar circuitos de entrega tendo em conta os indicadores de produtividade.'),nl,
    write('5. Escolher o circuito mais rápido para entregar uma dada encomenda, utilizado um algoritmo de pesquisa.'),nl,
    write('6. Escolher o circuito mais ecológico para entregar uma dada encomenda, utilizado um algoritmo de pesquisa.'),nl,
    write('7. Identificar quais os circuitos com maior número de entregas.'),nl,
    write('0. Sair'),nl,
    write('-----------------------------------------------------------------------------------------------------------------------'),nl,nl,
    write('Escolha um: '),nl,
    read(Z),
    (Z = 0 -> !, fail; true),
    nl,
    funcionalidade2(Z),fail.

%----------------------------------------------------------------FASE1----------------------------------------------------------------

% # FUNCIONALIDADES DA FASE 1 

funcionalidade1(1) :-
    estafetaMaisEcologico(IDs),
    write('ID(s) do(s) estafeta(s) mais ecológicos: '),write(IDs),!,nl.

funcionalidade1(2) :-
    write('Indique os IDs de encomendas, separados por vírgulas: '),!,nl,
    read(Input),
    format(atom(A), "~w", Input),
    atomic_list_concat(N,,,A),
    convertAtomsNumbers(N,L),
    estafetasEncomendasCliente(L,R),nl,
    write('ID(s) do(s) estafeta(s) que entregaram as referidas encomendas: '),write(R),!,nl.

funcionalidade1(3) :-
    write('Indique o ID do estafeta: '),!,nl,
    read(ID),
    clientesPorEstafeta(ID,L),nl,
    write('ID(s) do(s) cliente(s) servidos pelo estafeta '),write(ID),write(': '),write(L),!,nl.

funcionalidade1(4) :-
    write('Indique uma data no formato - data(Ano,Mês,Dia): '),!,nl,
    read(Input),nl,
    ((dataValida(Input),
    elementosData(Input,A,M,D),
    valorFaturadoDia(A,M,D,V),nl,
    write('Valor faturado em '),write(D),write('/'),write(M),write('/'),write(A),write(': '),write(V),write('€'),nl);
    (nao(dataValida(Input)),write('A data que inseriu não é válida.'),nl)),!.

funcionalidade1(5) :-
    freguesiasMaisFrequentes(L),
    write('Zona(s) com maior volume de entregas: '),write(L),!,nl.

funcionalidade1(6) :-
    write('Indique o ID do estafeta: '),!,nl,
    read(ID),
    mediaSatisfacaoEstafeta(ID,V),nl,
    write('Classificação média de satisfação de cliente: '),write(V),!,nl.
    
funcionalidade1(7) :-
    write('Indique a data inicial no formato - data(Ano,Mês,Dia,Horas,Minutos): '),!,nl,
    read(Input1),
        ((nao(dataTimeValida(Input1)),nl,write('A data que inseriu não é válida.'),nl);
        elementosDataTime(Input1,A1,M1,D1,H1,Min1),nl,
        write('Indique a data final no formato - data(Ano,Mês,Dia,Horas,Minutos): '),!,nl,
        read(Input2),
            ((nao(dataTimeValida(Input2)) -> nl,write('A data que inseriu não é válida.'),nl);
            elementosDataTime(Input2,A2,M2,D2,H2,Min2),
            numeroTotalEntregasTransporte(data(A1,M1,D1,H1,Min1),data(A2,M2,D2,H2,Min2),V1,V2,V3),nl,
            write('Número total de entregas transportadas de carro: '),write(V1),nl,
            write('Número total de entregas transportadas de mota: '),write(V2),nl,
            write('Número total de entregas transportadas de bicicleta: '),write(V3),nl)),!.

funcionalidade1(8) :-
    write('Indique a data inicial no formato - data(Ano,Mês,Dia,Horas,Minutos): '),!,nl,
    read(Input1),
        ((nao(dataTimeValida(Input1)) -> nl,write('A data que inseriu não é válida.'),nl);
        elementosDataTime(Input1,A1,M1,D1,H1,Min1),nl,
        write('Indique a data final no formato - data(Ano,Mês,Dia,Horas,Minutos): '),!,nl,
        read(Input2),
            ((nao(dataTimeValida(Input2)) -> nl,write('A data que inseriu não é válida.'),nl);
            elementosDataTime(Input2,A2,M2,D2,H2,Min2),
            totalEntregasEstafetas(data(A1,M1,D1,H1,Min1),data(A2,M2,D2,H2,Min2),L),nl,
            write('Número total de entregas pelos estafetas: '),write(L),nl)),!.

funcionalidade1(9) :-
    write('Indique a data inicial no formato - data(Ano,Mês,Dia,Horas,Minutos): '),!,nl,
    read(Input1),
        ((nao(dataTimeValida(Input1)) -> nl,write('A data que inseriu não é válida.'),nl);
        elementosDataTime(Input1,A1,M1,D1,H1,Min1),nl,
        write('Indique a data final no formato - data(Ano,Mês,Dia,Horas,Minutos): '),!,nl,
        read(Input2),
            ((nao(dataTimeValida(Input2)) -> nl,write('A data que inseriu não é válida.'),nl);
            elementosDataTime(Input2,A2,M2,D2,H2,Min2),
            numEntregasNaoEntregas(data(A1,M1,D1,H1,Min1),data(A2,M2,D2,H2,Min2),V1,V2,V3),nl,
            write('Número total de encomendas entregues: '),write(V1),nl,
            write('Número total de encomendas não entregues naquele período de tempo: '),write(V2),nl,
            write('Número total de encomendas nunca entregues: '),write(V3),nl)),!.

funcionalidade1(10) :-
    write('Indique uma data no formato - data(Ano,Mês,Dia): '),!,nl,
    read(Input),nl,
    ((dataValida(Input),
    elementosData(Input,A,M,D),
    pesoTotalEstafetasDia(A,M,D,L),nl,
    write('Peso total transportado por estafeta em '),
    write(D),write('/'),write(M),write('/'),write(A),write(': '),write(L),nl);
    (nao(dataValida(Input)),write('A data que inseriu não é válida.'),nl)),!.

% EXTRA Funcionalidades

funcionalidade1(11) :-
    clienteMaisEncomendas(IDs),
    write('ID(s) do(s) cliente(s) com mais encomendas: '),write(IDs),!,nl.

funcionalidade1(12) :-
    estafetasMenosPontuais(L),
    write('IDs dos estafetas menos pontuais: '),write(L),!,nl.


%----------------------------------------------------------------FASE2----------------------------------------------------------------

funcionalidade2(1) :-
    write('Indique o território a consultar: '),nl,
    read(Input),nl,
    ((pontoEntrega(Input) -> todosOsCaminhosTerritorio(Input,L),write(L),!,nl);
     (nao(pontoEntrega(Input)) -> write('Não existem informações sobre este território.'),!,nl)).

funcionalidade2(2) :-
    write('Representação dos pontos de entrega em forma de grafo:'),nl,nl,g(G),write(G),!,nl.

funcionalidade2(3) :-
    write('Peso(0) ou Volume(1)?: '),nl,
    read(Input1),nl,
    write('Insira o peso/volume: '),nl,
    read(Input2),nl,
    circuitoMaiorNumEntregasPorPesoEVolume(Input1,Input2,L,Encs),
    write('Número de entregas: '), write(Encs),!,nl,
    write('Circuito(s) com maior número de entregas: '),nl,nl,write(L),!,nl.

funcionalidade2(4) :-
    write('Indique o ID da encomenda a entregar: '),nl,
    read(Input),nl,
    write('Profundidade (DFS):'),nl,produtividade(Input,D1,T1,1),
        write('*** Distância: '),write(D1),write(' km'),!,nl,
        write('*** Tempo: '),write(T1),write(' horas'),!,nl,nl,
    write('Largura (BFS):'),nl,produtividade(Input,D2,T2,2),
        write('*** Distância: '),write(D2),write(' km'),!,nl,
        write('*** Tempo: '),write(T2),write(' horas'),!,nl,nl,
    write('Limitada em Profundidade:'),nl,produtividade(Input,D3,T3,3),
        write('*** Distância: '),write(D3),write(' km'),!,nl,
        write('*** Tempo: '),write(T3),write(' horas'),!,nl,nl,
    write('Gulosa (Greedy):'),nl,produtividade(Input,D4,T4,4),
        write('*** Distância: '),write(D4),write(' km'),!,nl,
        write('*** Tempo: '),write(T4),write(' horas'),!,nl,nl,
    write('A Estrela (A*):'),nl,produtividade(Input,D5,T5,5),
        write('*** Distância: '),write(D5),write(' km'),!,nl,
        write('*** Tempo: '),write(T5),write(' horas'),!,nl,nl.

funcionalidade2(5) :-
    write('Indique o ID da encomenda: '),nl,
    read(Input1),nl,
    write('---------Algoritmos de Pesquisa---------'),nl,
    write('1. Profundidade (DFS)'),nl,
    write('2. Largura (BFS)'),nl,
    write('3. Limitada em Profundidade'),nl,
    write('4. Gulosa (Greedy)'),nl,
    write('5. A Estrela (A*)'),nl,
    write('----------------------------------------'),nl,nl,
    write('Escolha um: '),nl,
    read(Input2),
    circuitoMaisRapido(Input1,Input2,C,D),nl,
    write('Caminho mais rápido: '),write(C),!,nl,
    write('*** Distância: '),write(D),write(' km'),!,nl,nl.

funcionalidade2(6) :-
    write('Indique o ID da encomenda: '),nl,
    read(Input1),nl,
    write('---------Algoritmos de Pesquisa---------'),nl,
    write('1. Profundidade (DFS)'),nl,
    write('2. Largura (BFS)'),nl,
    write('3. Limitada em Profundidade'),nl,
    write('4. Gulosa (Greedy)'),nl,
    write('5. A Estrela (A*)'),nl,
    write('----------------------------------------'),nl,nl,
    write('Escolha um: '),nl,
    read(Input2),
    circuitoMaisEficiente(Input1,Input2,C,T),nl,
    write('Caminho mais eficiente: '),write(C),!,nl,
    write('*** Tempo: '),write(T),write(' horas'),!,nl,nl.

funcionalidade2(7) :-
    circuitosMaiorNumEntregas(L),
    write('Circuito(s) com maior número de entregas: '),nl,nl,write(L),!,nl.
