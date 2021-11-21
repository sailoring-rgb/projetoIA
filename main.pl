%MENU

:- consult('funcionalidades.pl').
:- consult('aux.pl').

main :-
    nl,
    write('---------------------------------------------------------MENU----------------------------------------------------------'), nl,
    write('1. Consultar o estafeta que utilizou mais vezes um meio de transporte mais ecológico.'), nl,
    write('2. Consultar que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente.'), nl,
    write('3. Consultar os clientes servidos por um determinado estafeta.'), nl,
    write('4. Calcular o valor faturado pela Green Distribution num determinado dia.'), nl,
    write('5. Consultar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution.'), nl,
    write('6. Calcular a classificação média de satisfação de cliente para um determinado estafeta.'), nl,
    write('7. Consultar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo.'), nl,
    write('8. Consultar o número total de entregas pelos estafetas, num determinado intervalo de tempo.'), nl,
    write('9. Calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo.'), nl,
    write('10. Calcular o peso total transportado por estafeta num determinado dia.'), nl,
    write('0. Exit'), nl,
    write('-----------------------------------------------------------------------------------------------------------------------'), nl,nl,
    write('Escolha um: '),
    read(X),
    ( X = 0 -> !, fail ; true ),
    nl,
    funcionalidade(X),
    fail.

%Funcionalidades

funcionalidade(1) :-
    estafetaMaisEcologico(ID),
    write(ID), nl,
    main.

funcionalidade(2) :-
    write('Indique o ID do cliente: '),!,
    read(ID),
    estafetasEncomendasCliente(ID,L),nl,
    write(L),nl,
    main.

funcionalidade(3) :-
    write('Indique o ID do estafeta: '),!,
    read(ID),
    clientesPorEstafeta(ID,L),nl,
    write(L),nl,
    main.

funcionalidade(4) :-
    write('Indique uma data no formato AAAA-MM-DD: '),!,
    stringParaDateTime(Date,A,M,D),
    valorFaturadoDia(A,M,D,V),nl,
    write(V),nl,
    main.

funcionalidade(5) :-
    freguesiasComMaisEnc(L),
    write(L),nl,
    main.

% funcionalidade(6) :-

% funcionalidade(7) :-

% funcionalidade(8) :-

% funcionalidade(9) :-

% funcionalidade(10) :-

%EXTRA Funcionalidades
