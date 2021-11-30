% MENU

:- consult('funcionalidades.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%-----------------------------------------------------------------------------------------------------------------------------------------

main :-
    repeat,
    nl,
    write('---------------------------------------------------------MENU----------------------------------------------------------'), nl,
    write('1. Consultar o estafeta que utilizou mais vezes um meio de transporte mais ecológico.'), nl,
    write('2. Consultar os estafetas que entregaram determinada(s) encomenda(s) a um determinado cliente.'), nl,
    write('3. Consultar os clientes servidos por um determinado estafeta.'), nl,
    write('4. Calcular o valor faturado pela Green Distribution num determinado dia.'), nl,
    write('5. Consultar quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution.'), nl,
    write('6. Calcular a classificação média de satisfação de cliente para um determinado estafeta.'), nl,
    write('7. Consultar o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo.'), nl,
    write('8. Consultar o número total de entregas pelos estafetas, num determinado intervalo de tempo.'), nl,
    write('9. Calcular o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo.'), nl,
    write('10. Calcular o peso total transportado por estafeta num determinado dia.'), nl,
    write('11. Consultar o cliente que fez mais encomendas.'), nl,
    write('0. Sair'), nl,
    write('-----------------------------------------------------------------------------------------------------------------------'), nl,nl,
    write('Escolha um: '),
    read(X),
    (X = 0 -> !, fail; true),
    nl,
    funcionalidade(X),fail.

% # Funcionalidades

funcionalidade(1) :-
    estafetaMaisEcologico(IDs),
    write('ID(s) do(s) estafeta(s) mais ecológicos: '),write(IDs),!,nl.

funcionalidade(2) :-
    write('Indique os IDs de encomendas, separados por vírgulas: '),!,nl,
    read_line_to_codes(user_input,IDs),
    string_to_atom(IDs,ID),
    getAtomVirgulaLista(A,ID),
    listaNumAtom(A,L),
    estafetasEncomendasCliente(L,R),nl,
    write('ID(s) do(s) estafeta(s) que entregaram as referidas encomendas: '),write(R),!,nl.

funcionalidade(3) :-
    write('Indique o ID do estafeta: '),!,nl,
    read(ID),
    clientesPorEstafeta(ID,L),nl,
    write('ID(s) do(s) cliente(s) servidos pelo estafeta '),write(ID),write(': '),write(L),!,nl.

funcionalidade(4) :-
    write('Indique uma data no formato - data(Ano,Mês,Dia): '),!,nl,
    read(Input),
    ((dataValida(Input),
    elementosData(Input,A,M,D),
    valorFaturadoDia(A,M,D,V),nl,
    write('Valor faturado em '),write(D),write('/'),write(M),write('/'),write(A),write(': '),write(V),write('€'),nl);
    write('A data que inseriu não é válida.'),nl),!.

funcionalidade(5) :-
    freguesiasMaisFrequentes(L),
    write('Zona(s) com maior volume de entregas: '),write(L),!,nl.

funcionalidade(6) :-
    write('Indique o ID do estafeta: '),!,nl,
    read(ID),
    mediaSatisfacaoEstafeta(ID,V),nl,
    write('Classificação média de satisfação de cliente: '),write(V),!,nl.
    
funcionalidade(7) :-
    write('Indique a data inicial no formato - data(Ano,Mês,Dia,Horas,Minutos): '),!,nl,
    read(Input1),
        ((nao(dataTimeValida(Input1)) -> nl,write('A data que inseriu não é válida.'),nl);
        elementosDataTime(Input1,A1,M1,D1,H1,Min1),nl,
        write('Indique a data final no formato - data(Ano,Mês,Dia,Horas,Minutos): '),!,nl,
        read(Input2),
            ((nao(dataTimeValida(Input2)) -> nl,write('A data que inseriu não é válida.'),nl);
            elementosDataTime(Input2,A2,M2,D2,H2,Min2),nl,
            numeroTotalEntregasTransporte(data(A1,M1,D1,H1,Min1),data(A2,M2,D2,H2,Min2),V1,V2,V3),
            write('Número total de entregas transportadas de carro: '),write(V1),nl,
            write('Número total de entregas transportadas de mota: '),write(V2),nl,
            write('Número total de entregas transportadas de bicicleta: '),write(V3),nl)),!.

% funcionalidade(8) :-

funcionalidade(9) :-
    write('Indique a data inicial no formato - data(Ano,Mês,Dia,Horas,Minutos): '),!,nl,
    read(Input1),
        ((nao(dataTimeValida(Input1)) -> nl,write('A data que inseriu não é válida.'),nl);
        elementosDataTime(Input1,A1,M1,D1,H1,Min1),nl,
        write('Indique a data final no formato - data(Ano,Mês,Dia,Horas,Minutos): '),!,nl,
        read(Input2),
            ((nao(dataTimeValida(Input2)) -> nl,write('A data que inseriu não é válida.'),nl);
            elementosDataTime(Input2,A2,M2,D2,H2,Min2),nl,
            numEntregasNaoEntregas(data(A1,M1,D1,H1,Min1),data(A2,M2,D2,H2,Min2),V1,V2,V3),
            write('Número total de encomendas entregues: '),write(V1),nl,
            write('Número total de encomendas não entregues naquele período de tempo: '),write(V2),nl,
            write('Número total de encomendas nunca entregues: '),write(V3),nl)),!.

funcionalidade(10) :-
    write('Indique uma data no formato - data(Ano,Mês,Dia): '),!,nl,
    read(Input),
    elementosData(Input,A,M,D),
    pesoTotalEstafetasDia(A,M,D,L),nl,
    write('Peso total transportado por estafeta em '),
    write(D),write('/'),write(M),write('/'),write(A),write(': '),write(L),!,nl.

% EXTRA Funcionalidades

funcionalidade(11) :-
    clienteMaisEncomendas(IDs),
    write('ID(s) do(s) clientes(s) com mais encomendas: '), write(IDs),!,nl.