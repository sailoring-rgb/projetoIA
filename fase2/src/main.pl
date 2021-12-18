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
    write('---------------------------------------------------------FASE1----------------------------------------------------------'), nl,
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
    write('11. Consultar o cliente que fez mais encomendas.'),nl,
    write('12. Consultar os estefetas menos pontuais a fazer as suas entregas.'),nl,
    write('0. Sair'), nl,
    write('-----------------------------------------------------------------------------------------------------------------------'), nl,nl,
    write('Escolha um: '),nl,
    read(Y),
    (Y = 0 -> !, fail; true),
    nl,
    funcionalidade(Y),fail.


%----------------------------------------------------------------FASE1----------------------------------------------------------------

% # FUNCIONALIDADES DA FASE 1 

funcionalidade(1) :-
    estafetaMaisEcologico(IDs),
    write('ID(s) do(s) estafeta(s) mais ecológicos: '),write(IDs),!,nl.

funcionalidade(2) :-
    write('Indique os IDs de encomendas, separados por vírgulas: '),!,nl,
    read(Input),
    format(atom(A), "~w", Input),
    atomic_list_concat(N,,,A),
    convertAtomsNumbers(N,L),
    estafetasEncomendasCliente(L,R),nl,
    write('ID(s) do(s) estafeta(s) que entregaram as referidas encomendas: '),write(R),!,nl.

funcionalidade(3) :-
    write('Indique o ID do estafeta: '),!,nl,
    read(ID),
    clientesPorEstafeta(ID,L),nl,
    write('ID(s) do(s) cliente(s) servidos pelo estafeta '),write(ID),write(': '),write(L),!,nl.

funcionalidade(4) :-
    write('Indique uma data no formato - data(Ano,Mês,Dia): '),!,nl,
    read(Input),nl,
    ((dataValida(Input),
    elementosData(Input,A,M,D),
    valorFaturadoDia(A,M,D,V),nl,
    write('Valor faturado em '),write(D),write('/'),write(M),write('/'),write(A),write(': '),write(V),write('€'),nl);
    (nao(dataValida(Input)),write('A data que inseriu não é válida.'),nl)),!.

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

funcionalidade(8) :-
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

funcionalidade(9) :-
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

funcionalidade(10) :-
    write('Indique uma data no formato - data(Ano,Mês,Dia): '),!,nl,
    read(Input),nl,
    ((dataValida(Input),
    elementosData(Input,A,M,D),
    pesoTotalEstafetasDia(A,M,D,L),nl,
    write('Peso total transportado por estafeta em '),
    write(D),write('/'),write(M),write('/'),write(A),write(': '),write(L),nl);
    (nao(dataValida(Input)),write('A data que inseriu não é válida.'),nl)),!.

% EXTRA Funcionalidades

funcionalidade(11) :-
    clienteMaisEncomendas(IDs),
    write('ID(s) do(s) cliente(s) com mais encomendas: '), write(IDs),!,nl.

funcionalidade(12) :-
    estafetasMenosPontuais(L),
    write('IDs dos estafetas menos pontuais: '),write(L),!,nl.


%----------------------------------------------------------------FASE2----------------------------------------------------------------

% # 1 - DFS
% # 2 - BFS
% # 3 - DFS Limitada
% # 4 - Gulosa
% # 5 - A*

obterEstatisticas(1,Caminho,Distancia) :-
    statistics(global_stack,[M1,L1]),
    time(resolveDFS('São Victor',Caminho,Distancia)),
    statistics(global_stack,[M2,L2]),
    Mem is M2 - M1,
    write("Memória usada: "),write(Mem),nl,
	write("Custo: "),write(Distancia).

obterEstatisticas(2,Caminho,Distancia) :-
    statistics(global_stack,[M1,L1]),
    time(resolveBFS('São Victor',Caminho,Distancia)),
    statistics(global_stack,[M2,L2]),
    Mem is M2 - M1,
    write("Memória usada: "),write(Mem),nl,
	write("Custo: "),write(Distancia).

obterEstatisticas(3,Caminho,Distancia) :-
    statistics(global_stack, [M1,L1]),
    time(resolveLimitada('São Victor',Caminho,Distancia,5)),
    statistics(global_stack, [M2,L2]),
    Mem is M2 - M1,
    write("Memória usada: "),write(Mem),nl,
	write("Custo: "),write(Distancia).

obterEstatisticas(4,C,Distancia) :-
    statistics(global_stack, [M1,L1]),
    time(resolveGulosa('São Victor',C/Distancia)),
    statistics(global_stack, [M2,L2]),
    Mem is M2 - M1,
    write("Memória usada: "),write(Mem),nl,
	write("Custo: "),write(Distancia).

obterEstatisticas(5,C,Distancia) :-
    statistics(global_stack, [M1,L1]),
    time(resolveAEstrela('São Victor',C/Distancia)),
    statistics(global_stack, [M2,L2]),
    Mem is M2 - M1,
    write("Memória usada: "),write(Mem),nl,
	write("Custo: "),write(Distancia).