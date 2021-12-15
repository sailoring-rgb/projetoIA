:- consult('baseConhecimento.pl').
:- consult('auxiliares1.pl').
:- consult('invariantes.pl').
:- consult('evolucaoInvolucao.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%FUNCIONALIDADES
		
%---------------------------------------------Funcionalidade 1---------------------------------------------
% Extensão do predicado estafetaMaisEcologico: Lista,Maximo,Id -> {V,F}
% Identifica o estafeta que utilizou mais vezes um meio de transporte mais ecológico

estafetaMaisEcologico(Ids) :-
	solucoes(IdEstaf,estafeta(IdEstaf,_),ListaEstaf),
	estafetaMaisEcologico(ListaEstaf,Max,Ids).

estafetaMaisEcologico([],0,[]).
estafetaMaisEcologico([IdEstaf],Max,[IdEstaf]) :-
	encomendasPorBicicleta(IdEstaf,Contador),
	Max = Contador.	
estafetaMaisEcologico([IdEstaf|T],Max,IdsMax) :-
	encomendasPorBicicleta(IdEstaf,Contador),
	estafetaMaisEcologico(T,ContadorMax,Ids),
	((Contador > ContadorMax -> Max = Contador, IdsMax = [IdEstaf]);
	(Contador == ContadorMax -> Max = ContadorMax, IdsMax = [IdEstaf|Ids]);
	 Max = ContadorMax, IdsMax = Ids).
	 
%---------------------------------------------Funcionalidade 2---------------------------------------------
% Extensao do predicado estafetasEncomendasCliente: Lista,Lista -> {V,F}
% Identifica que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente

estafetasEncomendasCliente([],[]).
estafetasEncomendasCliente([IdEnc],L) :- estafetaEncCliente(IdEnc,R), adiciona((IdEnc,R),L1,L).
estafetasEncomendasCliente([IdEnc|Es],L) :- 
	estafetaEncCliente(IdEnc,R),
	estafetasEncomendasCliente(Es,L1),
	adiciona((IdEnc,R),L1,L).

%---------------------------------------------Funcionalidade 3---------------------------------------------
% Extensão do predicado clientesPorEstafeta: Id,Lista -> {V,F}
% Identifica os clientes servidos por um determinado estafeta

clientesPorEstafeta(IdEstaf,ListaR) :-
    encomendasDoEstafeta(IdEstaf,Lista0),
    listaClientesDasEnc(Lista0,Lista1),
    sort(0,@<,Lista1,ListaR).

%---------------------------------------------Funcionalidade 4---------------------------------------------
% Extensão do predicado valorFaturadoDia: Ano,Mes,Dia,Valor -> {V,F}

valorFaturadoDia(A,M,D,V) :- 
	encomendasDia(A,M,D,L),
	precosListaEncomendas(L,R),
	totalEncomendas(R,V).

%---------------------------------------------Funcionalidade 5---------------------------------------------
% Extensão do predicado freguesiasComMaisEnc: Lista,Max, Lista -> {V,F}
% Identifica quais as zonas (e.g., rua ou freguesia) com maior volume de entregas por parte da Green Distribution

freguesiasMaisFrequentes(ListaMaisFrequentes) :-
	solucoes(IdEstaf,estafeta(IdEstaf,_),ListaEstaf),
	todasAsFreguesias(ListaEstaf,ListaTodasFreg),
	freguesiasMaisFrequentes(ListaTodasFreg,ListaEstaf,Max,ListaMaisFrequentes).

freguesiasMaisFrequentes([],_,0,[]).
freguesiasMaisFrequentes([Freguesia],ListaEstaf,Max,[Freguesia]) :-
    contaEntregasFreguesia(Freguesia,ListaEstaf,Contador),
    Max = Contador.
freguesiasMaisFrequentes([Freguesia|T],ListaEstaf,Max,ListaMaisFrequentes) :-
    contaEntregasFreguesia(Freguesia,ListaEstaf,Contador),
    freguesiasMaisFrequentes(T,ListaEstaf,ContadorMax,ListaFregAux),
    ((Contador > ContadorMax -> Max = Contador, ListaMaisFrequentes = [Freguesia]);
     (Contador == ContadorMax -> Max = ContadorMax, ListaMaisFrequentes = [Freguesia|ListaFregAux]);
     (Contador < ContadorMax, Max = ContadorMax, ListaMaisFrequentes = ListaFregAux)).
    
%---------------------------------------------Funcionalidade 6---------------------------------------------
% Extensão do predicado mediaSatisfacaoEstafeta : Id, Media -> {V,F}
% Calcular a classificação media de satisfação de cliente para um determinado estafeta

mediaSatisfacaoEstafeta(IdEstaf,Media) :- 
	classificacoesDoEstafeta(IdEstaf,L),
	soma(L,S),
	comprimento(L,C),
	Media is S / C.

%---------------------------------------------Funcionalidade 7---------------------------------------------
% Extensão do predicado numeroTotalEntregas : DataInicio, DataFim, Contador-> {V,F}
% Identifica o número total de entregas pelos diferentes meios de transporte, num determinado intervalo de tempo

numeroTotalEntregasTransporte(data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaCarro,ContaMota,ContaBicicleta) :-
	solucoes(IdEstaf,estafeta(IdEstaf,_),ListaEstaf),
	contaEntregasIntervalo(data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ListaEntregasPeriodo,_),
    contaPorTransporteIntervalo(ListaEstaf,ListaEntregasPeriodo,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaCarro,ContaMota,ContaBicicleta).

%---------------------------------------------Funcionalidade 8---------------------------------------------
% Extensão do predicado totalEntregasEstafetas : DataInicio,DataFim, Total -> {V,F}
% Identificar o número total de entregas pelos estafetas, num determinado intervalo de tempo

totalEntregasEstafetas(data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Total) :- 
    solucoes(IdEnc,encomenda(IdEnc,_,_,_,_,_,_),Encs),
    contaEntregasIntervalo(Encs,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Lista),
    comprimento(Lista,Total).
    
%---------------------------------------------Funcionalidade 9---------------------------------------------
% Extensão do predicado numEntregasNaoEntregas : DataInicio, DataFim, ContadorEntregas, ContadorNaoEntregas, ContadorNuncaEntregues -> {V,F}
% Calcula o número de encomendas entregues e não entregues pela Green Distribution, num determinado período de tempo, e, ainda, o número de
% encomendas nunca entregues.

%Contador1: número de encomendas entregues naquele intervalo de tempo
%Contador2: número de encomendas não entregues naquele intervalo de tempo
%Contador3: número de encomendas nunca entregues

numEntregasNaoEntregas(data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Contador1,Contador2,Contador3) :-
    solucoes(IdEnc,encomenda(IdEnc,_,_,_,_,_,_),ListaTodasEnc),
	contaEntregasIntervalo(data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ListaEntregasPeriodo,Contador1),
    contaNaoEntregasIntervalo(ListaTodasEnc,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Contador2,Contador3).

%---------------------------------------------Funcionalidade 10---------------------------------------------
% Extensão do predicado pesoTotalEstafetasDia : Ano,Mes,Dia,Lista -> {V,F}
% Calcula o peso total transportado por estafeta num determinado dia

pesoTotalEstafetasDia(A,M,D,L) :-
  encomendasDia(A,M,D,L1),
  listaPesoTotalDia(L1,[],R),
  sort(R,L).

%-------------------------------------------Funcionalidade Extra1--------------------------------------------

clienteMaisEncomendas(Ids) :-
	solucoes(IdCliente,cliente(IdCliente),ListaClientes),
    clienteMaisEncomendas(ListaClientes,Max,Ids).

clienteMaisEncomendas([],0,[]).
clienteMaisEncomendas([IdCliente],Max,[IdCliente]) :-
    solucoes(IdEnc,encomenda(IdEnc,IdCliente,_,_,_,_,_),ListaEncCliente),
    comprimento(ListaEncCliente,Contador),
	Max = Contador.
clienteMaisEncomendas([IdCliente|T],Max,ListaIdMax) :-
    solucoes(IdEnc,encomenda(IdEnc,IdCliente,_,_,_,_,_),ListaEncCliente),
    comprimento(ListaEncCliente,Contador),
    clienteMaisEncomendas(T,ContadorMax,ListaAux),
    ((Contador > ContadorMax -> Max = Contador, ListaIdMax = [IdCliente]);
     (Contador == ContadorMax -> Max = ContadorMax, ListaIdMax = [IdCliente|ListaAux]);
     Max = ContadorMax, ListaIdMax = ListaAux).

%-------------------------------------------Funcionalidade Extra2--------------------------------------------
% Extensão do predicado estafetasMenosPontuais : Lista -> {V,F}
% Identifica quais estefetas são menos pontuais, ou seja, quais têm um maior rácio entre encomendas 
% não entregues/entregues com atraso e encomendas entregues

estafetasMenosPontuais(L) :- 
	getIdsEstafetas(Estafs),
    encomendasNaoEntreguesEAtrasadas(R),
    racioEstafetasAux(Estafs,R,Ratio),
    estafetasMaiorRacio(Ratio,R,L).