:- consult('baseConhecimento.pl').
:- consult('aux.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%Funcionalidades
		
%---------------------------------------------Funcionalidade 1---------------------------------------------
% Extensão do predicado estafetaMaisEcologico: Lista,Maximo,Id -> {V,F}
% Identifica o estafeta que utilizou mais vezes um meio de transporte mais ecológico

estafetaMaisEcologico([],0,_).

estafetaMaisEcologico([IdEstaf],Max,IdEstaf) :-
	estafetaEncomendasEcologicas(IdEstaf,Contador),
	Max = Contador.
	
estafetaMaisEcologico([IdEstaf|T],Max,IdMax) :-
	estafetaEncomendasEcologicas(IdEstaf,Contador),
	estafetaMaisEcologico(T,ContadorMax,Id),
	(Contador > ContadorMax -> Max = Contador, IdMax = IdEstaf;
	 Max = ContadorMax, IdMax = Id).
	 
%---------------------------------------------Funcionalidade 2---------------------------------------------
% Extensao do predicado estafetasEncomendasCliente: Lista,Lista -> {V,F}
% Identifica que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente

estafetasEncomendasCliente([],[]).
estafetasEncomendasCliente([IdEnc],L) :- estafetasEncCliente(IdEnc,L).
estafetasEncomendasCliente([IdEnc|Es],L) :- 
	estafetasEncCliente(IdEnc,R),
	estafetasEncomendasCliente(Es,L1),
	append([R],[L1],L).

%---------------------------------------------Funcionalidade 3---------------------------------------------
% Extensão do predicado clientesPorEstafeta: Id,Lista -> {V,F}
% Identifica os clientes servidos por um determinado estafeta

clientesPorEstafeta(IdEstaf,ListaR) :-
    encomendasDoEstafeta(IdEstaf,Lista0),
    listaClientesDasEnc(Lista0,Lista1),
    sort(0,@<,Lista1,ListaR).
