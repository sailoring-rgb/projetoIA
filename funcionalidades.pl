:- consult('baseConhecimento.pl').
:- consult('aux.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%Funcionalidades
		
%-----------------------------------------------------------------------------------------------------------
% Extensão do predicado estafetaMaisEcologico: Lista,Maximo,Id -> {V,F}
% Identifica o estafeta que utilizou mais vezes um meio de transporte mais ecológico

estafetaMaisEcologico([],0,_).

estafetaMaisEcologico([IdEstaf],Max,IdEstaf) :-
	estafetaEncomendasEcologicas(IdEstaf,Count),
	Max = Count.
	
estafetaMaisEcologico([IdEstaf|T],Max,IdMax) :-
	estafetaEncomendasEcologicas(IdEstaf,Count),
	estafetaMaisEcologico(T,CountMax,Id),
	(Count > CountMax -> Max = Count, IdMax = IdEstaf;
	 Max = CountMax, IdMax = Id).
	 
%-----------------------------------------------------------------------------------------------------------
% Extensao do predicado estafetasEncomendasCliente: Lista,Lista -> {V,F}
% Identifica que estafetas entregaram determinada(s) encomenda(s) a um determinado cliente

estafetasEncomendasCliente([],[]).
estafetasEncomendasCliente([IdEnc],L) :- estafetasEncCliente(IdEnc,L).
estafetasEncomendasCliente([IdEnc|Es],L) :- 
	estafetasEncCliente(IdEnc,R),
	estafetasEncomendasCliente(Es,L1),
	append([R],[L1],L).