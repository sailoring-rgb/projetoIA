:- consult('baseConhecimento.pl').
:- consult('auxiliares1.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%--------------------------------------Usadas em várias funcionalidades--------------------------------------

%Bicicleta - 10 km/h
%Moto - 35 km/h
%Carro - 25 km/h

velocidadeEntrega(IdEnc,Velocidade) :-
    encomenda(IdEnc,_,Peso,_,_,_,_),
    transporteEncomenda(IdEnc,Transporte),
    ((Transporte == 'Bicicleta' -> Velocidade is 10 - Peso * 0.7);
     (Transporte == 'Moto' -> Velocidade is 35 - Peso * 0.5);
     (Transporte == 'Carro' -> Velocidade is 25 - Peso * 0.7)).