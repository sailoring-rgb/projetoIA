:- consult('baseConhecimento.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%-----------------------------------------Exclusivamente sobre datas-----------------------------------------

% Extrai o ano, mes, dia
elementosData(data(A,M,D),A,M,D).

% Extrai o ano, mes, dia, horas e minutos
elementosDataTime(data(A,M,D,H,Min),A,M,D,H,Min).

% Verifica se a segunda data é anterior à primeira ou então se ambas são iguais
comparaDatas(data(AH,MH,DH,HH,MinH),data(AP,MP,DP,HP,MinP)) :-
    AP < AH;
    (AP =:= AH, (MP < MH;
                (MP =:= MH, (DP < DH;
                            (DP =:= DH, (HP < HH;
                                        (HH =:= HP, (MinP < MinH;
                                                    (MinP =:= MinH))))))))).

% Verifica se uma data é valida                                               
dataValida(data(A,M,D,H,Min)) :-
    A =\= 0, membro(M,[1,3,5,7,8,10,12]), D >= 1, D =< 31, H >= 0, H =< 23, Min >= 0, Min < 60.
dataValida(data(A,M,D,H,Min)) :-
    A =\= 0, membro(M,[4,6,9,11]), D >= 1, D =< 30, H >= 0, H =< 23, Min >= 0, Min < 60.
dataValida(data(A,M,D,H,Min)) :-
    A =\= 0, M =:= 2, mod(A,4) =:= 0, D >= 1, D =< 29, H >= 0, H =< 23, Min >= 0, Min < 60.
dataValida(data(A,M,D,H,Min)) :-
    A =\= 0, M =:= 2, mod(A,4) =\= 0, D >= 1, D =< 28, H >= 0, H =< 23, Min >= 0, Min < 60.

%--------------------------------------Usadas em várias funcionalidades--------------------------------------

% Devolve a lista dos ids das encomendas de um estafeta
encomendasDoEstafeta(IdEstaf,Lista) :-
    estafeta(IdEstaf,Lista0),
	encomendasDaLista(Lista0,Lista).

% Devolve a lista dos ids das encomendas a partir da lista de encomendas de um estafeta: [(IdEnc,Nota,Rua,Freguesia)|T]
encomendasDaLista([],[]).
encomendasDaLista([(X,_,_,_)],[X]).
encomendasDaLista([(X,_,_,_)|T],Lista) :-
	encomendasDaLista(T,Lista0),
	adiciona(X,Lista0,Lista).

% Devolve uma lista com os ids de todas as encomendas do sistema
listaTodasEncomendas(ListaEnc) :-
    solucoes(IdEstaf,estafeta(IdEstaf,_),Lista0),
    listaTodasEncomendas(Lista0,ListaEnc).

% Devolve uma lista com os ids de todas as encomendas a partir da lista de todos os estafetas do sistema
listaTodasEncomendas([],[]).
listaTodasEncomendas([IdEstaf],ListaEnc) :-
    encomendasDoEstafeta(IdEstaf,ListaEnc).
listaTodasEncomendas([IdEstaf|T],ListaEnc) :-
    encomendasDoEstafeta(IdEstaf,Lista0),
    listaTodasEncomendas(T,Lista1),
    concatena(Lista0,Lista1,ListaEnc).

%--------------------------------------Auxiliares para Funcionalidade 1--------------------------------------

% Conta o número de encomendas cujo transporte foi mais ecológico, ou seja, por bicicleta
estafetaEncomendasEcologicas( IdEstaf, Conta ) :-
	encomendasDoEstafeta( IdEstaf, Lista ),
	encomendasPorBicicleta( Lista, Conta ).

% Devolve o número de encomendas (duma lista) transportadas pelo meio de transporte bicicleta
encomendasPorBicicleta([],0).
encomendasPorBicicleta([IdEnc|T],Conta) :-
	nao(encomenda(IdEnc,_,_,_,_,_,_,'Bicicleta')),
	encomendasPorBicicleta( T, Conta ).
encomendasPorBicicleta([IdEnc|T],Conta) :-
	encomenda(IdEnc,_,_,_,_,_,_,'Bicicleta'),
	encomendasPorBicicleta(T,Conta0),
	Conta is Conta0 + 1.

%--------------------------------------Auxiliares para Funcionalidade 2--------------------------------------

% Devolve os estafetas que entregaram determinada encomenda 
 
estafetasEncCliente(IdEnc,L) :-
	solucoes(IdEstf,estafetaFezEncomenda(IdEstf,IdEnc),R),
	sort(R,L).

estafetaFezEncomenda(IdEstf, IdEnc) :- 
	encomendasDoEstafeta(IdEstf,L),
	membro(IdEnc,L).

%--------------------------------------Auxiliares para Funcionalidade 3--------------------------------------

% Devolve a lista dos ids dos clientes que estão associados aos ids das encomendas
listaClientesDasEnc([],[]).
listaClientesDasEnc([IdEnc|T],Lista) :-
    clienteDaEncomenda(IdEnc,IdCliente),
    listaClientesDasEnc(T,Lista1),
    adiciona(IdCliente,Lista1,Lista).

% Devolve o id do cliente de uma encomenda
clienteDaEncomenda( IdEnc, IdClient ) :-
    encomenda(IdEnc,X,_,_,_,_,_,_),
    IdClient is X.

%--------------------------------------Auxiliares para Funcionalidade 4--------------------------------------

% Falta completar
% Devolve o preço associado ao serviço de entrega de uma encomenda
% precoEncomenda(IdEnc,P) :- encomenda(IdEnc,_,Peso,Vol,Prazo,_,_,Trspt),
%	P is 5*Peso + 4*Vol + 

% Devolve a lista com os preços relativos a uma lista de encomendas
precosListaEncomendas([],[]).
precosListaEncomendas([IdEnc|T],L) :- 
	precoEncomenda(IdEnc,P),
	adiciona(P,L1,L).

% Devolve a soma dos preços das encomendas
totalEncomendas(L,V) :- soma(L,V).

% Devolve todas as encomendas entregues num determinado dia
encomendasDia(A,M,D,L) :- solucoes(IdEnc, encomenda(IdEnc,_,_,_,_,_,data(A,M,D,_,_),_), L).

%--------------------------------------Auxiliares para Funcionalidade 5--------------------------------------

freguesiaDoEstafeta(IdEstaf,Freguesia) :-
	estafeta(IdEstaf,Lista0),
	freguesiaDaLista(Lista0,Freguesia).

freguesiaDaLista([(_,_,_,X)|T],X).

%--------------------------------------Auxiliares para Funcionalidade 6--------------------------------------

% Devolve a lista das classificações de um estafeta
classificacoesDoEstafeta(IdEstaf,L) :-
	estafeta(IdEstaf,L1),
	classificacoesDaLista(L1,L).

% Devolve a lista das classificações a partir da lista de encomendas de um estafeta: [(IdEnc,Nota,Rua,Freguesia)|T]
classificacoesDaLista([],[]).
classificacoesDaLista([(_,C,_,_)], [C]).
classificacoesDaLista([(_,C,_,_)|T],L) :-
	classificacoesDaLista(T,L1),
	adiciona(C,L1,L).    

%--------------------------------------Auxiliares para Funcionalidade 7--------------------------------------

contaEntregasIntervalo([IdEnc|T],data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Contador) :-
    listaEntregasIntervalo([IdEnc|T],data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Lista),
    comprimento(Lista,Contador).

verificaIntervalo(data(AnoInicio,MesInicio,DiaInicio,HoraInicio,MinutoInicio),data(Ano,Mes,Dia,Hora,Minuto),data(AnoFim,MesFim,DiaFim,HoraFim,MinutoFim)) :-
    dataValida(data(Ano,Mes,Dia,Hora,Minuto)),
    comparaDatas(data(Ano,Mes,Dia,Hora,Minuto),data(AnoInicio,MesInicio,DiaInicio,HoraInicio,MinutoInicio)),
    comparaDatas(data(AnoFim,MesFim,DiaFim,HoraFim,MinutoFim),data(Ano,Mes,Dia,Hora,Minuto)).

listaEntregasIntervalo([],[]).
listaEntregasIntervalo([IdEnc|T],data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Lista) :-
    encomenda(IdEnc,_,_,_,_,_,data(Ano,Mes,Dia,Hora,Minuto),_),
    nao(verificaIntervalo(data(AI,MI,DI,HI,MinI),data(Ano,Mes,Dia,Hora,Minuto),data(AF,MF,DF,HF,MinF))),
    listaEntregasIntervalo(T,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Lista).
listaEntregasIntervalo([IdEnc|T],data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Lista) :-
    encomenda(IdEnc,_,_,_,_,_,data(Ano,Mes,Dia,Hora,Minuto),_),
    verificaIntervalo(data(AI,MI,DI,HI,MinI),data(Ano,Mes,Dia,Hora,Minuto),data(AF,MF,DF,HF,MinF)),
    contaEntregasIntervalo(T,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Lista0),
    adiciona(IdEnc,Lista0,Lista).
    
%--------------------------------------Auxiliares para Funcionalidade 10--------------------------------------

% Devolve o peso de uma encomenda 
pesoEstaf(IdEnc,Peso) :- 
   encomenda(IdEnc,_,P,_,_,_,_,_),
   Peso is P.

% Devolve a lista dos pesos 
 pesoEstafLista([],[]).
 pesoEstafLista([IdEnc|R],L) :- 
    pesoEstaf(IdEnc,P),
	pesoEstafLista(R,L1),
	adiciona(P,L1,L).



% Devolve a soma dos pesos das encomendas
totalPesoEnc(L,V) :- soma(L,V).
%---------------------------------------------------Extras---------------------------------------------------

% Adiciona um elemento a uma lista caso este ainda não pertença
adiciona( X,[],[X] ).
adiciona( X,L,[X|L] ) :- nao( membro(X,L) ).
adiciona( X,L,L ) :- membro( X,L ).

% Concatena duas listas sem elementos repetidos
concatena(L1,L2,CL):-
	concatenaAux(L1,L2,L0),
	append(L0,L2,AL),
	sort(AL,CL).

concatenaAux([],_,[]).
concatenaAux([H|T],L2,[H|R]):-
    nao(membro(H,L2)),
    concatenaAux(T,L2,R).
concatenaAux([H|T],L2,R):-
    membro(H,L2),
    concatenaAux(T,L2,R).

% Extensao do meta-predicado nao: Questao -> {V,F}
nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

% Extensao do meta-predicado membro: Elemento,Lista -> {V,F}
membro(X, [X|_]).
membro(X, [_|Xs]):- membro(X, Xs).

% Devolve todas as soluções que respeitam uma determinada condição
solucoes(X,Y,Z) :- findall(X,Y,Z).

% Comprimento de uma lista
comprimento(S,N) :- length(S,N).

% Soma os elementos de uma lista 
soma([],0).
soma([X|Y],Total) :- soma(Y, Ac), Total is X + Ac.
