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
                                                    
% Verifica se uma data AAAA-MM-DD é válida
dataValida(data(A,M,D)) :-
    ((A =:= 0; (M > 12; M < 1)), !, fail);
    (membro(M,[1,3,5,7,8,10,12]), D >= 1, D =< 31);
    (membro(M,[4,6,9,11]), D >= 1, D =< 30);
    (M =:= 2, (mod(A,4) =:= 0, D >= 1, D =< 29);
                              (D >= 1, D =< 28)).

% Verifica se uma data AAAA-MM-DD-HH-Min é válida                                               
dataTimeValida(data(A,M,D,H,Min)) :-
    A =\= 0, membro(M,[1,3,5,7,8,10,12]), D >= 1, D =< 31, H >= 0, H =< 23, Min >= 0, Min < 60.
dataTimeValida(data(A,M,D,H,Min)) :-
    A =\= 0, membro(M,[4,6,9,11]), D >= 1, D =< 30, H >= 0, H =< 23, Min >= 0, Min < 60.
dataTimeValida(data(A,M,D,H,Min)) :-
    A =\= 0, M =:= 2, mod(A,4) =:= 0, D >= 1, D =< 29, H >= 0, H =< 23, Min >= 0, Min < 60.
dataTimeValida(data(A,M,D,H,Min)) :-
    A =\= 0, M =:= 2, mod(A,4) =\= 0, D >= 1, D =< 28, H >= 0, H =< 23, Min >= 0, Min < 60.

% Verifica se uma data está dentro de um intervalo de tempo
verificaIntervalo(data(AnoInicio,MesInicio,DiaInicio,HoraInicio,MinutoInicio),data(Ano,Mes,Dia,Hora,Minuto),data(AnoFim,MesFim,DiaFim,HoraFim,MinutoFim)) :-
    dataTimeValida(data(Ano,Mes,Dia,Hora,Minuto)),
    comparaDatas(data(Ano,Mes,Dia,Hora,Minuto),data(AnoInicio,MesInicio,DiaInicio,HoraInicio,MinutoInicio)),
    comparaDatas(data(AnoFim,MesFim,DiaFim,HoraFim,MinutoFim),data(Ano,Mes,Dia,Hora,Minuto)).
    
%--------------------------------------Usadas em várias funcionalidades--------------------------------------

% Devolve a lista dos ids das encomendas de um estafeta
encomendasDoEstafeta(IdEstaf,Lista) :-
    estafeta(IdEstaf,Lista0),
	encomendasDaLista(Lista0,Lista).

% Devolve a lista dos ids das encomendas a partir da lista de encomendas de um estafeta: [(IdEnc,Nota,Velocidade,Transporte,Rua,Freguesia)|T]
encomendasDaLista([],[]).
encomendasDaLista([(X,_,_,_,_,_)],[X]).
encomendasDaLista([(X,_,_,_,_,_)|T],Lista) :-
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

% Devolve o número de encomendas (duma lista) transportadas pelo meio de transporte bicicleta
encomendasPorBicicleta(IdEstaf,Conta) :-
    estafeta(IdEstaf,Lista),
	verificaBicicleta(Lista,Conta).

verificaBicicleta([],0).
verificaBicicleta([(_,_,_,Transporte,_,_)|T],Conta) :-
    verificaBicicleta(T,Conta0),
    ((Transporte == 'Bicicleta' -> Conta is Conta0 + 1);
     Conta is Conta0).

%--------------------------------------Auxiliares para Funcionalidade 2--------------------------------------

% Devolve os estafetas que entregaram determinada encomenda 
 
estafetasEncCliente(IdEnc,L) :-
	solucoes(IdEstaf,estafetaFezEncomenda(IdEstaf,IdEnc),R),
	sort(R,L).

estafetaFezEncomenda(IdEstaf, IdEnc) :- 
	encomendasDoEstafeta(IdEstaf,L),
	membro(IdEnc,L).

%--------------------------------------Auxiliares para Funcionalidade 3--------------------------------------

% Devolve a lista dos ids dos clientes que estão associados aos ids das encomendas
listaClientesDasEnc([],[]).
listaClientesDasEnc([IdEnc|T],Lista) :-
    clienteDaEncomenda(IdEnc,IdCliente),
    listaClientesDasEnc(T,Lista1),
    adiciona(IdCliente,Lista1,Lista).

% Devolve o id do cliente de uma encomenda
clienteDaEncomenda(IdEnc,IdCliente) :-
    encomenda(IdEnc,X,_,_,_,_,_),
    IdCliente is X.

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

% Devolve a freguesia de um estafeta
freguesiaDoEstafeta(IdEstaf,Freguesia) :-
	estafeta(IdEstaf,Lista0),
	freguesiaDaLista(Lista0,Freguesia).

% Devolve a freguesia a partir da lista [(IdEnc,Nota,Velocidade,Transporte,Rua,Freguesia)|T]
freguesiaDaLista([(_,_,_,_,_,X)|T],X).

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

%--------------------------------------Auxiliar para Funcionalidades 7 e 9---------------------------------------

% Conta o número de encomendas entregues e o número de encomendas não entregues num intervalo de tempo
% Contador0 - nº de encomendas entregues naquele período de tempo
% Contador1 - nº de encomendas que foram entregues antes ou depois daquele período de tempo
% Contador2 - nº de encomendas que nunca chegaram a ser entregues

contaEntregasIntervalo([],_,_,0,0,0).
contaEntregasIntervalo([IdEnc|T],data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaEntregasPeriodo,ContaNaoEntregasPeriodo,ContaNuncaEntregues) :-
    encomenda(IdEnc,_,_,_,_,_,data(Ano,Mes,Dia,Hora,Minuto)),
    verificaIntervalo(data(AI,MI,DI,HI,MinI),data(Ano,Mes,Dia,Hora,Minuto),data(AF,MF,DF,HF,MinF)),
    contaEntregasIntervalo(T,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Contador0,ContaNaoEntregasPeriodo,ContaNuncaEntregues),
    ContaEntregasPeriodo is Contador0 + 1.
contaEntregasIntervalo([IdEnc|T],data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaEntregasPeriodo,ContaNaoEntregasPeriodo,ContaNuncaEntregues) :-
    encomenda(IdEnc,_,_,_,_,_,data(Ano,Mes,Dia,Hora,Minuto)),
    nao(verificaIntervalo(data(AI,MI,DI,HI,MinI),data(Ano,Mes,Dia,Hora,Minuto),data(AF,MF,DF,HF,MinF))),
    dataTimeValida(data(Ano,Mes,Dia,Hora,Minuto)),
    contaEntregasIntervalo(T,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaEntregasPeriodo,Contador1,ContaNuncaEntregues),
    ContaNaoEntregasPeriodo is Contador1 + 1.
contaEntregasIntervalo([IdEnc|T],data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaEntregasPeriodo,ContaNaoEntregasPeriodo,ContaNuncaEntregues) :-
    encomenda(IdEnc,_,_,_,_,_,data(Ano,Mes,Dia,Hora,Minuto)),
    nao(dataTimeValida(data(Ano,Mes,Dia,Hora,Minuto))),
    contaEntregasIntervalo(T,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaEntregasPeriodo,ContaNaoEntregasPeriodo,Contador2),
    ContaNuncaEntregues is Contador2 + 1.

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

% Verifica o tipo de dados da lista de encomendas do estafeta
verificaDadosLista([]).
verificaDadosLista([(IdEnc,Nota,Velocidade,Transporte,Rua,Freguesia)|T]) :- 
        integer(IdEnc),
        float(Nota),
        integer(Velocidade),
        atom(Transporte),
        atom(Rua),
        atom(Freguesia).

% Devolve a freguesia de um estafeta numa lista (a lista contém apenas um elemento)
% Este predicado só foi preciso para um invariante estrutural
listaFreguesiasEstaf(IdEstaf,Lista,ListaFreg) :-
    listaFreguesiasEstaf(Lista,ListaFreg).

listaFreguesiasEstaf([],[]).
listaFreguesiasEstaf([(_,_,_,_,_,Freg)|T],ListaFreg) :-
    listaFreguesiasEstaf(T,Lista0),
    adiciona(Freg,Lista0,ListaFreg).

% Verifica se o peso da encomenda e a velocidade do estafeta respeitaram os limites do meio de transporte usado
transportePesoVelocidade([]).
transportePesoVelocidade([(IdEnc,_,Velocidade,Transporte,_,_)|T]) :-
    encomenda(IdEstaf,_,Peso,_,_,_,_),
    ((Transporte == 'Bicicleta', Peso =< 5, Velocidade == 10);
     (Transporte == 'Mota', Peso =< 20, Velocidade == 35);
     (Transporte == 'Carro', Peso =< 100, Velocidade == 25)),
     transportePesoVelocidade(T).
