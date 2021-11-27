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

% Verifica se uma encomenda está associada a um determinado estafeta
encomendaPertenceEstafeta([(IdEnc,_,_,_,_,_)|T],IdEnc).
encomendaPertenceEstafeta([(X,_,_,_,_,_)|T],IdEnc) :- X \= IdEnc, encomendaPertenceEstafeta(T,IdEnc).

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

% Devolve o estafeta que entregou determinada encomenda 
estafetaEncCliente(IdEnc,IdEstaf) :- estafetaFezEncomenda(IdEstaf,IdEnc).

% Indica se um dado estafeta entregou determinada encomenda
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
% precoEncomenda(IdEnc,P) :- encomenda(IdEnc,_,Peso,Vol,Prazo,_,_),
%	P is 5*Peso + 4*Vol + 

% Devolve a lista com os preços relativos a uma lista de encomendas
precosListaEncomendas([],[]).
precosListaEncomendas([IdEnc|T],L) :- 
	precoEncomenda(IdEnc,P),
	adiciona(P,L1,L).

% Devolve a soma dos preços das encomendas
totalEncomendas(L,V) :- soma(L,V).

% Devolve todas as encomendas entregues num determinado dia
encomendasDia(A,M,D,L) :- solucoes(IdEnc, encomenda(IdEnc,_,_,_,_,_,data(A,M,D,_,_)), L).

%--------------------------------------Auxiliares para Funcionalidade 5--------------------------------------

% Conta o número de entregas feitas nessa freguesia
contaEntregasFreguesia(_,[],0).
contaEntregasFreguesia(Freguesia,[IdEstaf|T],Contador) :-
    estafeta(IdEstaf,ListaEncEstaf),
    contaEntregasFreguesiaAux(Freguesia,ListaEncEstaf,Contador1),
    contaEntregasFreguesia(Freguesia,T,Contador2),
    Contador is Contador1 + Contador2.

% Conta o número de entregas feitas nessa freguesia por um estafeta
contaEntregasFreguesiaAux(_,[],0).
contaEntregasFreguesiaAux(Freguesia,[(_,_,_,_,_,Freg)|T],Contador) :-
    (Freguesia == Freg, contaEntregasFreguesiaAux(Freguesia,T,Contador1), Contador is Contador1 + 1);
    (contaEntregasFreguesiaAux(Freguesia,T,Contador)).

% Devolve uma lista com todas as freguesias onde foram feitas entregas
todasAsFreguesias([],[]).
todasAsFreguesias([IdEstaf|T],ListaTodasFreg) :-
    freguesiasDoEstafeta(IdEstaf,ListaAux1),
    todasAsFreguesias(T,ListaAux2),
    concatena(ListaAux1,ListaAux2,ListaTodasFreg).

% Devolve a lista de todas as freguesias de um determinado estafeta
freguesiasDoEstafeta(IdEstaf,ListaFreg) :-
    estafeta(IdEstaf,ListaEncEstaf),
    freguesiasDoEstafetaAux(ListaEncEstaf,ListaFreg).

freguesiasDoEstafetaAux([],[]).
freguesiasDoEstafetaAux([(_,_,_,_,_,Freguesia)],[Freguesia]).
freguesiasDoEstafetaAux([(_,_,_,_,_,Freguesia)|T],ListaFreg) :-
    freguesiasDoEstafetaAux(T,ListaAux),
    adiciona(Freguesia,ListaAux,ListaFreg).

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

%--------------------------------------Auxiliar para Funcionalidade 7---------------------------------------

% Conta o número de encomendas entregues, num intervalo de tempo, pelos diferentes meios de transporte
contaPorTransporteIntervalo([],[],_,_,0,0,0).
contaPorTransporteIntervalo([IdEstaf],ListaEntregasPeriodo,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaCarro,ContaMota,ContaBicicleta) :-
    contaPorTransporte(IdEstaf,ListaEntregasPeriodo,ContaCarro,ContaMota,ContaBicicleta).
contaPorTransporteIntervalo([IdEstaf|T],ListaEntregasPeriodo,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaCarro,ContaMota,ContaBicicleta) :-
    contaPorTransporte(IdEstaf,ListaEntregasPeriodo,ContaCarro0,ContaMota0,ContaBicicleta0),
    contaPorTransporteIntervalo(T,ListaEntregasPeriodo,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaCarro1,ContaMota1,ContaBicicleta1),
    ContaCarro is ContaCarro0 + ContaCarro1,
    ContaMota is ContaMota0 + ContaMota1,
    ContaBicicleta is ContaBicicleta0 + ContaBicicleta1.

% Conta o número de encomendas entregues por um determinado estafeta, pelos diferentes meios de transporte
contaPorTransporte(_,[],0,0,0).
contaPorTransporte(IdEstaf,[IdEnc|T],ContaCarro,ContaMota,ContaBicicleta) :-
    estafeta(IdEstaf,Lista),
    nao(membro((IdEnc,A,B,C,D,E),Lista)),
    contaPorTransporte(IdEstaf,T,ContaCarro,ContaMota,ContaBicicleta).
contaPorTransporte(IdEstaf,[IdEnc|T],ContaCarro,ContaMota,ContaBicicleta) :-
    estafeta(IdEstaf,Lista),
    encomendaPertenceEstafeta(Lista,IdEnc),
    membro((IdEnc,A,B,Transporte,D,E),Lista),
    ((Transporte == 'Carro', contaPorTransporte(IdEstaf,T,Contador0,ContaMota,ContaBicicleta), ContaCarro is Contador0 + 1);
     (Transporte == 'Mota', contaPorTransporte(IdEstaf,T,ContaCarro,Contador1,ContaBicicleta), ContaMota is Contador1 + 1);
     (Transporte == 'Bicicleta', contaPorTransporte(IdEstaf,T,ContaCarro,ContaMota,Contador2), ContaBicicleta is Contador2 + 1)).

%--------------------------------------Auxiliar para Funcionalidade 9---------------------------------------

% Conta o número de encomendas entregues e o número de encomendas não entregues num intervalo de tempo
% ContaEntregasPeriodo - nº de encomendas entregues naquele período de tempo
% ContaNaoEntregasPeriodo - nº de encomendas que foram entregues antes ou depois daquele período de tempo
% ContaNuncaEntregues - nº de encomendas que nunca chegaram a ser entregues

contaEntregasIntervalo([],_,_,[],0).
contaEntregasIntervalo([IdEnc|T],data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ListaEntregasPeriodo,ContaEntregasPeriodo) :-
    encomenda(IdEnc,_,_,_,_,_,data(Ano,Mes,Dia,Hora,Minuto)),
    ((verificaIntervalo(data(AI,MI,DI,HI,MinI),data(Ano,Mes,Dia,Hora,Minuto),data(AF,MF,DF,HF,MinF)),
    contaEntregasIntervalo(T,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Lista0,Contador0),
    adiciona(IdEnc,Lista0,ListaEntregasPeriodo),
    ContaEntregasPeriodo is Contador0 + 1);
    (nao(verificaIntervalo(data(AI,MI,DI,HI,MinI),data(Ano,Mes,Dia,Hora,Minuto),data(AF,MF,DF,HF,MinF)))),
    contaEntregasIntervalo(T,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ListaEntregasPeriodo,ContaEntregasPeriodo)).

contaNaoEntregasIntervalo([],_,_,0,0).
contaNaoEntregasIntervalo([IdEnc|T],data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaNaoEntregasPeriodo,ContaNuncaEntregues) :-
    encomenda(IdEnc,_,_,_,_,_,data(Ano,Mes,Dia,Hora,Minuto)),
    ((nao(verificaIntervalo(data(AI,MI,DI,HI,MinI),data(Ano,Mes,Dia,Hora,Minuto),data(AF,MF,DF,HF,MinF))),
        ((dataTimeValida(data(Ano,Mes,Dia,Hora,Minuto)),
            contaNaoEntregasIntervalo(T,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Contador1,ContaNuncaEntregues),
            ContaNaoEntregasPeriodo is Contador1 + 1);
         (nao(dataTimeValida(data(Ano,Mes,Dia,Hora,Minuto))),
            contaNaoEntregasIntervalo(T,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaNaoEntregasPeriodo,Contador2),
            ContaNuncaEntregues is Contador2 + 1)));
     (verificaIntervalo(data(AI,MI,DI,HI,MinI),data(Ano,Mes,Dia,Hora,Minuto),data(AF,MF,DF,HF,MinF)),
        contaNaoEntregasIntervalo(T,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ContaNaoEntregasPeriodo,ContaNuncaEntregues))).

%--------------------------------------Auxiliares para Funcionalidade 10--------------------------------------

% Devolve o peso de uma encomenda 
pesoEnc(IdEnc,P) :- encomenda(IdEnc,_,P,_,_,_,_).

% Devolve a lista dos pesos das encomendas 
pesoEncLista([],[]).
pesoEncLista([IdEnc|R],L) :- 
    pesoEnc(IdEnc,P),
	pesoEncLista(R,L1),
	adiciona(P,L1,L).

% Adiciona a uma lista de pares o par com o id de estafeta e o peso
adicionaParPesoEstafeta(IdEstaf,Peso,[],[(IdEstaf,Peso)]).
adicionaParPesoEstafeta(IdEstaf,Peso,L, [(IdEstaf,Peso) |L]) :- nao(membroPar(IdEstaf,L)).
adicionaParPesoEstafeta(IdEstaf,Peso, [(E,P) | T], L) :- 
	(IdEstaf == E -> NovoPeso is P + Peso, L = [(E,NovoPeso) | T]; adiciona((E,P),R1,R), adicionaParPesoEstafetaAux(IdEstaf,Peso,T,R,R,L)).

adicionaParPesoEstafetaAux(IdEstaf,Peso, [(E,P) | T], R, R1, L) :- 
	(IdEstaf == E -> NovoPeso is P + Peso, concatena([(E,NovoPeso) | T], R, L); concatena([(E,P)],R1,R2), adicionaParPesoEstafetaAux(IdEstaf,Peso,T,R2,R2,L)).

% Devolve uma lista com todos os ids de estafetas existentes
getIdsEstafetas(L) :- solucoes(IdEstaf,estafeta(IdEstaf,R),L).

% Devolve a lista de pares com o estafeta e o respetivo peso transportado num dia
listaPesoTotalDia([E],L1,L) :- 
	estafetaEncCliente(E,IdEstaf),
	pesoEnc(E,P),
	adicionaParPesoEstafeta(IdEstaf,P,L1,L).
listaPesoTotalDia([E | T],L1,L) :-
	estafetaEncCliente(E,IdEstaf),
	pesoEnc(E,P),
	adicionaParPesoEstafeta(IdEstaf,P,L1,L2),
	listaPesoTotalDia(T,L2,L).

% Devolve o peso total de uma lista de encomendas
calculaPesoEncomendas([],0).
calculaPesoEncomendas(L, P) :- 
  	pesoEncLista(L,Pesos),
  	soma(Pesos,P). 

%---------------------------------------------------Extras---------------------------------------------------

% Adiciona um elemento a uma lista caso este ainda não pertença
adiciona( X,[],[X] ).
adiciona( X,L,[X|L] ) :- nao( membro(X,L) ).
adiciona( X,L,L ) :- membro( X,L ).

apaga(Lista,X,Lista0) :- delete(Lista,X,Lista0).

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

% Extensao do predicado membroPar : Elemento,Lista -> {V,F}
membroPar(X,[(X,Y) | _]).
membroPar(X,[_|Xs]) :- membroPar(X,Xs).

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
