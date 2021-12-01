:- consult('baseConhecimento.pl').

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

%-----------------------------------------Exclusivamente para invariantes------------------------------------

% Verifica que uma encomenda não se encontra na lista de encomendas de nenhum estafeta 
encomendaNaoTemEstafeta([],_).
encomendaNaoTemEstafeta([IdEstaf|T],IdEnc) :-
    estafeta(IdEstaf,Lista),
    nao(membro((IdEnc,A,B,C,D,E),Lista)),
    encomendaNaoTemEstafeta(T,IdEnc).

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

% Verifica se um estafeta não tem classificação superior a X caso tenha uma encomenda entregue com atraso (estou a assumir X como 3.5 para já)
verificaClafMaiorQueX(IdEstaf) :-
    encomendasDoEstafeta(IdEstaf,L),
    estafeta(IdEstaf,Encs),
    verificaClafMaiorQueXAux(Encs,3.5).

verificaClafMaiorQueXAux([],_).
verificaClafMaiorQueXAux([(IdEnc,Nota,_,_,_,_)],X) :- encEntregueAtraso(IdEnc),Nota =< X.
verificaClafMaiorQueXAux([(IdEnc,_,_,_,_,_)],X) :- nao(encEntregueAtraso(IdEnc)).
verificaClafMaiorQueX([(IdEnc,Nota,_,_,_,_) | T],X) :-
    (encEntregueAtraso(IdEnc) -> (Nota =< X -> verificaClafMaiorQueXAux(T,X); fail) ; verificaClafZeroAux(T,X)).

% Verifica se um estafeta tem classificação 0 caso não tenha entregue uma encomenda
verificaClafZero(IdEstaf):-
    encomendasDoEstafeta(IdEstaf,L),
    estafeta(IdEstaf,Encs),
    verificaClafZeroAux(Encs).

verificaClafZeroAux([]).
verificaClafZeroAux([(IdEnc,0,_,_,_,_)]) :- encNaoEntregue(IdEnc).
verificaClafZeroAux([(IdEnc,_,_,_,_,_)]) :- nao(encNaoEntregue(IdEnc)).
verificaClafZeroAux([(IdEnc,Nota,_,_,_,_) | T]) :-  
    (encNaoEntregue(IdEnc) -> (Nota == 0 -> verificaClafZeroAux(T); fail); verificaClafZeroAux(T)).

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
verificaIntervalo(data(AnoI,MesI,DiaI,HoraI,MinI),data(Ano,Mes,Dia,Hora,Min),data(AnoF,MesF,DiaF,HoraF,MinF)) :-
    dataTimeValida(data(Ano,Mes,Dia,Hora,Min)),
    comparaDatas(data(Ano,Mes,Dia,Hora,Min),data(AnoI,MesI,DiaI,HoraI,MinI)),
    comparaDatas(data(AnoF,MesF,DiaF,HoraF,MinF),data(Ano,Mes,Dia,Hora,Min)).

% Devolve a diferença em horas entre duas datas, assumindo que cumprem os requisitos de tempo maximo de entrega
diferencaDatas(data(AI,MI,DI,HI,MinI), data(AF,MF,DF,HF,MinF), R) :- (AI == AF -> 
	(MI == MF -> 
		(DI == DF -> R is HF-HI; R is (24-HI) + HF + 24*(DF-DI-1)); 
		diasMes(AI,MI,Dias), R is (24-HI) + HF + (DF-(Dias-DI)-1) * 24); 
	diasMes(AI,MI,Dias), R is (24-HI) + HF + (DF-(Dias-DI)-1) * 24).

% Verica se um ano é ou não bissexto
bissexto(A) :- 0 =:= mod(A,4).

% Retorna o número de dias de um mes
diasMes(A,2,Dias) :- bissexto(A) -> Dias is 29; Dias is 28. 
diasMes(_,M,Dias) :- dias31(M), Dias is 31; Dias is 30. 

% Verifica que um mes tem 31 dias
dias31(M) :- M == 1; M == 3; M == 5; M ==7; M == 8; M ==10; M == 12.
    
%----------------------------------------- Exclusivamente para penalizar Estafetas -----------------------------------------
% Devolve a lista com as encomendas entregues em atraso de um dado estafeta
encAtrasoEstafeta(IdEstaf,R) :- 
       encomendasDoEstafeta(IdEstaf,L),
       encomendasEntreguesAtraso(L1),
       comuns(L,L1,R).


% Devolve  a penalizacao do estafeta ao atraso de entrega da encomenda
calculaPenalizacaoPorAtraso(Atraso,Penalizacao) :- 
	(
        Atraso < 1 -> Penalizacao is 0;
		Atraso < 24 -> Penalizacao is 0.2;
		Atraso < 48 -> Penalizacao is 0.3;
		Penalizacao is 0.7). 

% Devolve o atraso com que foi entregue uma encomenda
calculaAtraso(DataI,DataF,Prazo,Atraso) :- 
      getPrazoEncomendaHoras(Prazo,PH),
      diferencaDatas(DataI,DataF,D),
      (D =< PH -> Atraso is 0;
       D > PH  -> Atraso is D-PH).

% Aplica a penalizacao ao estafeta
estafetaPenalizacao(IdEstaf,DataI,DataF,Prazo,L):- 
        calculaAtraso(DataI,DataF,Prazo,A),
        calculaPenalizacaoPorAtraso(A,P),
        estafeta(IdEstaf,[(_,Nota-P,_,_,_,_)]).

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

% Devolve a lista dos ids das encomendas entregues naquele intervalo de tempo
contaEntregasIntervalo(data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ListaEntregasPeriodo,Contador) :-
    solucoes(IdEnc,encomenda(IdEnc,_,_,_,_,_,_),ListaTodasEnc),
    contaEntregasIntervalo(ListaTodasEnc,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ListaEntregasPeriodo),
    comprimento(ListaEntregasPeriodo,Contador).

contaEntregasIntervalo([],_,_,[]).
contaEntregasIntervalo([IdEnc|T],data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ListaEntregasPeriodo) :-
    encomenda(IdEnc,_,_,_,_,_,data(Ano,Mes,Dia,Hora,Minuto)),
    ((verificaIntervalo(data(AI,MI,DI,HI,MinI),data(Ano,Mes,Dia,Hora,Minuto),data(AF,MF,DF,HF,MinF)),
        contaEntregasIntervalo(T,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),Lista0),
        adiciona(IdEnc,Lista0,ListaEntregasPeriodo));
    (nao(verificaIntervalo(data(AI,MI,DI,HI,MinI),data(Ano,Mes,Dia,Hora,Minuto),data(AF,MF,DF,HF,MinF))),
        contaEntregasIntervalo(T,data(AI,MI,DI,HI,MinI),data(AF,MF,DF,HF,MinF),ListaEntregasPeriodo))).

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

% Devolve o preço associado ao serviço de entrega de uma encomenda
precoEncomenda(IdEnc,P) :- 
	encomenda(IdEnc,_,Peso,Vol,Prazo,_,_),
	transporteEncomenda(IdEnc,Transporte),
	calculaPrecoPorTransporte(Transporte,PT),
	getPrazoEncomendaHoras(Prazo,Horas),
	calculaPrecoPorPrazo(Horas,PP),
	P is 3*Peso + 2*Vol + PT + PP.

% Devolve o número de horas correspondente a um prazo de uma encomenda
getPrazoEncomendaHoras(Prazo,Horas) :-
	(Prazo == 'Imediato' -> Horas is 0;
	getListaAtom([A,S],Prazo),
	numeroAtom(A,N),
	(S == 'horas' -> Horas is N; Horas is N*24)).

% Devolve o preço relativo a cada prazo de entrega
calculaPrecoPorPrazo(PrazoH,Preco) :- 
	(PrazoH < 7 -> calculaPrecoAte7Horas(PrazoH,Preco);
		PrazoH < 13 -> Preco is 8;
		PrazoH < 25 -> Preco is 5;
		PrazoH < 73 -> Preco is 4;
		PrazoH < 121 -> Preco is 3;
		Preco is 2). 

% Devolve o preço relativo a cada prazo inferior a 7 horas                               
calculaPrecoAte7Horas(0,25).
calculaPrecoAte7Horas(1,20).
calculaPrecoAte7Horas(2,18).
calculaPrecoAte7Horas(3,16).
calculaPrecoAte7Horas(4,14).
calculaPrecoAte7Horas(5,12).
calculaPrecoAte7Horas(6,10).

% Devolve o transporte de uma encomenda
transporteEncomenda(IdEnc,Transporte) :-
	estafetaEncCliente(IdEnc,IdEstaf),
	estafeta(IdEstaf,L),
	procuraTransporteEncomenda(IdEnc,L,Transporte).

% Procura o transporte de uma encomenda numa lista de encomendas de um estafeta
procuraTransporteEncomenda(IdEnc,[(IdEnc,_,_,Transporte,_,_)],Transporte).
procuraTransporteEncomenda(IdEnc,[(IdEnc,_,_,Transporte,_,_) | T], Transporte).
procuraTransporteEncomenda(IdEnc,[(_,_,_,_,_,_) | T], Transporte) :- procuraTransporteEncomenda(IdEnc,T,Transporte).

% Devolve o preço a ser cobrado conforme cada meio de transporte
calculaPrecoPorTransporte(Transporte,P) :-
	(Transporte == 'Bicicleta' -> P is 5;
	  Transporte == 'Mota' -> P is 10;
	  Transporte == 'Carro' -> P is 15).

% Devolve a lista com os preços relativos a uma lista de encomendas
precosListaEncomendas([],[]).
precosListaEncomendas([IdEnc],L) :- precoEncomenda(IdEnc,P), adiciona(P,L1,L).
precosListaEncomendas([IdEnc|T],L) :- 
	precoEncomenda(IdEnc,P), 
	precosListaEncomendas(T,L1),
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

%--------------------------------------Auxiliar para Funcionalidade 7 e 8---------------------------------------

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

% Conta o número de encomendas não entregues, num intervalo de tempo, e o número de encomendas nunca entregues
% ContaNaoEntregasPeriodo - nº de encomendas que foram entregues antes ou depois daquele período de tempo
% ContaNuncaEntregues - nº de encomendas que nunca chegaram a ser entregues

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

%--------------------------------------Auxiliares para Funcionalidade Extra2--------------------------------------

% Devolve o maior rácio entre encomendas não entregues/entregues com atraso e encomendas entregues existente entre todos os estafetas
racioEstafetasAux([IdEstaf],L,Ratio) :- racioEstafeta(IdEstaf,L,Ratio).
racioEstafetasAux([IdEstaf | T], L, MaxRatio) :-
    racioEstafeta(IdEstaf,L,Ratio),
    racioEstafetasAux(T,L,Ratio1),
    (Ratio > Ratio1 -> MaxRatio = Ratio; MaxRatio = Ratio1).

% Devolve o rácio entre encomendas não entregues/entregues com atraso e encomendas entregues de um estafeta
racioEstafeta(IdEstaf,L,Ratio) :-
    contaEncomendasEstafetaLista(IdEstaf,L,C),
    encomendasDoEstafeta(IdEstaf,E),
    comprimento(E,T),
    Ratio is C / T.

% Devolve a lista de estafetas com maior rácio entre encomendas não entregues/entregues com atraso e encomendas entregues 
estafetasMaiorRacio(Ratio,R,L) :- 
    solucoes(IdEstaf,racioEstafeta(IdEstaf,R,Ratio),S),
    sort(S,L).

% Devolve o número de encomendas feitas por um estafeta dada a lista de encomendas não entregues e entregues com atraso
contaEncomendasEstafetaLista(IdEstaf,[],0).
contaEncomendasEstafetaLista(IdEstaf,[IdEnc],1) :-
    encomendasDoEstafeta(IdEstaf,R),
    membro(IdEnc,R).
contaEncomendasEstafetaLista(IdEstaf,[IdEnc],0) :-
    encomendasDoEstafeta(IdEstaf,R),
    nao(membro(IdEnc,R)).
contaEncomendasEstafetaLista(IdEstaf,[IdEnc | T], Contador) :-
    encomendasDoEstafeta(IdEstaf,R),
    membro(IdEnc,R),
    contaEncomendasEstafetaLista(IdEstaf,T,Contador1),
    Contador is Contador1+1.
contaEncomendasEstafetaLista(IdEstaf,[IdEnc | T], Contador) :-
    encomendasDoEstafeta(IdEstaf,R),
    nao(membro(IdEnc,R)),
    contaEncomendasEstafetaLista(IdEstaf,T,Contador).

% Devolve uma lista com as encomendas não entregues e entregues com atraso
encomendasNaoEntreguesEAtrasadas(L) :- 
    encomendasEntreguesAtraso(A),
    encomendasNaoEntregues(NE),
    concatena(A,NE,L).

% Devolve uma lista com todas as encomendas entregues com atraso
encomendasEntreguesAtraso(L) :- solucoes(IdEnc,encEntregueAtraso(IdEnc),L).

% Determina se uma encomenda foi entregue com atraso
encEntregueAtraso(IdEnc) :- 
    encomenda(IdEnc,_,_,_,Prazo,DataI,DataF),
    getPrazoEncomendaHoras(Prazo,Horas),
    diferencaDatas(DataI,DataF,DifH),
    DifH > Horas.

% Devolve uma lista com todas as encomendas não entregues
encomendasNaoEntregues(L) :- solucoes(IdEnc, encNaoEntregue(IdEnc),L).

% Determina se uma determinada encomenda não foi entregue
encNaoEntregue(IdEnc) :- encomenda(IdEnc,_,_,_,_,_,data(0,0,0,0,0)).

%---------------------------------------------------Anexos---------------------------------------------------

% Adiciona um elemento a uma lista caso este ainda não pertença
adiciona( X,[],[X] ).
adiciona( X,L,[X|L] ) :- nao( membro(X,L) ).
adiciona( X,L,L ) :- membro( X,L ).

% Concatena duas listas sem elementos repetidos
concatena(L1,L2,CL):- concatenaAux(L1,L2,L0), append(L0,L2,AL), sort(AL,CL).

concatenaAux([],_,[]).
concatenaAux([H|T],L2,[H|R]):- nao(membro(H,L2)), concatenaAux(T,L2,R).
concatenaAux([H|T],L2,R):- membro(H,L2), concatenaAux(T,L2,R).

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

% Torna um atom numa lista 
getListaAtom([N,S],P) :- atomic_list_concat([N,S],' ',P).

% Torna um atom separado por vírgulas numa lista
getAtomVirgulaLista(L,A) :- atomic_list_concat(L,,,A).

% Devolve o número contido num atom
numeroAtom(A,N) :- atom_number(A,N).

% Devolve uma lista de número a partir de uma lista de atom
listaNumAtom([],[]).
listaNumAtom([A],L) :- numeroAtom(A,N), adiciona(N,L1,L).
listaNumAtom([A|T],L) :- numeroAtom(A,N), listaNumAtom(T,L1), adiciona(N,L1,L). 

% Devolve a lista com os elementos comuns 
comuns(L1,L2,R) :- intersection(L1,L2,R).