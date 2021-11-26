%Invariantes

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).

:- style_check(-singleton).

:-op(900,xfy,'::').

%-----------------------------------------------------------------------------------------------------------------------------------------

% Invariante estrutural: não permitir a entrada de conhecimento contraditório
+Termo :: (
    nao(-Termo)
).

+(-Termo) :: (
    nao(Termo)
).

% # Invariantes sobre encomendas 

% Invariante estrutural: os campos da Encomenda devem respeitar este tipo de dados
+encomenda(IdEnc,IdCliente,Peso,Volume,Prazo,DataInicio,DataFim) :: (
    integer(IdEnc),
    integer(IdCliente),
    integer(Peso),
    integer(Volume),
    atom(Prazo),
    dataTimeValida(DataInicio)
).

% Invariante estrutural: não permitir a entrada de uma encomenda cujo ID já exista
+encomenda(IdEnc,_,_,_,_,_,_) :: (
    solucoes(IdEnc,encomenda(IdEnc,_,_,_,_,_,_),L),
    comprimento(L,1)
).

% Invariante referencial: não permitir a remoção de uma encomenda que esteja associada a um estafeta
-encomenda(IdEnc,_,_,_,_,_,_) :: (
    listaTodasEncomendas(L),
    nao(membro(IdEnc,L))
).


% # Invariantes sobre clientes

% Invariante estrutural: o ID de um cliente deve ser um inteiro
+cliente(IdCliente) :: (
    integer(IdCliente)
).

% Invariante estrutural: não permitir a entrada de um cliente cujo ID já exista
+cliente(IdCliente) :: (
    solucoes(IdCliente,cliente(IdCliente),L),
    comprimento(L,1)
).

% Invariante referencial: não permitir a remoção de um cliente que esteja associado a uma encomenda
-cliente(IdCliente) :: (
    solucoes(IdCliente,encomenda(_,IdCliente,_,_,_,_,_,_),L),
    comprimento(L,0)
).


% # Invariantes sobre estafetas

% Invariante estrutural: os campos do Estafeta devem respeitar este tipo de dados
+estafeta(IdEstaf,Lista) :: (
    integer(IdEstaf),
    verificaDadosLista(Lista)
).

% Invariante estrutural: não permitir a entrada de um estafeta cujo ID já exista
+estafeta(IdEstaf,_) :: (
    solucoes(IdEstaf,estafeta(IdEstaf,_),L),
    comprimento(L,1)
).

% Invariante estrutural: não permitir a entrada de um estafeta que está associado a mais do que uma freguesia
+estafeta(IdEstaf,Lista) :: (
    listaFreguesiasEstaf(IdEstaf,Lista,L),
    comprimento(L,1)
).

% Invariante estrutural: não permitir a entrada de um estafeta cujo meio de transporte não suporte o peso da encomenda e a velocidade 
+estafeta(IdEstaf,Lista) :: (
    transportePesoVelocidade(Lista)
).

% Invariante referencial: não permitir a remoção de um estafeta que tenha feito entregas
-estafeta(IdEstaf,Lista) :: (
    estafeta(IdEstaf,[])
).