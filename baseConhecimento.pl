%---------------------------Encomenda---------------------------
% Encomenda: #IdEnc, #IdCliente, Peso, Volume, Prazo, Transporte

encomenda(1,1,30,20,'6h','Carro').
encomenda(2,4,2,15,'Imediato','Bicicleta').
encomenda(3,3,5,30,'2h','Bicicleta').
encomenda(4,3,10,25,'3h','Mota').
encomenda(5,4,1,25,'6h','Bicicleta').
encomenda(6,2,70,40,'1 dia','Carro').

%----------------------------Cliente----------------------------
% Cliente: #IdCliente, TempoMax, Nota 

cliente(1).
cliente(2).
cliente(3).
cliente(4).
cliente(5).

%---------------------------Estafeta---------------------------
% Estafeta: #IdEstf, [ (#IdEnc,Nota,Rua,Freguesia) | T]

estafeta(1,[(1,3.2,'Rua da Boavista','Real')]).
estafeta(2,[(1,3.5,'Av. da Liberdade','São Lázaro'),(2,4.6,'Rua do Raio','São Lázaro')]).
estafeta(3,[(2,4.7,'Rua José Antunes Guimarães','Gualtar'),(3,4.9,'Rua da Universidade','Gualtar')]).
estafeta(4,[(5,3.9,'Av. Conde Dom Henriques','Maximinos')]).
estafeta(5,[(6,3.9,'Rua da Veiga','Dume')]).