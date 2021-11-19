%---------------------------Encomenda---------------------------
% Encomenda: #IdEnc, #IdCliente, Peso, Volume, Prazo, Transporte

encomenda(1,1,30,20,'6h','Carro').
encomenda(2,4,2,15,'Imediato','Bicicleta').
encomenda(3,3,5,30,'2h','Bicicleta').
encomenda(4,3,10,25,'3h','Mota').
encomenda(5,4,1,25,'6h','Bicicleta').

%----------------------------Cliente----------------------------
% Cliente: #IdCliente, TempoMax, Nota 

cliente(1).
cliente(2).
cliente(3).
cliente(4).

%---------------------------Estafeta---------------------------
% Estafeta: #IdEstf, [ (#IdEnc,Nota,Rua,Freguesia) | T]

estafeta(1,[ (1,3.2,'Rua da Boavista','Real'), (5,3.9,'Av. Conde Dom Henriques','Maximinos') ].
estafeta(2,[ (1,3.5,'Av. da Liberdade','São Lázaro'),(2,4.6,'Av. Robert Smith','Lamaçães') ].
estafeta(3,[ (2,4.7,'Rua da Veiga','Dume'),(3,4.9,'Rua da Universidade','Gualtar') ].