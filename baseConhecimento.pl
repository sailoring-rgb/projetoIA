%---------------------------Encomenda---------------------------
% Encomenda: #IdEnc, #IdCliente, Peso, Volume, Prazo, Transporte

encomenda(1,1,30,20,'6h','Carro').
encomenda(2,4,2,15,'Imediato','Bicicleta').
encomenda(3,2,5,30,'2h','Bicicleta').
encomenda(4,3,10,25,'3h','Mota').

%----------------------------Cliente----------------------------
% Cliente: #IdCliente, TempoMax, Nota 

cliente(1,'6h',4).
cliente(2,'2h',3.5).
cliente(3,'3h', 5).
cliente(3,'Imediato', 2).

%---------------------------Estafeta---------------------------
% Estafeta: #IdEstf, #IdEnc, Rua, Freguesia

estafeta(1,1,'Rua da Boavista','Real').
estafeta(2,1,'Rua da Boavista','Real').
estafeta(2,2,'Av. Robert Smith','Lamaçães').
estafeta(3,2,'Rua da Veiga','Dume').
estafeta(3,3,'Rua da Universidade','Gualtar').
