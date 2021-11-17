%----------------Encomenda---------------
% Encomenda: #IdEnc, #IdCliente, Peso, Volume, Prazo, Transporte


encomenda(1,1,30,20,'6h','Carro').
encomenda(3,2,5,30,'2h','Bicleta').
encomenda(4,3,10,'3h','Mota').

%---------------Cliente------------------
% Cliente: #IdCliente, TempoMax, Nota 

cliente(1,'6h',4).
cliente(2,'Imediato',3.5).
cliente(3,'1 dia', 5).

%--------------Estafeta------------------
% Estafeta: #IdEstf, #IdEnc, Rua, Freguesia

estafeta(1,1,'Rua da Boavista','Real').
estafeta(2,1,'Rua da Boavista','Real').
estafeta(1,2,'Rua da Veiga','Dume').
estafeta(2,3,'Rua da Universidade','Gualtar').

