%---------------------------Encomenda---------------------------
% Encomenda: #IdEnc, #IdCliente, Peso, Volume, Prazo, DataInicio, DataFim, Transporte

encomenda(1,1,30,20,'6h',data(2021,7,25,10,0),data(2021,7,25,15,30),'Carro').
encomenda(2,4,2,15,'Imediato', data(2021,8,1,12,30), data(2021,8,1,12,35), 'Bicicleta').
encomenda(3,3,5,30,'2h', data(2021,4,10,17,20), data(0,0,0,0,0), 'Bicicleta'). % não entregue
encomenda(4,3,10,25,'3h',data(2021,5,29,13,10), data(2021,5,29,16,0),'Mota').
encomenda(5,4,1,25,'6h', data(2021,6,30,9,0), data(2021,6,30,16,45), 'Bicicleta'). % entregue com atraso 
encomenda(6,2,70,40,'1 dia', data(2021,4,16,15,40), data(2021,4,17,15,0),'Carro').

%----------------------------Cliente----------------------------
% Cliente: #IdCliente

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