%---------------------------Encomenda---------------------------
% Encomenda: #IdEnc, #IdCliente, Peso, Volume, Prazo, DataInicio, DataFim
% Data: #Ano, Mês, Dia, Hora, Minuto

encomenda(1,1,30,20,'6h',data(2021,7,25,10,0),data(2021,7,25,15,30)).
encomenda(2,4,2,15,'Imediato', data(2021,8,1,12,30), data(2021,8,1,12,35)).
encomenda(3,3,5,30,'2h', data(2021,4,10,17,20), data(0,0,0,0,0)). % não entregue
encomenda(4,3,10,25,'3h',data(2021,5,29,13,10), data(2021,5,29,16,0)).
encomenda(5,4,1,25,'6h', data(2021,6,30,9,0), data(2021,6,30,16,45)). % entregue com atraso 
encomenda(6,2,70,40,'1 dia', data(2021,4,16,15,40), data(2021,4,17,15,0)).
encomenda(7,2,4,10,'3h', data(2021,2,28,16,12), data(2021,2,28,18,4)).

%----------------------------Cliente----------------------------
% Cliente: #IdCliente

cliente(1).
cliente(2).
cliente(3).
cliente(4).
cliente(5).

%---------------------------Estafeta---------------------------
% Estafeta: #IdEstaf, [ (#IdEnc,Nota,Velocidade,Transporte,Rua,Freguesia) | T]

estafeta(1,[(1,3.2,25,'Carro','Rua da Boavista','Real')]).
estafeta(2,[(1,3.5,25,'Carro','Av. da Liberdade','São Lázaro'),(2,4.6,10,'Bicicleta','Rua do Raio','São Lázaro'),(4,2.7,25,'Carro','Rua do Poço','São Lázaro')]).
estafeta(3,[(2,4.7,35,'Mota','Rua José Antunes Guimarães','Gualtar'),(3,4.9,10,'Bicicleta','Rua da Universidade','Gualtar')]).
estafeta(4,[(5,3.9,10,'Bicicleta','Av. Conde Dom Henriques','Maximinos'),(7,4.3,10,'Bicicleta','Av. São Pedro de Maximinos','Maximinos')]).
estafeta(5,[(6,3.9,25,'Carro','Rua da Veiga','Dume')]).