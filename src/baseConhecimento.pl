:- dynamic (encomenda/7).
:- dynamic (cliente/1).
:- dynamic (estafeta/2).

%---------------------------Encomenda---------------------------
% Encomenda: #IdEnc, #IdCliente, Peso, Volume, Prazo, DataInicio, DataFim
% Data: #Ano, Mês, Dia, Hora, Minuto
% IdEnc: #12 dígitos

encomenda(100000000001,1,10,20,'6h',data(2021,7,25,10,0),data(2021,7,25,15,30)).
encomenda(123456789000,4,16,15,'Imediato',data(2021,8,1,12,30),data(2021,8,1,12,35)).
encomenda(300145999366,3,5,30,'2h',data(2021,4,10,17,20),data(0,0,0,0,0)).                   % não entregue
encomenda(411188745632,3,30,25,'3h',data(2021,5,29,13,10),data(2021,5,29,16,0)).
encomenda(512200534686,4,1,25,'6h',data(2021,6,30,9,0),data(2021,6,30,16,45)).               % entregue com atraso 
encomenda(611111154895,2,70,40,'1 dia',data(2021,4,16,15,40),data(2021,4,18,15,0)).          % entregue com atraso
encomenda(792555468332,2,4,10,'3h',data(2021,2,28,16,12),data(0,0,0,0,0)).                   % não entregue
encomenda(800000000234,5,97,52,'12h',data(2021,6,24,8,30),data(2021,6,24,20,0)).
encomenda(910928382779,4,8,10,'4h',data(2021,8,1,14,0),data(2021,8,1,17,10)).
encomenda(100910101098,6,4,17,'1h',data(2021,1,31,12,27),data(2021,1,31,12,45)).
encomenda(113364968333,2,18,67,'1 dia',data(2021,12,19,21,29),data(2021,12,20,20,23)).
encomenda(121203928222,7,10,28,'Imediato',data(2021,10,13,11,43),data(2021,10,14,8,33)).     % entregue com atraso
encomenda(136666733413,5,20,41,'6h',data(2021,3,28,12,12),data(2021,3,28,15,53)).
encomenda(142019203922,2,7,21,'3h',data(2021,6,12,18,49),data(0,0,0,0,0)).                   % não entregue
encomenda(150393815151,8,30,77,'12h',data(2021,1,5,13,59),data(2021,1,5,22,12)). 
encomenda(169998372344,4,87,87,'1 dia',data(2021,2,13,00,00),data(2021,2,14,13,00)).
encomenda(172123212430,9,9,18,'2h',data(2021,11,7,12,12),data(2021,11,7,13,45)).

%----------------------------Cliente----------------------------
% Cliente: #IdCliente

cliente(1).
cliente(2).
cliente(3).
cliente(4).
cliente(5).
cliente(6).
cliente(7).
cliente(8).
cliente(9).

%---------------------------Estafeta---------------------------
% Estafeta: #IdEstaf, [ (#IdEnc,Nota,Velocidade,Transporte,Rua,Freguesia) | T]

estafeta(1,[(792555468332,1.0,10,'Bicicleta','Rua da Arcela','Gualtar')]).
estafeta(2,[(300145999366,1.0,10,'Bicicleta','Rua de São Victor-O-Velho','São Victor'),(100910101098,4.2,10,'Bicicleta','Rua das Mimosas','Gualtar'),(100000000001,4.7,10,'Bicicleta','Parque da Rodovia','São Victor')]).
estafeta(3,[(123456789000,4.7,35,'Mota','Rua José Antunes Guimarães','Gualtar'),(169998372344,4.7,25,'Carro','Rua da Graciosa','Esporões')]).
estafeta(4,[(512200534686,3.9,10,'Bicicleta','Av. Conde Dom Henriques','Maximinos'),(910928382779,4.3,35,'Mota','Av. São Pedro de Maximinos','Maximinos'),(113364968333,5.0,35,'Mota','Largo Senhora-A-Branca','São Victor'),(136666733413,5.0,35,'Mota','Largo do Bairro','Tadim'),(411188745632,4.3,25,'Carro','Ponte dos Falcões','Maximinos')]).
estafeta(5,[(611111154895,2.8,10,'Bicicleta','Rua da Veiga','Dume'),(150393815151,4.8,25,'Carro','Rua de Santo André','São Vicente')]).
estafeta(6,[(800000000234,4.9,25,'Carro','Rua 15 de maio','Gualtar'),(172123212430,4.9,25,'Carro','Rua da Boavista','Maximinos'),(142019203922,1.0,10,'Bicicleta','Rua da Arcela','Gualtar')]).
estafeta(7,[(121203928222,2.9,10,'Bicicleta','Largo Carlos Amarante','Maximinos')]).