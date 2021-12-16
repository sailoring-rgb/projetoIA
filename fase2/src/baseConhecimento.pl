:- dynamic (encomenda/7).
:- dynamic (cliente/1).
:- dynamic (estafeta/2).

%---------------------------Encomenda---------------------------
% Encomenda: #IdEnc, #IdCliente, Peso, Volume, Prazo, DataInicio, DataFim
% Data: #Ano, Mês, Dia, Hora, Minuto
% IdEnc: #12 dígitos

encomenda(100000000001,1,10,20,'6 horas',data(2021,7,25,10,0),data(2021,7,25,15,30)).
encomenda(123456789000,4,16,15,'Imediato',data(2021,8,1,12,30),data(2021,8,1,12,35)).
encomenda(300145999366,3,5,30,'2 horas',data(2021,4,10,17,20),data(0,0,0,0,0)).                   % não entregue
encomenda(411188745632,3,30,25,'3 horas',data(2021,5,29,13,10),data(2021,5,29,16,0)).
encomenda(512200534686,4,1,25,'6 horas',data(2021,6,30,9,0),data(2021,6,30,16,45)).               % entregue com atraso 
encomenda(611111154895,2,70,40,'1 dia',data(2021,4,16,15,40),data(2021,4,18,15,0)).               % entregue com atraso
encomenda(792555468332,2,4,10,'3 horas',data(2021,2,28,16,12),data(0,0,0,0,0)).                   % não entregue
encomenda(800000000234,5,97,52,'12 horas',data(2021,6,24,8,30),data(2021,6,24,20,0)).
encomenda(910928382779,4,8,10,'4 horas',data(2021,8,1,14,0),data(2021,8,1,17,10)).
encomenda(100910101098,6,4,17,'1 horas',data(2021,1,31,12,27),data(2021,1,31,12,45)).
encomenda(113364968333,2,18,67,'1 dia',data(2021,12,19,21,29),data(2021,12,20,20,23)).
encomenda(121203928222,7,10,28,'Imediato',data(2021,10,13,11,43),data(2021,10,14,8,33)).          % entregue com atraso
encomenda(136666733413,5,20,41,'6 horas',data(2021,3,28,12,12),data(2021,3,28,15,53)).
encomenda(142019203922,2,7,21,'3 horas',data(2021,6,12,18,49),data(0,0,0,0,0)).                   % não entregue
encomenda(150393815151,8,30,77,'12 horas',data(2021,1,5,13,59),data(2021,1,5,22,12)). 
encomenda(169998372344,4,87,87,'1 dia',data(2021,2,13,00,00),data(2021,2,14,13,00)).
encomenda(172123212430,9,9,18,'2 horas',data(2021,11,7,12,12),data(2021,11,7,13,45)).

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
% Estafeta: #IdEstaf, [ (#IdEnc,Nota,Transporte,Freguesia) | T]

estafeta(1,[(792555468332,0,'Bicicleta','Gualtar')]).
estafeta(2,[(300145999366,0,'Bicicleta','São Victor'),(100910101098,4.2,'Bicicleta','Gualtar'),(100000000001,4.7,'Bicicleta','São Victor')]).
estafeta(3,[(123456789000,4.7,'Mota','Gualtar'),(169998372344,4.7,'Carro','Esporões')]).
estafeta(4,[(512200534686,3.9,'Bicicleta','Maximinos'),(910928382779,4.3,'Mota','Maximinos'),(113364968333,5.0,'Mota','São Victor'),(136666733413,5.0,'Mota','Tadim'),(411188745632,4.3,'Carro','Maximinos')]).
estafeta(5,[(611111154895,2.8,'Bicicleta','Dume'),(150393815151,4.8,'Carro','São Vicente')]).
estafeta(6,[(800000000234,4.9,'Carro','Gualtar'),(172123212430,4.9,'Carro','Maximinos'),(142019203922,0,'Bicicleta','Gualtar')]).
estafeta(7,[(121203928222,2.9,'Bicicleta','Maximinos')]).

%---------------------------Grafo------------------------------
% Termo-grafo: #grafo([b,c,d,f,g,h,k],[e(b,c),e(b,g),e(b,h), ...])
% Aresta: #Inicio, Fim, Distância

g(grafo(['Green Distribuition','Tadim','Esporões','Maximinos','Dume','São Vicente','São Victor','Gualtar'],
  [aresta('Green Distribuition','Tadim',4.5),
   aresta('Green Distribuition','Esporões',3.9),
   aresta('Green Distribuition','Maximinos',4.5),
   aresta('Maximinos','Dume',3.7),
   aresta('Maximinos','São Vicente',3.9),
   aresta('Dume','São Vicente',4.3),
   aresta('São Vicente','São Victor',2.15),
   aresta('São Victor','Gualtar',3.8)]
 )).

goal('Green Distribuition').
goal('Tadim').
goal('Esporões').
goal('Maximinos').
goal('Dume').
goal('São Vicente').
goal('São Victor').
goal('Gualtar').