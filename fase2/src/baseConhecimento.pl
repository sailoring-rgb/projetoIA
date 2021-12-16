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
encomenda(154268745963,10,33,12,'7 horas',data(2021,12,14,9,00),data(2021,12,14,16,00)).
encomenda(774951266458,9,10,13,'10 horas',data(2021,10,10,8,30),data(2021,10,10,18,30)).
encomenda(122457456652,11,72,40,'1 dia',data(2021,9,20,12,45),data(2021,9,21,13,00)).
encomenda(001230054687,12,4,5,'5 horas',data(2021,6,1,9,00),data(2021,6,1,14,00)).
encomenda(758846125940,10,54,21,'13 horas',data(2021,4,10,21,00),data(2021,4,10,00)).
encomenda(214568744021,13,25,20,'2 horas',data(2021,3,14,13,00),data(2021,3,14,15,00)).
encomenda(115542001235,12,61,35,'4 horas',data(2021,9,21,11,00),data(2021,9,21,15,00)).
encomenda(125489652311,14,8,5,'Imediato',data(2021,11,3,10,00),data(2021,11,3,10,00)).
encomenda(012485569441,13,46,52,'6 horas',data(2021,2,16,14,00),data(2021,2,16,20,00)).
encomenda(122145875630,15,15,12,'1 hora',data(2021,8,7,17,00),data(2021,8,7,18,00)).
encomenda(124562335601,16,85,54,'1 dia',data(2021,7,23,15,00),data(2021,7,24,15,00)).
encomenda(458751021235,14,32,10,'2 horas',data(2021,10,12,12,00),data(2021,10,12,14,00)).
encomenda(124501236889,16,3,4,'5 horas',data(2021,2,7,17,00),data(2021,2,7,22,00)).
encomenda(214501239987,11,26,9,'12 horas',data(2021,5,21,9,00),data(2021,5,21,21,00)).
encomenda(023100045791,3,45,36,'Imediato',data(2021,12,6,16,00),data(2021,12,6,16,00)).
encomenda(012450012994,15,76,81,'4 horas',data(2021,1,15,10,00),data(2021,1,15,14,00)).
encomenda(124568790122,12,8,24,'16 horas',data(2021,4,3,22,00),data(2021,4,5,14,00)).
encomenda(154200113287,16,32,17,'3 horas',data(2021,2,25,17,00),data(2021,2,25,20,00)).
encomenda(890125440127,10,47,30,'9 horas',data(2021,3,26,9,00),data(2021,3,26,18,00)).
encomenda(125400133654,4,15,41,'7 horas',data(2021,7,14,16,00),data(2021,7,14,23,00)).
encomenda(124578512366,14,84,95,'10 horas',data(2021,6,11,9,00),data(2021,6,11,19,00)).
encomenda(125004357899,7,16,24,'8 horas',data(2021,7,13,11,00),data(2021,7,13,19,00)).

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
cliente(10).
cliente(11).
cliente(12).
cliente(13).
cliente(14).
cliente(15).
cliente(16).

%---------------------------Estafeta---------------------------
% Estafeta: #IdEstaf, [ (#IdEnc,Nota,Transporte,Freguesia) | T]

estafeta(1,[(792555468332,0,'Bicicleta','Gualtar')]).
estafeta(2,[(300145999366,0,'Bicicleta','São Victor'),(100910101098,4.2,'Bicicleta','Gualtar'),(100000000001,4.7,'Bicicleta','São Victor')]).
estafeta(3,[(123456789000,4.7,'Mota','Gualtar'),(169998372344,4.7,'Carro','Esporões')]).
estafeta(4,[(512200534686,3.9,'Bicicleta','Maximinos'),(910928382779,4.3,'Mota','Maximinos'),(113364968333,5.0,'Mota','São Victor'),(136666733413,5.0,'Mota','Tadim'),(411188745632,4.3,'Carro','Maximinos')]).
estafeta(5,[(611111154895,2.8,'Bicicleta','Dume'),(150393815151,4.8,'Carro','São Vicente')]).
estafeta(6,[(800000000234,4.9,'Carro','Gualtar'),(172123212430,4.9,'Carro','Maximinos'),(142019203922,0,'Bicicleta','Gualtar')]).
estafeta(7,[(121203928222,2.9,'Bicicleta','Maximinos')]).
estafeta(8,[(154268745963,4.5,'Carro','Lamas'),(774951266458,1.2,'Mota','Figueiredo')]).
estafeta(9,[(122457456652,2.3,'Carro','Figueiredo')]).
estafeta(10,[(001230054687,3.0,'Bicicleta','Priscos'),(758846125940,1.9,'Carro','Tebosa'),(214568744021,3.6,'Carro','Lamas')]).
estafeta(11,[(115542001235,2.5,'Carro','Esporões'),(125489652311,4.1,'Bicicleta','Tadim')]).
estafeta(12,[(012485569441,3.7,'Carro','Cabreiros')]).
estafeta(13,[(122145875630,4.0,'Mota','Figueiredo'),(124562335601,3.1,'Carro','Maximinos'),(458751021235,2.6,'Carro','Tebosa'),(124501236889,4.2,'Bicicleta','Figueiredo')]).
estafeta(14,[(214501239987,3.3,'Carro','Ruilhe'),(023100045791,2.8,'Carro','São Vicente')]).
estafeta(15,[(012450012994,3.4,'Carro','Priscos'),(124568790122,2.7,'Bicicleta','Morreira'),(154200113287,3.5,'Carro','Lamas')]).
estafeta(16,[890125440127,4.3,'Carro','Palmeira']).
estafeta(17,[(125400133654,3.8,'Mota','Cabreiros'),(124578512366,4.1,'Carro','Palmeira')]).
estafeta(18,[(125004357899,4.3,'Mota','Morreira')]).

%---------------------------Grafo------------------------------
% Termo-grafo: #grafo([b,c,d,f,g,h,k],[e(b,c),e(b,g),e(b,h), ...])
% Aresta: #Inicio, Fim, Distância

g(grafo(['Green Distribuition','Tadim','Esporões','Maximinos','Dume','São Vicente','São Victor','Gualtar','Figueiredo','Lamas',
          'Morreira','Palmeira','Tebosa','Priscos','Ruilhe','Cabreiros'],
  [aresta('Green Distribuition','Priscos',4.9),
   aresta('Green Distribuition','Maximinos',4.5),
   aresta('Green Distribuition','Figueiredo',2.7),
   aresta('Priscos','Tebosa',2.5),
   aresta('Priscos','Ruilhe',2.6),
   aresta('Priscos','Tadim',3.2),
   aresta('Tebosa','Ruilhe',2.2),
   aresta('Ruilhe','Tadim',2.1),
   aresta('Tadim','Cabreiros',6.4),
   aresta('Figueiredo','Lamas',1.3),
   aresta('Figueiredo','Esporões',3.4),
   aresta('Lamas','Esporões',2.8),
   aresta('Lamas','Morreira',2.9),
   aresta('Esporões','Morreira',2.8),
   aresta('Maximinos','Dume',3.7),
   aresta('Maximinos','São Vicente',3.9),
   aresta('Dume','São Vicente',4.3),
   aresta('Dume','Palmeira',4.8),
   aresta('São Vicente','São Victor',2.15),
   aresta('São Victor','Gualtar',3.8)]
 )).

% Origem: #Estado inicial
origem('Green Distribuition').

% Goal: #Estado objetivo
goal('Green Distribuition').

% PontoEntrega: #Destino da encomenda
%pontoEntrega('Tadim').
%pontoEntrega('Esporões').
%pontoEntrega('Maximinos').
%pontoEntrega('Dume').
%pontoEntrega('São Vicente').
%pontoEntrega('São Victor').
%pontoEntrega('Gualtar').
%pontoEntrega('Priscos').
%potnoEntrega('Cabreiros').
%pontoEntrega('Figueiredo').
%pontoentrega('Lamas').
%pontoEntrega('Morreira').
%pontoEntrega('Palmeira').
%pontoEntrega('Tebosa').
%pontoEntrega('Ruilhe').
