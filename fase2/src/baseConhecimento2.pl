:- dynamic (encomenda/5).
:- dynamic (estafeta/2).
:- dynamic (grafo/1).
:- dynamic (aresta/3).
:- dynamic (goal/1).
:- dynamic (pontoEntrega/1).
:- dynamic (estima/2).

%---------------------------Encomenda------------------------------
% Encomenda: #IdEnc, IdEstafeta, Peso, Volume, Freguesia

encomenda(100000000001,2,10,20,'São Victor').
encomenda(123456789000,3,16,15,'Gualtar').
encomenda(300145999366,2,5,30,'São Victor').
encomenda(411188745632,4,30,25,'Maximinos').
encomenda(512200534686,4,1,25,'Maximinos').
encomenda(611111154895,5,50,40,'Dume').
encomenda(792555468332,1,4,10,'Gualtar').
encomenda(800000000234,6,25,52,'Gualtar').
encomenda(910928382779,4,8,10,'Maximinos').
encomenda(100910101098,2,4,17,'Gualtar').
encomenda(113364968333,4,18,67,'São Victor').
encomenda(121203928222,7,10,28,'Maximinos').
encomenda(136666733413,4,20,41,'Tadim').
encomenda(142019203922,6,7,21,'Gualtar').
encomenda(150393815151,5,30,77,'São Vicente'). 
encomenda(169998372344,3,50,87,'Esporões').
encomenda(172123212430,6,9,18,'Maximinos').
encomenda(154268745963,8,33,12,'Lamas').
encomenda(774951266458,8,10,13,'Figueiredo').
encomenda(122457456652,9,42,40,'Figueiredo').
encomenda(901230054687,10,4,5,'Priscos').
encomenda(758846125940,10,6,21,'Tebosa').
encomenda(214568744021,10,3,20,'Lamas').
encomenda(115542001235,11,11,35,'Esporões').
encomenda(125489652311,11,8,5,'Tadim').
encomenda(812485569441,12,46,52,'Cabreiros').
encomenda(122145875630,13,15,12,'Figueiredo').
encomenda(124562335601,13,20,54,'Maximinos').
encomenda(458751021235,13,8,10,'Tebosa').
encomenda(124501236889,13,3,4,'Gualtar').
encomenda(214501239987,14,26,9,'Ruilhe').
encomenda(723100045791,14,40,36,'São Victor').
encomenda(312450012994,15,26,81,'São Victor').
encomenda(124568790122,15,8,24,'Morreira').
encomenda(154200113287,15,32,17,'Lamas').
encomenda(890125440127,16,17,30,'Palmeira').
encomenda(125400133654,17,15,41,'Cabreiros').
encomenda(124578512366,17,50,95,'Palmeira').
encomenda(125004357899,10,16,24,'São Victor').

%-----------------------------Grafo--------------------------------
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
       aresta('São Victor','Gualtar',3.8)])).
/* 
aresta('Green Distribuition','Priscos',4.9).
aresta('Green Distribuition','Maximinos',4.5).
aresta('Green Distribuition','Figueiredo',2.7).
aresta('Priscos','Tebosa',2.5).
aresta('Priscos','Ruilhe',2.6).
aresta('Priscos','Tadim',3.2).
aresta('Tebosa','Ruilhe',2.2).
aresta('Ruilhe','Tadim',2.1).
aresta('Tadim','Cabreiros',6.4).
aresta('Figueiredo','Lamas',1.3).
aresta('Figueiredo','Esporões',3.4).
aresta('Lamas','Esporões',2.8).
aresta('Lamas','Morreira',2.9).
aresta('Esporões','Morreira',2.8).
aresta('Maximinos','Dume',3.7).
aresta('Maximinos','São Vicente',3.9).
aresta('Dume','São Vicente',4.3).
aresta('Dume','Palmeira',4.8).
aresta('São Vicente','São Victor',2.15).
aresta('São Victor','Gualtar',3.8).
*/

% Goal: #Estado objetivo
goal('Green Distribuition').

% PontoEntrega: #Destino da encomenda
pontoEntrega('Tadim').
pontoEntrega('Esporões').
pontoEntrega('Maximinos').
pontoEntrega('Dume').
pontoEntrega('São Vicente').
pontoEntrega('São Victor').
pontoEntrega('Gualtar').
pontoEntrega('Priscos').
pontoEntrega('Cabreiros').
pontoEntrega('Figueiredo').
pontoEntrega('Lamas').
pontoEntrega('Morreira').
pontoEntrega('Palmeira').
pontoEntrega('Tebosa').
pontoEntrega('Ruilhe').

% Estimativas
estima('Green Distribuition',0).
estima('Figueiredo',2.7).
estima('Priscos',4.5).
estima('Maximinos', 4.5).
estima('Dume',7.5).
estima('Palmeira',9).
estima('São Vicente',6.5).
estima('São Victor', 8).
estima('Gualtar',4).
estima('Tebosa',6.7).
estima('Ruilhe',7).
estima('Tadim',6.2).
estima('Cabreiros',11).
estima('Lamas',2).
estima('Morreira',5.7).
estima('Esporões',5).