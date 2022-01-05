{- |
Module      : Tarefa1_2021li1g092
Description : Validação de um potencial mapa
Copyright   : Diogo José Correia Ribeiro <a100755@alunos.uminho.pt>;
            : José Afonso Lopes Correia <a100610@alunos.uminho.pt>;

Módu\lo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g092 where

import LI12122 
import Funcoesuteis

{- | __Função principal que vai verificar se uma lista de peças e respetivas coordenadas podem construir um mapa válido.__  

===Exemplo:

>>> validaPotencialMapa [(Porta,(0,3)),(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4)),(Caixa,(4,3)),(Bloco,(5,4)),(Bloco,(6,4)),(Bloco,(6,3)),(Bloco,(6,2)),(Bloco,(6,1))]
True
>>> validaPotencialMapa []
False
-}
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False 
validaPotencialMapa l@((p,(x,y)):t) 
    |norepeat l == True && contador Porta l == 1 && emptyspace l == True = caixaNvoa l && chao l
    |otherwise = False

-- |Verifica se existem peças repetidas.
norepeat :: [(Peca, Coordenadas)] -> Bool
norepeat ((p,(x,y)):t) = if [pecaigual a ((p,(x,y)):t) | a <-((p,(x,y)):t)] == [setOne a ((p,(x,y)):t) | a <-((p,(x,y)):t)] then True else False

-- |Verifica as vezes que existe uma peca numa lista.
pecaigual :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> Int
pecaigual _ [] = 0
pecaigual (p,(x,y)) ((p1,(x1,y1)):t) 
    |(x == x1 && y==y1) = 1 + pecaigual (p,(x,y)) t
    |otherwise = pecaigual (p,(x,y)) t

setOne :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> Int
setOne (p,(x,y)) ((p1,(x1,y1)):t) = 1 

-- |Conta as vezes que um tipo determinado de peça ocorre numa lista de peças.
contador :: Peca -> [(Peca,Coordenadas)] -> Int
contador p [] = 0
contador p ((p1,(x1,y1)):t) 
    |p == p1 = 1 + contador p t
    |otherwise = contador p t

-- |Função que verifica que não existem caixasa flutuar.
caixaNvoa :: [(Peca,Coordenadas)] -> Bool
caixaNvoa [(p,(x,y))] = False
caixaNvoa ((p,(x,y)) : t) = caixaNflutua ( box ((p,(x,y)) : t) )  ((p,(x,y)) : t)

-- |Função auxiliar que recebe uma caixa e verifica se a caixa tem outra caixa ou um bloco na posição abaixo.
caixaNflutua :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool
caixaNflutua (p1,(x1,y1)) [] = False
caixaNflutua (p1,(x1,y1)) ((p2,(x2,y2)):t) 
    |p1==Caixa && ((p2 == Caixa) || (p2 == Bloco)) = if x1==x2 && y2 == (y1+1) then True else caixaNflutua (p1,(x1,y1)) t
    |otherwise = caixaNflutua (p1,(x1,y1)) t

-- |Função que verifica se existe pelo menos um espaço vazio no mapa.
emptyspace :: [(Peca,Coordenadas)] -> Bool
emptyspace [] = False
emptyspace l = nVazios l >= 1

-- |Função que determina o número de espaços Vazios.
nVazios :: [(Peca, Coordenadas)] -> Int
nVazios [] = 0
nVazios l = area l - nPecas l

-- |Função que conta o número de peças da lista recebida. 
nPecas :: [(Peca,Coordenadas)] -> Int
nPecas [] = 0
nPecas ((p,(x,y)):t) = 1 + nPecas t

-- |Função da área.
area :: [(Peca,Coordenadas)] -> Int
area [] = 0
area l = area' (ultpeca l)

-- |Auxiliar de area, temos de contar a linha 0 e coluna 0 do mapa, daí ser (x+1)*(y+1).
area' :: Coordenadas -> Int
area' (x,y) = (x+1)*(y+1)

-- |Função que calcula qual é a peça que está mais afastada da origem.
ultpeca :: [(Peca,Coordenadas)] -> Coordenadas
ultpeca [(p,(x,y))] = (x,y)
ultpeca ((p,(x,y)):t) 
    |x == comprimento ((p,(x,y)):t) && y == altura ((p,(x,y)):t) = (x,y)
    |otherwise = ultpeca t

-- |Função que verifica se existe um seguimento de blocos que constroiem um chão.
chao :: [(Peca,Coordenadas)] -> Bool --funciona apenas para o chao seguido
chao m = mylength (lineofblocks (ultimobloco m)  m) == (comprimento m + 1)

-- |Função auxiliar principal de chao que cria uma lista com os blocos que possuem a mesma altura que o ultimo bloco da coluna 0.
lineofblocks :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
lineofblocks (p,(x,y)) [] = []
lineofblocks (p,(x,y)) ((p1,(x1,y1)):t)
    |y == y1 = (p1,(x1,y1)) : lineofblocks  (p,(x,y)) t
    |otherwise = lineofblocks (p,(x,y)) t

-- |Função que nos indica qual a peça e respetivas coordenadas que está mais abaixo numa coluna.
ultimobloco :: [(Peca,Coordenadas)] -> (Peca,Coordenadas)
ultimobloco ((p,(x,y)):t) = lowerblock (coluna 0 ((p,(x,y)):t))