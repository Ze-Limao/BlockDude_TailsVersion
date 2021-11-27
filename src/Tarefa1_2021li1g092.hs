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

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False 
validaPotencialMapa ((p,(x,y)):t) 
    |contador Porta ((p,(x,y)):t) == 1 && emptyspace ((p,(x,y)):t) == True = caixaNvoa ((p,(x,y)):t) && chao ((p,(x,y)):t)
    |otherwise = False


contador :: Peca -> [(Peca,Coordenadas)] -> Int
contador p [] = 0
contador p ((p1,(x1,y1)):t) 
    |p == p1 = 1 + contador p t
    |otherwise = contador p t


caixaNvoa :: [(Peca,Coordenadas)] -> Bool
caixaNvoa [(p,(x,y))] = False
caixaNvoa ((p,(x,y)) : t) = caixaNflutua ( box ((p,(x,y)) : t) )  ((p,(x,y)) : t)


caixaNflutua :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool
caixaNflutua (p1,(x1,y1)) [] = False
caixaNflutua (p1,(x1,y1)) ((p2,(x2,y2)):t) 
    |p1==Caixa && ((p2 == Caixa) || (p2 == Bloco)) = if x1==x2 && y2 == (y1+1) then True else caixaNflutua (p1,(x1,y1)) t
    |otherwise = caixaNflutua (p1,(x1,y1)) t


emptyspace :: [(Peca,Coordenadas)] -> Bool
emptyspace [] = False
emptyspace l = nVazios l >= 1

ultpeca :: [(Peca,Coordenadas)] -> Coordenadas
ultpeca [(p,(x,y))] = (x+1,y+1)
ultpeca ((p,(x,y)):t) 
    |x == comprimento ((p,(x,y)):t) && y == altura ((p,(x,y)):t) = (x,y)
    |otherwise = ultpeca t

area :: [(Peca,Coordenadas)] -> Int
area [] = 0
area l = area' (ultpeca l)

area' :: Coordenadas -> Int
area' (x,y) = x*y

nPecas :: [(Peca,Coordenadas)] -> Int
nPecas [] = 0
nPecas ((p,(x,y)):t) = 1 + nPecas t

nVazios :: [(Peca, Coordenadas)] -> Int
nVazios [] = 0
nVazios l = area l - nPecas l


chao :: [(Peca,Coordenadas)] -> Bool --funciona apenas para o chao seguido
chao m = mylength (lineofblocks (ultimobloco m)  m) == (comprimento m + 1)

lineofblocks :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
lineofblocks (p,(x,y)) [] = []
lineofblocks (p,(x,y)) ((p1,(x1,y1)):t)
    |y == y1 = (p1,(x1,y1)) : lineofblocks  (p,(x,y)) t
    |otherwise = lineofblocks (p,(x,y)) t

ultimobloco :: [(Peca,Coordenadas)] -> (Peca,Coordenadas)
ultimobloco ((p,(x,y)):t) = lowerblock (coluna 0 ((p,(x,y)):t))