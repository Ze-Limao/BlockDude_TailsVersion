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
validaPotencialMapa ((p,c):t) 
    |contador (Porta,_) ((p,c):t) == 1 


contador :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Int
contador (p1,(x1,y1)) [] = 0
contador (p1,(x1,y1)) ((p2,(x,y)):t) 
    |p1 == p2 = 1 + contador (p1,(x1,y1)) t
    |otherwise = contador (p1,(x1,y1)) t


nOcorre :: [(Peca,Coordenadas)] -> Bool
nOcorre ((p1,(x1,y1)) : t) (0,0) = 


nOcorre' :: Coordenadas -> [(Peca,Coordenadas)] -> Int


nOcorre'' :: Coordenadas -> [(Peca,Coordenadas)] -> Int
nOcorre'' (x,y) ((p1,(x1,y1)) : t) 
    |x==x1 && y==y1 = 1 + nOcorre' (x,y) t
    |otherwise = nOcorre' (x,y) t

caixaNvoa :: [(Peca,Coordenadas)] -> Bool
caixaNvoa [] = False
caixaNvoa ((p1,(x1,y1)) : t)
    |p1 == Caixa = caixaNflutua (p1,(x1,y1)) t 
    |otherwise = caixaNflutua (head t) (tail t)

caixaNflutua :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool
caixaNflutua (p1,(x1,y1)) [] = False
caixaNflutua (p1,(x1,y1)) ((p2,(x2,y2)):t) 
    |p1==Caixa && ((p2 == Caixa) || (p2 == Bloco)) = if x1==x2 && y2 == (y1+1) then True else caixaNflutua (p1,(x1,y1)) t
    |otherwise = caixaNflutua (p1,(x1,y1)) t


emptyspace :: [(Peca,Coordenadas)] -> Bool
emptyspace [] = False
emptyspace l = nVazios l >= 1

ultpeca :: [(Peca,Coordenadas)] -> Coordenadas
ultpeca [] = (0,0)
ultpeca [(p,(x,y))] = (x+1,y+1)
ultpeca ((p,(x,y)):t) 
    |x == comprimento [(p,(x,y))] && y == altura [(p,(x,y))] = (x,y)
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
chao ((p,(x,y)):t)


    