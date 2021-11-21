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

{-- validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False 
validaPotencialMapa ((p,c):t) 
    |contador (Porta,_) ((p,c):t) == 1 -}


contador :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Int
contador (p1,(x1,y1)) [] = 0
contador (p1,(x1,y1)) ((p2,(x,y)):t) 
    |p1 == p2 = 1 + contador (p1,(x1,y1)) t
    |otherwise = contador (p1,(x1,y1)) t


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

{--chao :: [(Peca,Coordenadas)] -> Bool
chao ((p,(x,y)):t) -}
    
