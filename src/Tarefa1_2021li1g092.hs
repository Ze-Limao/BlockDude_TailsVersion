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
    |otherwise= contador (p1,(x1,y1))t

caixaNflutua :: (Peca,Coordenadas) -> (Peca,Coordenadas) -> Bool
caixaNflutua (p1,c1) (p2,c2) = p1 == Caixa && (p2 == Caixa) || (p2 == Bloco)
caixaNflutua (p1,(x1,y1)) (p2,(x2,y2)) = x1==x2 && y2 == (y1+1) 


comprimento :: [(Peca, Coordenadas)] -> Int
comprimento [(a, (b, c))] = b
comprimento ((a, (b, c)) : (d, (e, f)) : xs)
  | b > e = comprimento ((a, (b, c)) : xs)
  | otherwise = comprimento ((d, (e, f)) : xs)

altura :: [(Peca, Coordenadas)] -> Int
altura [(a, (b, c))] = c
altura ((a, (b, c)) : (d, (e, f)) : xs)
  | c > f = altura ((a, (b, c)) : xs)
  | otherwise = altura ((d, (e, f)) : xs)

