{- |
Module      : Tarefa1_2021li1g092
Description : Validação de um potencial mapa
Copyright   : Diogo José Correia Ribeiro <a100755@alunos.uminho.pt>;
            : José Afonso Lopes Correia <a100610@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g092 where

import LI12122 ( Peca, Coordenadas )


contador :: a -> [a] -> Int
contador _ [] = 0
contador a (h:t) 
    |a==h = 1 + contador a t
    |otherwise= contador a t

caixaNflutua :: (Caixa,Coordenadas) -> (Bloco,Coordenadas) -> Bool
caixaNflutua (Caixa,c1) [] = False 
caixaNflutua (_,(x1,y1))(_,(x2,y2)) 
    |x1==x2 && y2 == (y1+1) = True 
    |otherwise = False

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False 
validaPotencialMapa ((p,c):t) 
    |contador (Porta,_) ((p,c):t) == 1 = True 
    |otherwise = False
    |(Caixa,_) =  caixaNflutua (p,c) 

