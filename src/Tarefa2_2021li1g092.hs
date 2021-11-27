{- |
Module      : Tarefa2_2021li1g092
Description : Construção/Desconstrução do mapa
Copyright   : Diogo José Correia Ribeiro <a100755@alunos.uminho.pt>;
            : José Afonso Lopes Correia <a100610@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g092 where

import LI12122
import Funcoesuteis

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa pecas = undefined


desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = desconstroiMapa' (0,0) mapa  

desconstroiMapa' :: Coordenadas -> Mapa -> [(Peca, Coordenadas)]
desconstroiMapa' (x,y) [[]]  = []
desconstroiMapa' (x,y) ([]:xs)  = desconstroiMapa' (0,y+1) xs 
desconstroiMapa' (x,y) ((h:t) : xs)   
    |h == Vazio = desconstroiMapa' (x+1,y) (t:xs) 
    |otherwise = (h,(x,y)) : desconstroiMapa'  (x+1,y) (t:xs)



