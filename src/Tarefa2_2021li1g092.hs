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

--constroiMapa :: [(Peca, Coordenadas)] -> Mapa


--ver uma linha uma a uma e fazer a line , depois disso ,juntar todas as lines (cosntroimapa)

line :: [(Peca, Coordenadas)] -> [(Peca)]
line ((p,(x,y)):t) = line' (0,0) ((p,(x,y)):t)  

line' :: Coordenadas -> [(Peca, Coordenadas)] -> [(Peca)]
line' (a,b) ((p,(x,y)):t) 
    |a == x = p : line' ((a+1),b) t
    |a /= x = Vazio : line' ((a+1),b) ((p,(x,y)):t)
    |otherwise = []



desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = desconstroiMapa' (0,0) mapa  

desconstroiMapa' :: Coordenadas -> Mapa -> [(Peca, Coordenadas)]
desconstroiMapa' (x,y) [[]]  = []
desconstroiMapa' (x,y) ([]:xs)  = desconstroiMapa' (0,y+1) xs 
desconstroiMapa' (x,y) ((h:t) : xs)   
    |h == Vazio = desconstroiMapa' (x+1,y) (t:xs) 
    |otherwise = (h,(x,y)) : desconstroiMapa'  (x+1,y) (t:xs)
