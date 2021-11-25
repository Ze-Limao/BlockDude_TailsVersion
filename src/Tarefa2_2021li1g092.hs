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



line :: [(Peca, Coordenadas)] -> [(Peca)]
line ((p,(x,y)):t) = line' (0,0) ((p,(x,y)):t)  

line' :: Coordenadas -> [(Peca, Coordenadas)] -> [(Peca)]
line' (a,b) ((p,(x,y)):t) 
    |a == x = p : line' ((a+1),b) t
    |a /= x = Vazio : line' ((a+1),b) ((p,(x,y)):t)
    |otherwise = []


--ver uma linha uma a uma e fazer a line , depois disso ,juntar todas as lines (cosntroimapa)


desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = descontroiMapa' (0,0) mapa


desconstroiMapa' :: Coordenadas -> Mapa -> [(Peca, Coordenadas)]

desconstroiMapa' (x,y) ((h:t) : xs)
    |h==Vazio =  desconstroiMapa' ((x+1),y) 
    |h==Bloco = (Bloco,(x,y)) : desconstroiMapa' ((x+1),y)
    |h==Caixa = (Caixa,(x,y)) : desconstroiMapa' ((x+1),y)
    |h==Porta = (Porta,(x,y)) desconstroiMapa' ((x+1),y)
