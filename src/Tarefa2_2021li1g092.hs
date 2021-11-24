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
constroiMapa [] = []
constroiMapa (p,(x,y):t) =


--fazmapa , alturta l = n linhas

line :: Coordenadas -> [(Peca, Coordenadas)] -> [(Peca)]
line (a,b) (p,(x,y):t) 
    |(a,b) elem (p,(x,y):t) = p : line (a,b+1) t
    |otherwise = Vazio : line (a,b+1)


--ver uma linha uma a uma e fazer a line , depois dissojuntar todas as lines (cosntroimapa)


desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = 
